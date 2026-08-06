import { execFile } from "node:child_process";
import {
	CustomEditor,
	InteractiveMode,
	type ExtensionAPI,
	type ExtensionContext,
} from "@earendil-works/pi-coding-agent";
import { truncateToWidth, visibleWidth } from "@earendil-works/pi-tui";

type TextBlock = {
	type?: string;
	text?: string;
};

type SessionEntry = {
	type?: string;
	message?: {
		role?: string;
		content?: unknown;
	};
};

const MAX_CONVERSATION_CHARS = 12_000;

type SessionAttachments = {
	ticket?: string;
	ticketUrl?: string;
	pr?: string;
	prUrl?: string;
};

let currentContext: ExtensionContext | undefined;
let currentSessionName: string | undefined;
let currentAttachments: SessionAttachments = {};

const originalHandleNameCommandSymbol = Symbol.for("ben.pi.auto-name.originalHandleNameCommand");
const interactiveModePrototype = InteractiveMode.prototype as typeof InteractiveMode.prototype & {
	[originalHandleNameCommandSymbol]?: (text: string) => void;
	handleNameCommand: (text: string) => void;
};

interactiveModePrototype[originalHandleNameCommandSymbol] ??= interactiveModePrototype.handleNameCommand;
const originalHandleNameCommand = interactiveModePrototype[originalHandleNameCommandSymbol];

const originalEditorRenderSymbol = Symbol.for("ben.pi.auto-name.originalEditorRender");
const customEditorPrototype = CustomEditor.prototype as typeof CustomEditor.prototype & {
	[originalEditorRenderSymbol]?: (width: number) => string[];
	render: (width: number) => string[];
};

customEditorPrototype[originalEditorRenderSymbol] ??= customEditorPrototype.render;
const originalEditorRender = customEditorPrototype[originalEditorRenderSymbol];

const textParts = (content: unknown): string[] => {
	if (typeof content === "string") {
		return [content];
	}
	if (!Array.isArray(content)) {
		return [];
	}
	return content.flatMap((part) => {
		const block = part as TextBlock;
		return block?.type === "text" && typeof block.text === "string" ? [block.text] : [];
	});
};

const conversationText = (entries: SessionEntry[]): string => {
	const sections: string[] = [];

	for (const entry of entries) {
		if (entry.type !== "message") {
			continue;
		}

		const role = entry.message?.role;
		if (role !== "user" && role !== "assistant") {
			continue;
		}

		const text = textParts(entry.message?.content).join("\n").trim();
		if (text) {
			sections.push(`${role}: ${text}`);
		}
	}

	return sections.join("\n\n").slice(-MAX_CONVERSATION_CHARS);
};

const fallbackName = (conversation: string): string => {
	const firstUserLine = conversation
		.split("\n")
		.find((line) => line.startsWith("user: "))
		?.replace(/^user:\s*/, "")
		.trim();

	return cleanName(firstUserLine ?? "Untitled session");
};

const cleanName = (name: string): string => {
	const cleaned = name
		.replace(/^[-*\d.\s]+/, "")
		.replace(/^['\"]|['\"]$/g, "")
		.replace(/\s+/g, " ")
		.trim();

	return cleaned.length > 60 ? cleaned.slice(0, 57).trimEnd() + "..." : cleaned;
};

const hyperlink = (text: string, url: string): string => `\x1b]8;;${url}\x07${text}\x1b]8;;\x07`;

const sessionLabel = (name: string, attachments: SessionAttachments): string => {
	const sanitizedName = name.replace(/[\r\n\t]/g, " ").replace(/ +/g, " ").trim();
	const displayName = attachments.ticket
		? sanitizedName.replace(new RegExp(`^${attachments.ticket}\\s*[:|-]?\\s*`, "i"), "").trim() || sanitizedName
		: sanitizedName;
	const parts = [displayName, attachments.ticket, attachments.pr].filter(Boolean);
	return `[${parts.join("|")}]`;
};

const linkSessionLabel = (label: string, attachments: SessionAttachments): string => {
	let linkedLabel = label;
	if (attachments.ticket && attachments.ticketUrl) {
		linkedLabel = linkedLabel.replace(attachments.ticket, hyperlink(attachments.ticket, attachments.ticketUrl));
	}
	if (attachments.pr && attachments.prUrl) {
		linkedLabel = linkedLabel.replace(attachments.pr, hyperlink(attachments.pr, attachments.prUrl));
	}
	return linkedLabel;
};

const sessionNameBorder = (name: string, attachments: SessionAttachments, width: number, color: (text: string) => string): string => {
	const maxLabelWidth = Math.max(1, width - 4);
	const truncatedLabel = truncateToWidth(sessionLabel(name, attachments), maxLabelWidth, "…");
	const label = ` ${linkSessionLabel(truncatedLabel, attachments)} `;
	const remainingWidth = Math.max(0, width - 1 - visibleWidth(label));
	return color(`─${label}${"─".repeat(remainingWidth)}`);
};

const runCommand = (command: string, cwd: string, args: string[], timeout: number): Promise<string | undefined> => {
	return new Promise((resolve) => {
		execFile(command, args, { cwd, timeout }, (error, stdout) => {
			if (error) {
				resolve(undefined);
				return;
			}
			resolve(stdout.trim() || undefined);
		});
	});
};

const runGit = (cwd: string, args: string[]): Promise<string | undefined> => runCommand("git", cwd, args, 2_000);

const runGh = (cwd: string, args: string[]): Promise<string | undefined> => runCommand("gh", cwd, args, 4_000);

const ticketFromText = (text: string | undefined): string | undefined => {
	const match = text?.match(/\b([A-Z][A-Z0-9]+-\d+)\b/i);
	return match?.[1]?.toUpperCase();
};

const discoverAttachments = async (cwd: string, sessionName: string | undefined): Promise<SessionAttachments> => {
	const branch = await runGit(cwd, ["branch", "--show-current"]);
	const ticket = ticketFromText(sessionName) ?? ticketFromText(branch);
	const prDetails = await runGh(cwd, ["pr", "view", "--json", "number,url", "--jq", "[.number, .url] | @tsv"]);
	const [prNumber, prUrl] = prDetails?.split("\t") ?? [];
	return {
		ticket,
		ticketUrl: ticket ? `https://datadoghq.atlassian.net/browse/${ticket}` : undefined,
		pr: prNumber ? `PR #${prNumber}` : undefined,
		prUrl,
	};
};

const refreshAttachments = async (cwd: string) => {
	currentAttachments = await discoverAttachments(cwd, currentSessionName);
};

const generateName = async (ctx: ExtensionContext): Promise<string> => {
	const conversation = conversationText(ctx.sessionManager.getBranch());
	if (!conversation) {
		throw new Error("No conversation text found");
	}

	const model = ctx.model;
	const provider = model ? ctx.modelRegistry.getProvider(model.provider) : undefined;
	const auth = model ? await ctx.modelRegistry.getProviderAuth(model.provider) : undefined;
	if (!model || !provider || !auth) {
		return fallbackName(conversation);
	}

	const response = await provider
		.streamSimple(
			model,
			{
				messages: [
					{
						role: "user" as const,
						content: [
							{
								type: "text" as const,
								text: [
									"Generate a short session name for this coding-agent conversation.",
									"Return only the name, no quotes or punctuation.",
									"Use 2-6 words, title case or sentence case.",
									"",
									"<conversation>",
									conversation,
									"</conversation>",
								].join("\n"),
							},
						],
						timestamp: Date.now(),
					},
				],
			},
			{
				...auth.auth,
				env: auth.env,
				reasoningEffort: "minimal",
				signal: ctx.signal,
			},
		)
		.result();

	const name = response.content
		.filter((part): part is { type: "text"; text: string } => part.type === "text")
		.map((part) => part.text)
		.join(" ");

	return cleanName(name || fallbackName(conversation));
};

export default function (pi: ExtensionAPI) {
	pi.on("session_start", async (_event, ctx) => {
		currentContext = ctx;
		currentSessionName = pi.getSessionName();
		currentAttachments = await discoverAttachments(ctx.cwd, currentSessionName);
	});

	pi.on("session_info_changed", (event, ctx) => {
		currentSessionName = event.name;
		void refreshAttachments(ctx.cwd);
	});

	pi.on("session_shutdown", () => {
		currentContext = undefined;
	});

	customEditorPrototype.render = function (width: number) {
		const lines = originalEditorRender.call(this, width);
		if (!currentSessionName || lines.length === 0) {
			return lines;
		}

		const editor = this as CustomEditor & { borderColor?: (text: string) => string };
		const borderColor = editor.borderColor ?? ((text: string) => text);
		return [sessionNameBorder(currentSessionName, currentAttachments, width, borderColor), ...lines.slice(1)];
	};

	interactiveModePrototype.handleNameCommand = function (text: string) {
		const name = text.replace(/^\/name\s*/, "").trim();
		if (name) {
			return originalHandleNameCommand.call(this, text);
		}

		const ctx = currentContext;
		if (!ctx) {
			return;
		}

		ctx.ui.notify("Generating session name...", "info");
		void generateName(ctx)
			.then((generatedName) => {
				pi.setSessionName(generatedName);
				ctx.ui.notify(`Session name set: ${generatedName}`, "info");
			})
			.catch((error) => {
				const currentName = pi.getSessionName();
				if (currentName) {
					ctx.ui.notify(`Session name: ${currentName}`, "info");
					return;
				}
				ctx.ui.notify(error instanceof Error ? error.message : String(error), "warning");
			});
	};
}
