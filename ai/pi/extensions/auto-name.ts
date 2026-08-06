import { execFile } from "node:child_process";
import { complete } from "@earendil-works/pi-ai";
import { CustomEditor, InteractiveMode, type ExtensionAPI } from "@earendil-works/pi-coding-agent";
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

type InteractiveModeWithInternals = InteractiveMode & {
	sessionManager: {
		getBranch(): SessionEntry[];
		getSessionName(): string | undefined;
		getCwd(): string;
	};
	session: {
		state: { model?: unknown; thinkingLevel?: string };
		modelRegistry: {
			getApiKeyAndHeaders(model: unknown): Promise<
				| { ok: true; apiKey?: string; headers?: Record<string, string> }
				| { ok: false; error: string }
			>;
		};
		setSessionName(name: string): void;
	};
	showStatus(message: string): void;
	showWarning(message: string): void;
	showError(message: string): void;
};

const MAX_CONVERSATION_CHARS = 12_000;

type SessionAttachments = {
	ticket?: string;
	ticketUrl?: string;
	pr?: string;
	prUrl?: string;
};

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

const refreshAttachments = async (mode: Pick<InteractiveModeWithInternals, "sessionManager"> & { cwd?: string }) => {
	currentAttachments = await discoverAttachments(mode.cwd ?? mode.sessionManager.getCwd(), currentSessionName);
};

const generateName = async (mode: InteractiveModeWithInternals): Promise<string> => {
	const conversation = conversationText(mode.sessionManager.getBranch());
	if (!conversation) {
		throw new Error("No conversation text found");
	}

	const model = mode.session.state.model;
	if (!model) {
		return fallbackName(conversation);
	}

	const auth = await mode.session.modelRegistry.getApiKeyAndHeaders(model);
	if (!auth.ok || !auth.apiKey) {
		return fallbackName(conversation);
	}

	const response = await complete(
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
			apiKey: auth.apiKey,
			headers: auth.headers,
			reasoningEffort: "minimal",
		},
	);

	const name = response.content
		.filter((part): part is { type: "text"; text: string } => part.type === "text")
		.map((part) => part.text)
		.join(" ");

	return cleanName(name || fallbackName(conversation));
};

export default function (pi: ExtensionAPI) {
	pi.on("session_start", async (_event, ctx) => {
		currentSessionName = ctx.sessionManager.getSessionName();
		currentAttachments = await discoverAttachments(ctx.cwd, currentSessionName);
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
			const result = originalHandleNameCommand.call(this, text);
			const mode = this as InteractiveModeWithInternals;
			currentSessionName = mode.sessionManager.getSessionName();
			void refreshAttachments(mode);
			return result;
		}

		const mode = this as InteractiveModeWithInternals;
		mode.showStatus("Generating session name...");

		void generateName(mode)
			.then((generatedName) => {
				mode.session.setSessionName(generatedName);
				currentSessionName = generatedName;
				void refreshAttachments(mode);
				mode.showStatus(`Session name set: ${generatedName}`);
			})
			.catch((error) => {
				const currentName = mode.sessionManager.getSessionName();
				if (currentName) {
					mode.showStatus(`Session name: ${currentName}`);
					return;
				}
				mode.showWarning(error instanceof Error ? error.message : String(error));
			});
	};
}
