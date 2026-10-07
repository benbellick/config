import {
	CustomEditor,
	InteractiveMode,
	type ExtensionAPI,
	type ExtensionContext,
} from "@earendil-works/pi-coding-agent";
import { sliceByColumn, truncateToWidth, visibleWidth } from "@earendil-works/pi-tui";

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

let currentContext: ExtensionContext | undefined;
let currentSessionName: string | undefined;

const originalHandleNameCommandSymbol = Symbol.for("ben.pi.auto-name.originalHandleNameCommand");
const interactiveModePrototype = InteractiveMode.prototype as typeof InteractiveMode.prototype & {
	[originalHandleNameCommandSymbol]?: (text: string) => void;
	handleNameCommand: (text: string) => void;
};

interactiveModePrototype[originalHandleNameCommandSymbol] ??= interactiveModePrototype.handleNameCommand;
const originalHandleNameCommand = interactiveModePrototype[originalHandleNameCommandSymbol];

const originalEditorTopBorderSymbol = Symbol.for("ben.pi.auto-name.originalEditorTopBorder");
const previousEditorRenderSymbol = Symbol.for("ben.pi.auto-name.originalEditorRender");
const customEditorPrototype = CustomEditor.prototype as typeof CustomEditor.prototype & {
	[originalEditorTopBorderSymbol]?: (width: number, hiddenLineCount: number) => string;
	[previousEditorRenderSymbol]?: (width: number) => string[];
};

if (customEditorPrototype[previousEditorRenderSymbol]) {
	customEditorPrototype.render = customEditorPrototype[previousEditorRenderSymbol];
}
customEditorPrototype[originalEditorTopBorderSymbol] ??= customEditorPrototype.renderTopBorder;
const originalEditorTopBorder = customEditorPrototype[originalEditorTopBorderSymbol];

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

const sessionNameBorder = (
	border: string,
	name: string,
	width: number,
	reservedLeftWidth: number,
	color: (text: string) => string,
): string => {
	const maxNameWidth = width - reservedLeftWidth - 6;
	if (maxNameWidth < 8) {
		return border;
	}

	const displayName = name.replace(/[\r\n\t]/g, " ").replace(/ +/g, " ").trim();
	const truncatedName = truncateToWidth(displayName, maxNameWidth, "…");
	const rightLabel = ` [${truncatedName}] `;
	return sliceByColumn(border, 0, width - visibleWidth(rightLabel)) + color(rightLabel);
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
	pi.on("session_start", (_event, ctx) => {
		currentContext = ctx;
		currentSessionName = pi.getSessionName();
	});

	pi.on("session_info_changed", (event) => {
		currentSessionName = event.name;
	});

	pi.on("session_shutdown", () => {
		currentContext = undefined;
	});

	customEditorPrototype.renderTopBorder = function (width: number, hiddenLineCount: number) {
		const border = originalEditorTopBorder.call(this, width, hiddenLineCount);
		if (!currentSessionName || hiddenLineCount > 0) {
			return border;
		}

		const status = this.embedWorkingStatus && this.workingStatusIndicator
			? this.workingStatusIndicator.renderInBorder(Math.max(1, width - 5))
			: "";
		const reservedLeftWidth = status ? visibleWidth(status) + 5 : 3;
		const borderColor = this.borderColor ?? ((text: string) => text);
		return sessionNameBorder(border, currentSessionName, width, reservedLeftWidth, borderColor);
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
