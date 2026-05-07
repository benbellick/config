import { complete } from "@earendil-works/pi-ai";
import { InteractiveMode, type ExtensionAPI } from "@earendil-works/pi-coding-agent";

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

const originalHandleNameCommandSymbol = Symbol.for("ben.pi.auto-name.originalHandleNameCommand");
const interactiveModePrototype = InteractiveMode.prototype as typeof InteractiveMode.prototype & {
	[originalHandleNameCommandSymbol]?: (text: string) => void;
	handleNameCommand: (text: string) => void;
};

interactiveModePrototype[originalHandleNameCommandSymbol] ??= interactiveModePrototype.handleNameCommand;
const originalHandleNameCommand = interactiveModePrototype[originalHandleNameCommandSymbol];

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

export default function (_pi: ExtensionAPI) {
	interactiveModePrototype.handleNameCommand = function (text: string) {
		const name = text.replace(/^\/name\s*/, "").trim();
		if (name) {
			return originalHandleNameCommand.call(this, text);
		}

		const mode = this as InteractiveModeWithInternals;
		mode.showStatus("Generating session name...");

		void generateName(mode)
			.then((generatedName) => {
				mode.session.setSessionName(generatedName);
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
