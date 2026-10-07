import { execFile } from "node:child_process";
import type { ExtensionAPI, ExtensionContext } from "@earendil-works/pi-coding-agent";
import { Container, Input, Key, matchesKey, SelectList, Text, truncateToWidth, visibleWidth } from "@earendil-works/pi-tui";
import { Type } from "typebox";

type SavedLink = { name: string; url: string };
type PickerResult = { action: "open" | "insert"; link: SavedLink } | undefined;
const ENTRY_TYPE = "ben.saved-links";

function validateName(value: string | undefined): string {
	const name = value?.trim();
	if (!name || /[\x00-\x1f\x7f]/.test(name)) {
		throw new Error("Provide a non-empty link name without control characters.");
	}
	return name;
}

function validateUrl(value: string | undefined): string {
	const url = value?.trim();
	if (!url || /[\x00-\x20\x7f]/.test(url) || !/^https?:\/\//i.test(url)) {
		throw new Error("Provide a full HTTP or HTTPS URL without whitespace.");
	}
	try {
		const parsed = new URL(url);
		if (!parsed.hostname) throw new Error();
	} catch {
		throw new Error("Provide a valid HTTP or HTTPS URL.");
	}
	return url;
}

function readLinks(ctx: ExtensionContext): SavedLink[] {
	for (const entry of ctx.sessionManager.getBranch().slice().reverse()) {
		if (entry.type === "custom" && entry.customType === ENTRY_TYPE) {
			return (entry.data as { links: SavedLink[] }).links;
		}
	}
	return [];
}

function openUrl(url: string): Promise<void> {
	const command = process.platform === "darwin" ? "open" : process.platform === "win32" ? "rundll32" : "xdg-open";
	const args = process.platform === "win32" ? ["url.dll,FileProtocolHandler", url] : [url];
	return new Promise((resolve, reject) => {
		execFile(command, args, { timeout: 10_000 }, (error) => error ? reject(error) : resolve());
	});
}

async function showLinks(ctx: ExtensionContext): Promise<void> {
	if (ctx.mode !== "tui") {
		ctx.ui.notify("The links picker requires interactive terminal mode.", "warning");
		return;
	}
	const links = readLinks(ctx);
	if (!links.length) {
		ctx.ui.notify('No saved links. Ask the assistant to save a URL with a name.', "info");
		return;
	}

	const result = await ctx.ui.custom<PickerResult>((tui, theme, _keybindings, done) => {
		const input = new Input({ prompt: "Search: " });
		const container = new Container();
		const listContainer = new Container();
		const listTheme = {
			selectedPrefix: (text: string) => theme.fg("accent", text),
			selectedText: (text: string) => theme.fg("accent", text),
			description: (text: string) => theme.fg("muted", text),
			scrollInfo: (text: string) => theme.fg("dim", text),
			noMatch: (text: string) => theme.fg("warning", text),
		};
		let list: SelectList;
		const rebuildList = () => {
			const query = input.getValue().toLowerCase();
			const matches = links.filter((link) => `${link.name}\n${link.url}`.toLowerCase().includes(query));
			list = new SelectList(matches.map((link) => ({ value: link.name, label: link.name, description: link.url })), 8, listTheme);
			list.onSelect = (item) => done({ action: "open", link: links.find((link) => link.name === item.value)! });
			list.onCancel = () => done(undefined);
			listContainer.clear();
			listContainer.addChild(list);
		};
		rebuildList();
		container.addChild(new Text(theme.fg("accent", theme.bold("Saved links")), 0, 0));
		container.addChild(input);
		container.addChild(listContainer);
		container.addChild(new Text(theme.fg("dim", "↑↓ select · Enter open · Tab insert URL · Esc close"), 0, 0));
		return {
			get focused() { return input.focused; },
			set focused(value: boolean) { input.focused = value; },
			render(width: number) {
				const border = (text: string) => theme.fg("accent", text);
				if (width < 4) return [border("─".repeat(Math.max(0, width)))];
				const contentWidth = width - 4;
				const lines = container.render(contentWidth).map((line) => {
					const content = truncateToWidth(line, contentWidth);
					const padding = " ".repeat(contentWidth - visibleWidth(content));
					return border("│ ") + content + padding + border(" │");
				});
				return [border(`╭${"─".repeat(width - 2)}╮`), ...lines, border(`╰${"─".repeat(width - 2)}╯`)];
			},
			invalidate: () => container.invalidate(),
			handleInput(data: string) {
				if (matchesKey(data, Key.tab)) {
					const selected = list.getSelectedItem();
					if (selected) done({ action: "insert", link: links.find((link) => link.name === selected.value)! });
				} else if ([Key.up, Key.down, Key.pageUp, Key.pageDown, Key.enter, Key.escape, Key.ctrl("c")].some((key) => matchesKey(data, key))) {
					list.handleInput(data);
				} else {
					input.handleInput(data);
					rebuildList();
				}
				tui.requestRender();
			},
		};
	}, { overlay: true, overlayOptions: { width: "80%", maxHeight: "80%" } });

	if (!result) return;
	if (result.action === "insert") {
		ctx.ui.pasteToEditor(result.link.url);
		return;
	}
	try {
		await openUrl(result.link.url);
	} catch (error) {
		ctx.ui.notify(`Could not open ${result.link.url}: ${error instanceof Error ? error.message : String(error)}`, "error");
	}
}

export default function savedLinks(pi: ExtensionAPI) {
	pi.registerTool({
		name: "saved_links",
		label: "Saved links",
		description: "Manage named HTTP(S) URL bookmarks on the current session branch. Save only when the user asks. Actions: save (name, url), list, get (name), remove (name). Names are case-insensitive; saving an existing name updates its URL. Links persist across reload, resume, and compaction. Saving never fetches or opens the URL.",
		promptSnippet: "Save, list, retrieve, or remove named URL bookmarks attached to this session.",
		promptGuidelines: ["When the user refers to a saved link by name, retrieve it with saved_links before using its URL."],
		parameters: Type.Object({
			action: Type.Union([Type.Literal("save"), Type.Literal("list"), Type.Literal("get"), Type.Literal("remove")]),
			name: Type.Optional(Type.String({ description: "Bookmark name, e.g. design doc" })),
			url: Type.Optional(Type.String({ description: "Full HTTP or HTTPS URL; required for save" })),
		}),
		executionMode: "sequential",
		async execute(_id, params, _signal, _onUpdate, ctx) {
			let links = readLinks(ctx);
			if (params.action === "list") {
				return { content: [{ type: "text", text: links.length ? JSON.stringify(links, null, 2) : "No saved links." }], details: { links } };
			}
			const name = validateName(params.name);
			const existing = links.find((link) => link.name.toLowerCase() === name.toLowerCase());
			if (params.action === "save") {
				const link = { name: existing?.name ?? name, url: validateUrl(params.url) };
				links = existing ? links.map((saved) => saved === existing ? link : saved) : [...links, link];
				pi.appendEntry(ENTRY_TYPE, { links });
				return { content: [{ type: "text", text: `Saved ${link.name}: ${link.url}` }], details: { link } };
			}
			if (!existing) throw new Error(`No saved link named "${name}". Use list to see available names.`);
			if (params.action === "remove") {
				pi.appendEntry(ENTRY_TYPE, { links: links.filter((link) => link !== existing) });
				return { content: [{ type: "text", text: `Removed ${existing.name}.` }], details: { link: existing } };
			}
			return { content: [{ type: "text", text: JSON.stringify(existing) }], details: { link: existing } };
		},
	});
	pi.registerCommand("links", { description: "Browse saved session URLs", handler: async (_args, ctx) => showLinks(ctx) });
	pi.registerShortcut(Key.alt("l"), { description: "Browse saved session URLs", handler: showLinks });
}
