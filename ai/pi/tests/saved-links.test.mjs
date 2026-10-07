import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { pathToFileURL, fileURLToPath } from "node:url";
import test from "node:test";

const packageRoot = process.env.PI_PACKAGE_ROOT ?? join(execFileSync("npm", ["root", "-g"], { encoding: "utf8" }).trim(), "@earendil-works/pi-coding-agent");
const { loadExtensions, createExtensionRuntime } = await import(pathToFileURL(join(packageRoot, "dist/core/extensions/loader.js")));
const { SessionManager } = await import(pathToFileURL(join(packageRoot, "dist/core/session-manager.js")));
const { visibleWidth } = await import(pathToFileURL(join(packageRoot, "node_modules/@earendil-works/pi-tui/dist/index.js")));
const extensionPath = fileURLToPath(new URL("../extensions/saved-links.ts", import.meta.url));

async function setup(sessionManager = SessionManager.inMemory()) {
	const runtime = createExtensionRuntime();
	runtime.appendEntry = (type, data) => sessionManager.appendCustomEntry(type, data);
	const result = await loadExtensions([extensionPath], process.cwd(), undefined, runtime);
	assert.deepEqual(result.errors, []);
	const extension = result.extensions[0];
	const tool = extension.tools.get("saved_links").definition;
	const notifications = [];
	const ctx = { mode: "tui", sessionManager, ui: { notify: (...args) => notifications.push(args) } };
	return {
		ctx, extension, notifications,
		call: (params) => tool.execute("test", params, undefined, undefined, ctx),
		picker: () => extension.shortcuts.get("alt+l").handler(ctx),
	};
}

const theme = { fg: (_color, text) => text, bold: (text) => text };

test("save, retrieve case-insensitively, update, list, and remove", async () => {
	const { call } = await setup();
	await call({ action: "save", name: " Design Doc ", url: "https://example.com/design?q=1#section" });
	assert.deepEqual((await call({ action: "get", name: "design doc" })).details.link, { name: "Design Doc", url: "https://example.com/design?q=1#section" });
	await call({ action: "save", name: "DESIGN DOC", url: "https://example.com/new" });
	assert.deepEqual((await call({ action: "list" })).details.links, [{ name: "Design Doc", url: "https://example.com/new" }]);
	await call({ action: "remove", name: "design doc" });
	assert.deepEqual((await call({ action: "list" })).details.links, []);
	await assert.rejects(call({ action: "get", name: "missing" }), /No saved link/);
});

test("reject unsafe URLs and invalid names without persisting them", async () => {
	const { call } = await setup();
	for (const url of [undefined, "file:///etc/passwd", "javascript:alert(1)", "example.com", "https://", "https://example.com/\x1b", "https://example.com/a b"]) {
		await assert.rejects(call({ action: "save", name: "test", url }), /HTTP or HTTPS/);
	}
	for (const name of [undefined, " ", "doc\x1b[31m"]) {
		await assert.rejects(call({ action: "save", name, url: "https://example.com" }), /link name/);
	}
	assert.deepEqual((await call({ action: "list" })).details.links, []);
});

test("bookmarks survive disk resume and compaction, and follow branch history", async () => {
	const directory = mkdtempSync(join(tmpdir(), "pi-saved-links-"));
	try {
		const manager = SessionManager.create(process.cwd(), directory);
		manager.appendMessage({ role: "user", content: "hello", timestamp: Date.now() });
		manager.appendMessage({ role: "assistant", content: [{ type: "text", text: "hello" }], api: "openai-responses", provider: "openai", model: "test", usage: { input: 0, output: 0, cacheRead: 0, cacheWrite: 0, totalTokens: 0, cost: { input: 0, output: 0, cacheRead: 0, cacheWrite: 0, total: 0 } }, stopReason: "stop", timestamp: Date.now() });
		const { call } = await setup(manager);
		await call({ action: "save", name: "doc", url: "https://example.com/one" });
		const savedLeaf = manager.getLeafId();
		await call({ action: "save", name: "doc", url: "https://example.com/two" });
		manager.branch(savedLeaf);
		assert.equal((await call({ action: "get", name: "doc" })).details.link.url, "https://example.com/one");
		manager.appendCompaction("summary", savedLeaf, 100);
		const resumed = await setup(SessionManager.open(manager.getSessionFile()));
		assert.equal((await resumed.call({ action: "get", name: "doc" })).details.link.url, "https://example.com/one");
		const empty = await setup();
		assert.deepEqual((await empty.call({ action: "list" })).details.links, []);
	} finally {
		rmSync(directory, { recursive: true, force: true });
	}
});

test("shortcut and command share a searchable boxed picker; Tab does not select a link", async () => {
	const { ctx, call, extension, picker } = await setup();
	await call({ action: "save", name: "design doc", url: "https://example.com/design" });
	await call({ action: "save", name: "issue", url: "https://tracker.example.com/123" });
	ctx.ui.pasteToEditor = () => { throw new Error("Picker should not modify the prompt"); };
	ctx.ui.custom = async (factory, options) => {
		assert.equal(options.overlay, true);
		let result;
		const component = factory({ requestRender() {} }, theme, {}, (value) => { result = value; });
		component.focused = true;
		for (const character of "tracker") component.handleInput(character);
		for (const width of [20, 40, 80]) {
			const lines = component.render(width);
			assert.equal(lines[0], `╭${"─".repeat(width - 2)}╮`);
			assert.equal(lines.at(-1), `╰${"─".repeat(width - 2)}╯`);
			assert.ok(lines.slice(1, -1).every((line) => line.startsWith("│ ") && line.endsWith(" │")));
			assert.ok(lines.every((line) => visibleWidth(line) === width));
		}
		assert.ok(!component.render(80).join("\n").includes("Tab insert URL"));
		component.handleInput("\t");
		assert.equal(result, undefined);
		component.handleInput("\r");
		assert.deepEqual(result, { name: "issue", url: "https://tracker.example.com/123" });
		return undefined;
	};
	await picker();
	await extension.commands.get("links").handler("", ctx);
});

test("picker handles navigation, opening selection, empty search, and cancellation", async () => {
	const { ctx, call, picker } = await setup();
	await call({ action: "save", name: "one", url: "https://example.com/one" });
	await call({ action: "save", name: "two", url: "https://example.com/two" });
	ctx.ui.custom = async (factory) => {
		let result;
		const component = factory({ requestRender() {} }, theme, {}, (value) => { result = value; });
		component.handleInput("\x1b[B");
		component.handleInput("\r");
		assert.deepEqual(result, { name: "two", url: "https://example.com/two" });
		result = undefined;
		component.handleInput("z");
		component.handleInput("\t");
		assert.equal(result, undefined);
		component.handleInput("\x1b");
		assert.equal(result, undefined);
		return undefined;
	};
	await picker();
});

test("empty sessions and non-terminal modes do not open the picker", async () => {
	const { ctx, picker, notifications } = await setup();
	ctx.ui.custom = () => { throw new Error("Should not open picker"); };
	await picker();
	assert.match(notifications[0][0], /No saved links/);
	ctx.mode = "print";
	await picker();
	assert.match(notifications[1][0], /interactive terminal/);
});
