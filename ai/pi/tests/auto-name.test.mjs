import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import { join } from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";
import test from "node:test";

const packageRoot = process.env.PI_PACKAGE_ROOT ?? join(execFileSync("npm", ["root", "-g"], { encoding: "utf8" }).trim(), "@earendil-works/pi-coding-agent");
const { loadExtensions, createExtensionRuntime } = await import(pathToFileURL(join(packageRoot, "dist/core/extensions/loader.js")));
const { CustomEditor } = await import(pathToFileURL(join(packageRoot, "dist/index.js")));
const { visibleWidth } = await import(pathToFileURL(join(packageRoot, "node_modules/@earendil-works/pi-tui/dist/index.js")));

test("auto-name displays only the session name and preserves border layout", async (t) => {
	const prototype = CustomEditor.prototype;
	const originalBorderSymbol = Symbol.for("ben.pi.auto-name.originalEditorTopBorder");
	const originalRender = prototype.renderTopBorder;
	const savedOriginal = prototype[originalBorderSymbol];
	prototype[originalBorderSymbol] = (width) => "─".repeat(width);
	t.after(() => {
		prototype.renderTopBorder = originalRender;
		if (savedOriginal) prototype[originalBorderSymbol] = savedOriginal;
		else delete prototype[originalBorderSymbol];
	});
	const runtime = createExtensionRuntime();
	runtime.getSessionName = () => "DDSQL-123 Improve query planning";
	const extensionPath = fileURLToPath(new URL("../extensions/auto-name.ts", import.meta.url));
	const result = await loadExtensions([extensionPath], process.cwd(), undefined, runtime);
	assert.deepEqual(result.errors, []);
	const extension = result.extensions[0];
	const emit = async (event, data = {}) => {
		for (const handler of extension.handlers.get(event) ?? []) await handler(data, {});
	};
	await emit("session_start");
	const editor = { embedWorkingStatus: false };
	const render = (width, hidden = 0) => prototype.renderTopBorder.call(editor, width, hidden);

	assert.ok(render(80).endsWith(" [DDSQL-123 Improve query planning] "));
	assert.ok(!render(80).includes("\x1b]8"));
	assert.equal(visibleWidth(render(80)), 80);
	assert.equal(render(12), "─".repeat(12));
	assert.equal(render(80, 1), "─".repeat(80));

	await emit("session_info_changed", { name: "A long name with wide characters 查询查询查询查询查询查询查询" });
	assert.equal(visibleWidth(render(40)), 40);
	assert.ok(render(40).includes("…"));
	editor.embedWorkingStatus = true;
	editor.workingStatusIndicator = { renderInBorder: () => "Working" };
	assert.ok(render(40).startsWith("─".repeat(12)));
	assert.equal(visibleWidth(render(40)), 40);

	await emit("session_info_changed", { name: "Line one\nLine two\tmore" });
	assert.ok(render(80).endsWith(" [Line one Line two more] "));
	await emit("session_info_changed", { name: undefined });
	assert.equal(render(80), "─".repeat(80));
});
