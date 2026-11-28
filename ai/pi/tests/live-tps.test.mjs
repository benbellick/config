import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import { mkdtempSync, rmSync } from "node:fs";
import { createRequire } from "node:module";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { pathToFileURL } from "node:url";
import test from "node:test";
import liveTps, { renderTpsGraph, sampleTpsHistory } from "../extensions/live-tps.ts";

const asciichart = createRequire(new URL("../extensions/package.json", import.meta.url))("asciichart");
const EPOCH = Date.parse("2026-01-01T00:00:00Z");

function setup(t, hasUI = true, manager) {
	let now = 0;
	t.mock.method(performance, "now", () => now);
	t.mock.method(Date, "now", () => EPOCH + now);
	t.mock.timers.enable({ apis: ["setInterval"] });
	const handlers = new Map();
	const statuses = new Map();
	const commands = new Map();
	const entries = [];
	const sessionManager = manager ?? {
		getHeader: () => ({ timestamp: new Date(EPOCH).toISOString() }),
		getBranch: () => entries,
		appendCustomEntry: (customType, data) => entries.push({
			type: "custom", customType, data: structuredClone(data), timestamp: new Date(Date.now()).toISOString(),
		}),
	};
	const terminal = { rows: 30 };
	let component;
	let overlayOptions;
	let redraws = 0;
	let closeOverlay;
	const ctx = {
		hasUI,
		mode: hasUI ? "tui" : "print",
		sessionManager,
		ui: {
			setStatus: (key, value) => statuses.set(key, value),
			custom: (factory, options) => {
				overlayOptions = options;
				return new Promise((resolve) => {
					closeOverlay = resolve;
					component = factory(
						{ requestRender: () => redraws++, terminal },
						{ fg: (_color, text) => text },
						{ matches: (data, key) =>
							(data === "escape" && key === "tui.select.cancel") ||
							(data === "tab" && key === "tui.input.tab") },
						resolve,
					);
				});
			},
		},
	};
	const loadExtension = () => {
		handlers.clear();
		commands.clear();
		liveTps({
			on: (name, handler) => handlers.set(name, handler),
			registerCommand: (name, command) => commands.set(name, command),
			appendEntry: (type, data) => ctx.sessionManager.appendCustomEntry(type, data),
		});
	};
	loadExtension();
	const emit = (name, event = {}) => handlers.get(name)?.(event, ctx);
	emit("session_start");
	t.after(() => {
		emit("session_shutdown");
		closeOverlay?.();
	});
	const advance = (milliseconds) => {
		while (milliseconds > 0) {
			const step = Math.min(250, milliseconds);
			now += step;
			t.mock.timers.tick(step);
			milliseconds -= step;
		}
	};
	const start = () => emit("message_start", { message: { role: "assistant" } });
	const delta = (type, text) => emit("message_update", {
		assistantMessageEvent: { type, delta: text },
	});
	const end = (output = 0, stopReason = "stop") => emit("message_end", {
		message: { role: "assistant", usage: { output }, stopReason },
	});
	const saved = () => ctx.sessionManager.getBranch().filter((entry) => entry.customType === "live-tps.graph");
	return {
		emit, advance, start, delta, end, ctx, statuses, terminal, saved,
		history: () => saved().flatMap((entry) => entry.data.intervals ?? []),
		reload: () => {
			emit("session_shutdown", { reason: "reload" });
			loadExtension();
			emit("session_start", { reason: "reload" });
		},
		status: () => statuses.get("live-tps"),
		openGraph: () => commands.get("tps").handler("", ctx),
		graph: () => component,
		overlayOptions: () => overlayOptions,
		redraws: () => redraws,
	};
}

test("footer averages exclude initial latency and count text, thinking, and tool arguments", (t) => {
	const h = setup(t);
	h.start();
	h.advance(10_000);
	assert.equal(h.status(), "TPS waiting…");
	h.delta("text_delta", "a".repeat(40));
	h.delta("thinking_delta", "b".repeat(40));
	h.delta("toolcall_delta", "c".repeat(40));
	h.delta("text_end", "ignored".repeat(100));
	h.advance(1000);
	assert.equal(h.status(), "~30.0 TPS");
	h.advance(1000);
	assert.equal(h.status(), "~15.0 TPS");
	h.end(80);
	assert.equal(h.status(), "40.0 TPS");
	assert.equal(h.history().reduce((tokens, interval) => tokens + interval.tokens, 0), 30);
});

test("missing usage keeps the estimate and footer averages restart for each response", (t) => {
	const h = setup(t);
	h.start();
	h.delta("text_delta", "a".repeat(80));
	h.advance(1000);
	h.end();
	assert.equal(h.status(), "~20.0 TPS");
	h.start();
	assert.equal(h.status(), "TPS waiting…");
	h.delta("text_delta", "b".repeat(40));
	h.advance(1000);
	h.end(12);
	assert.equal(h.status(), "12.0 TPS");
	assert.equal(h.saved().length, 2);
});

test("session history spans responses and preserves idle and tool gaps as zero TPS", (t) => {
	const h = setup(t);
	h.start();
	h.delta("text_delta", "a".repeat(80));
	h.advance(1000);
	h.end(20);
	h.emit("message_start", { message: { role: "toolResult" } });
	h.advance(2000);
	h.emit("message_end", { message: { role: "toolResult" } });
	h.start();
	h.delta("text_delta", "b".repeat(160));
	h.advance(1000);
	h.end(40);
	assert.deepEqual(sampleTpsHistory(h.history(), 0, 5, 80).map((sample) => sample.rate), [20, 0, 0, 40, 0, 0]);
	assert.equal(h.saved()[0].data.intervals.length, 1);
	assert.equal(h.saved()[1].data.intervals.length, 1);
});

test("sampling records changing interval speed instead of cumulative averages", (t) => {
	const h = setup(t);
	h.start();
	h.delta("text_delta", "a".repeat(80));
	h.advance(1000);
	h.delta("text_delta", "b".repeat(160));
	h.advance(1000);
	h.end();
	assert.deepEqual(sampleTpsHistory(h.history(), 0, 2, 80).map((sample) => sample.rate), [20, 40, 40]);
	assert.equal(h.status(), "~30.0 TPS");
});

test("aggregation clips window boundaries, distributes overlaps, and conserves tokens", () => {
	const intervals = [{ start: 0.5, end: 1.5, tokens: 20 }, { start: 3, end: 4, tokens: 40 }];
	assert.deepEqual(sampleTpsHistory(intervals, 0, 4, 80).map((sample) => sample.rate), [10, 10, 0, 40, 40]);
	assert.deepEqual(sampleTpsHistory(intervals, 1, 4, 80).map((sample) => sample.rate), [10, 0, 40, 40]);
	assert.deepEqual(sampleTpsHistory(intervals, 0, 4, 2).map((sample) => sample.rate), [10, 20, 20]);
	assert.deepEqual(sampleTpsHistory([], 0, 0, 80), [{ seconds: 0, rate: 0 }]);
	assert.deepEqual(sampleTpsHistory([], 8, 10, 80).map((sample) => sample.rate), [0, 0, 0]);
	assert.equal(sampleTpsHistory([{ start: 0, end: 0.5, tokens: 10 }], 0, 0.5, 80)[0].rate, 20);
});

test("fractional bucket widths do not create a zero-duration final bucket", () => {
	const end = 64.17647058823529;
	const samples = sampleTpsHistory([], 0, end, 13);
	assert.equal(samples.length, 14);
	assert.ok(samples.every((sample) => Number.isFinite(sample.rate)));
	assert.doesNotThrow(() => renderTpsGraph(samples, 21, 10));
});

test("sampling stays finite and conserves tokens across fractional windows and widths", () => {
	for (const start of [0, 10.123, 86400.25]) {
		for (const duration of [0.001, 0.25, 1.001, 59.999, 60, 64.17647058823529, 3600.123]) {
			for (const maximumSamples of [1, 7, 13, 60, 73, 100]) {
				const end = start + duration;
				const samples = sampleTpsHistory([{ start, end, tokens: 100 }], start, end, maximumSamples);
				assert.ok(samples.length <= maximumSamples + 2);
				assert.ok(samples.every((sample) => Number.isFinite(sample.rate) && sample.rate >= 0));
				const tokens = samples.slice(0, -1).reduce((total, sample, index) =>
					total + sample.rate * (samples[index + 1].seconds - sample.seconds), 0);
				assert.ok(Math.abs(tokens - 100) < 0.00001);
				assert.doesNotThrow(() => renderTpsGraph(samples, maximumSamples + 16, 10));
			}
		}
	}
});

test("time buckets stay session-aligned as the window moves or grows", () => {
	const intervals = [{ start: 10.2, end: 10.8, tokens: 12 }];
	const full = sampleTpsHistory(intervals, 0, 60, 80);
	const moving = sampleTpsHistory(intervals, 0.2, 60.2, 80);
	const growing = sampleTpsHistory(intervals, 0, 60.8, 100);
	assert.deepEqual(full.find((sample) => sample.seconds === 10), { seconds: 10, rate: 12 });
	assert.deepEqual(moving.find((sample) => sample.seconds === 10), { seconds: 10, rate: 12 });
	assert.deepEqual(growing.find((sample) => sample.seconds === 10), { seconds: 10, rate: 12 });
	assert.equal(moving[0].seconds, 0.2);
	assert.equal(moving[1].seconds, 1);
	assert.equal(moving.at(-1).seconds, 60.2);
});

test("rounding around a one-second resolution does not double the bucket width", () => {
	const samples = sampleTpsHistory([], 10.123, 70.123, 60);
	assert.equal(samples[1].seconds, 11);
	assert.equal(samples[2].seconds, 12);
});

test("edge buckets use their clipped durations and exact boundaries do not leak tokens", () => {
	const samples = sampleTpsHistory([
		{ start: 10, end: 11, tokens: 20 },
		{ start: 12, end: 13, tokens: 40 },
	], 10.25, 12.75, 80);
	assert.deepEqual(samples, [
		{ seconds: 10.25, rate: 20 },
		{ seconds: 11, rate: 0 },
		{ seconds: 12, rate: 40 },
		{ seconds: 12.75, rate: 40 },
	]);
	assert.deepEqual(sampleTpsHistory([
		{ start: 0, end: 1, tokens: 10 }, { start: 1, end: 2, tokens: 20 },
	], 0, 3, 80).map((sample) => sample.rate), [10, 20, 0, 0]);
});

test("randomized windows conserve visible tokens when bucket widths change", () => {
	let seed = 12345;
	const random = () => ((seed = (Math.imul(seed, 1664525) + 1013904223) >>> 0) / 2 ** 32);
	for (let iteration = 0; iteration < 500; iteration++) {
		const start = random() * 86400;
		const end = start + 0.001 + random() * 600;
		const maximumSamples = 1 + Math.floor(random() * 100);
		const intervals = Array.from({ length: 20 }, () => {
			const from = start - 20 + random() * (end - start + 40);
			return { start: from, end: from + 0.001 + random() * 10, tokens: random() * 100 };
		});
		const samples = sampleTpsHistory(intervals, start, end, maximumSamples);
		assert.ok(samples.length <= maximumSamples + 2);
		assert.ok(samples.every((sample) => Number.isFinite(sample.rate) && sample.rate >= 0));
		const actualTokens = samples.slice(0, -1).reduce((total, sample, index) =>
			total + sample.rate * (samples[index + 1].seconds - sample.seconds), 0);
		const expectedTokens = intervals.reduce((total, interval) => total + interval.tokens *
			Math.max(0, Math.min(end, interval.end) - Math.max(start, interval.start)) / (interval.end - interval.start), 0);
		assert.ok(Math.abs(actualTokens - expectedTokens) < 0.00001);
		assert.doesNotThrow(() => renderTpsGraph(samples, maximumSamples + 16, 10));
	}
});

test("graph rendering delegates plot lines and vertical labels to asciichart", () => {
	const lines = renderTpsGraph([{ seconds: 0, rate: 20 }, { seconds: 1, rate: 20 }], 40, 8);
	const expected = asciichart.plot(Array(35).fill(20), {
		min: 0, max: 20, height: 7, offset: 2,
		format: (value) => `${value.toFixed(0).padStart(3)} `,
	}).split("\n").map((line) => line.trimEnd());
	assert.deepEqual(lines.slice(0, -2), expected);
});

test("graph handles invalid rates without crashing the terminal UI", () => {
	const samples = [NaN, Infinity, -Infinity, -10, 20].map((rate, seconds) => ({ seconds, rate }));
	const lines = renderTpsGraph(samples, 60, 10);
	assert.doesNotMatch(lines.join("\n"), /NaN|Infinity/);
	assert.ok(lines.every((line) => line.length <= 60));
});

test("empty and very short streams do not display infinite rates", (t) => {
	const h = setup(t);
	h.start();
	h.delta("text_delta", "");
	h.end();
	assert.equal(h.status(), undefined);
	h.start();
	h.delta("text_delta", "hello");
	h.end(1);
	assert.equal(h.status(), undefined);
	assert.deepEqual(h.history(), []);
});

for (const reason of ["error", "aborted"]) {
	test(`${reason} retains recorded history without a success footer`, async (t) => {
		const h = setup(t);
		h.start();
		h.delta("text_delta", "a".repeat(40));
		h.advance(1000);
		h.end(10, reason);
		h.reload();
		assert.equal(h.status(), undefined);
		assert.equal(h.history().length, 1);
		const opened = h.openGraph();
		assert.match(h.graph().render(80).join("\n"), reason === "error" ? /Error/ : /Aborted/);
		h.graph().handleInput("escape");
		await opened;
	});
}

test("headless sessions do not create statuses, intervals, or overlays", async (t) => {
	const h = setup(t, false);
	h.start();
	h.delta("text_delta", "hello");
	h.advance(1000);
	h.end(10);
	await h.openGraph();
	assert.equal(h.statuses.size, 0);
	assert.equal(h.saved().length, 0);
	assert.equal(h.graph(), undefined);
});

test("/tps toggles full session and last 60 seconds and advances while idle", async (t) => {
	const h = setup(t);
	h.start();
	h.delta("text_delta", "a".repeat(80));
	h.advance(1000);
	h.end(20);
	h.advance(69_000);
	const opened = h.openGraph();
	assert.equal(h.overlayOptions().overlay, true);
	let rendered = h.graph().render(90).join("\n");
	assert.match(rendered, /Full session/);
	assert.match(rendered, /0.0s/);
	assert.match(rendered, /70.0s/);
	assert.match(rendered, /Response average 20.0 TPS/);
	assert.match(rendered, /Tab: 60s \/ full history/);
	h.graph().handleInput("tab");
	rendered = h.graph().render(90).join("\n");
	assert.match(rendered, /Last 60s/);
	assert.match(rendered, /10.0s/);
	const redraws = h.redraws();
	h.advance(1000);
	assert.ok(h.redraws() > redraws);
	assert.match(h.graph().render(90).join("\n"), /11.0s/);
	assert.match(h.graph().render(90).join("\n"), /71.0s/);
	h.graph().handleInput("tab");
	assert.match(h.graph().render(90).join("\n"), /Full session/);
	h.graph().handleInput("escape");
	await opened;
	const closedRedraws = h.redraws();
	h.advance(1000);
	assert.equal(h.redraws(), closedRedraws);
});

test("model changes and new responses do not erase session history", (t) => {
	const h = setup(t);
	h.start();
	h.delta("text_delta", "a".repeat(40));
	h.advance(1000);
	h.end(10);
	const firstHistory = structuredClone(h.history());
	h.emit("model_select");
	h.start();
	h.advance(1000);
	h.end();
	assert.deepEqual(h.history(), firstHistory);
});

test("/tps ignores duplicate opens and retains the selected view when reopened", async (t) => {
	const h = setup(t);
	const opened = h.openGraph();
	const firstGraph = h.graph();
	await h.openGraph();
	assert.equal(h.graph(), firstGraph);
	h.graph().handleInput("tab");
	h.graph().handleInput("escape");
	await opened;
	const reopened = h.openGraph();
	assert.match(h.graph().render(80).join("\n"), /Last 60s/);
	h.graph().handleInput("escape");
	await reopened;
});

test("overlay has a closed padded frame within small and normal terminal sizes", async (t) => {
	const h = setup(t);
	h.advance(1000);
	const opened = h.openGraph();
	for (const rows of [10, 15, 20, 30]) {
		h.terminal.rows = rows;
		for (const width of [4, 12, 20, 40, 80]) {
			const lines = h.graph().render(width);
			assert.equal(lines[0], `╭${"─".repeat(width - 2)}╮`);
			assert.equal(lines.at(-1), `╰${"─".repeat(width - 2)}╯`);
			assert.ok(lines.slice(1, -1).every((line) => line.startsWith("│ ") && line.endsWith(" │")));
			assert.ok(lines.every((line) => line.length === width));
			assert.ok(lines.length <= Math.floor(rows * 0.9));
		}
	}
	h.graph().handleInput("escape");
	await opened;
});

test("history grows beyond the previous cap and saves only each response's new intervals", (t) => {
	const h = setup(t);
	for (let response = 0; response < 2; response++) {
		h.start();
		for (let sample = 0; sample < 650; sample++) {
			h.delta("text_delta", "a".repeat(4));
			h.advance(250);
		}
		h.end();
	}
	assert.equal(h.history().length, 1300);
	assert.deepEqual(h.saved().map((entry) => entry.data.intervals.length), [650, 650]);
	const snapshot = structuredClone(h.history());
	h.reload();
	assert.deepEqual(h.history(), snapshot);
	assert.equal(h.saved().length, 2);
});

test("reload retains the original time origin and the offline gap", async (t) => {
	const h = setup(t);
	h.start();
	h.delta("text_delta", "a".repeat(80));
	h.advance(1000);
	h.end(20);
	h.emit("session_shutdown");
	h.advance(9000);
	h.reload();
	assert.equal(h.status(), "20.0 TPS");
	const opened = h.openGraph();
	assert.match(h.graph().render(80).join("\n"), /10.0s/);
	assert.equal(h.history().length, 1);
	h.graph().handleInput("escape");
	await opened;
});

for (const event of ["session_shutdown", "agent_settled", "model_select"]) {
	test(`${event} checkpoints unfinished output without duplicating it on reload`, (t) => {
		const h = setup(t);
		h.start();
		h.delta("text_delta", "a".repeat(40));
		h.advance(1000);
		h.emit(event);
		h.reload();
		h.advance(1000);
		assert.equal(h.status(), undefined);
		assert.equal(h.saved().length, 1);
		assert.equal(h.saved()[0].data.responseState, "Interrupted");
		assert.equal(h.history().length, 1);
	});
}

test("legacy per-response averages are recovered onto the session timeline", async (t) => {
	const h = setup(t);
	h.ctx.sessionManager.appendCustomEntry("live-tps.graph", {
		version: 1,
		samples: [{ seconds: 1, rate: 20 }, { seconds: 2, rate: 30 }],
		latestRate: "35.0 TPS", responseState: "Complete",
	});
	const entry = h.ctx.sessionManager.getBranch()[0];
	entry.timestamp = new Date(EPOCH + 12_000).toISOString();
	h.advance(15_000);
	h.reload();
	const opened = h.openGraph();
	const lines = h.graph().render(100);
	assert.match(lines.join("\n"), /Full session/);
	assert.match(lines.join("\n"), /35.0 TPS/);
	assert.match(lines.join("\n"), /15.0s/);
	assert.match(lines.join("\n"), /40 [┤┼]/);
	h.graph().handleInput("escape");
	await opened;
});

test("history survives disk resume and compaction and follows the active branch", async (t) => {
	const packageRoot = process.env.PI_PACKAGE_ROOT ?? join(
		execFileSync("npm", ["root", "-g"], { encoding: "utf8" }).trim(), "@earendil-works/pi-coding-agent",
	);
	const { SessionManager } = await import(pathToFileURL(join(packageRoot, "dist/core/session-manager.js")));
	const directory = mkdtempSync(join(tmpdir(), "pi-live-tps-"));
	const manager = SessionManager.create(process.cwd(), directory);
	manager.getHeader().timestamp = new Date(EPOCH).toISOString();
	manager.appendMessage({ role: "user", content: "hello", timestamp: EPOCH });
	manager.appendMessage({
		role: "assistant", content: [{ type: "text", text: "hello" }],
		api: "openai-responses", provider: "openai", model: "test", stopReason: "stop", timestamp: EPOCH,
		usage: { input: 0, output: 0, cacheRead: 0, cacheWrite: 0, totalTokens: 0,
			cost: { input: 0, output: 0, cacheRead: 0, cacheWrite: 0, total: 0 } },
	});
	const h = setup(t, true, manager);
	t.after(() => rmSync(directory, { recursive: true, force: true }));
	h.start();
	h.delta("text_delta", "a".repeat(40));
	h.advance(1000);
	h.end(10);
	const firstGraphLeaf = manager.getLeafId();
	h.advance(2000);
	h.start();
	h.delta("text_delta", "b".repeat(80));
	h.advance(1000);
	h.end(20);
	assert.equal(h.history().length, 2);
	manager.branch(firstGraphLeaf);
	h.emit("session_tree");
	assert.equal(h.status(), "10.0 TPS");
	assert.equal(h.history().length, 1);
	manager.appendCompaction("summary", firstGraphLeaf, 100);
	h.ctx.sessionManager = SessionManager.open(manager.getSessionFile());
	h.reload();
	assert.equal(h.status(), "10.0 TPS");
	assert.equal(h.history().length, 1);
	const opened = h.openGraph();
	assert.match(h.graph().render(80).join("\n"), /Full session/);
	assert.match(h.graph().render(80).join("\n"), /4.0s/);
	h.graph().handleInput("escape");
	await opened;
	h.ctx.sessionManager = SessionManager.inMemory();
	h.ctx.sessionManager.getHeader().timestamp = new Date(EPOCH + 4000).toISOString();
	h.reload();
	assert.equal(h.status(), undefined);
	assert.deepEqual(h.history(), []);
});

test("graph handles empty, single, flat, zero, and changing data within its width", () => {
	const histories = [
		[],
		[{ seconds: 1, rate: 20 }],
		[{ seconds: 1, rate: 0 }, { seconds: 2, rate: 0 }],
		[{ seconds: 1, rate: 20 }, { seconds: 2, rate: 20 }],
		[{ seconds: 1, rate: 10 }, { seconds: 2, rate: 80 }, { seconds: 3, rate: 5 }],
	];
	for (const samples of histories) {
		for (const width of [0, 1, 12, 16, 40, 80]) {
			const lines = renderTpsGraph(samples, width, 8);
			assert.ok(lines.every((line) => line.length <= width));
			assert.doesNotMatch(lines.join("\n"), /NaN|Infinity/);
		}
	}
});

test("time axis includes aligned intermediate ticks", () => {
	const lines = renderTpsGraph([{ seconds: 0.25, rate: 20 }, { seconds: 20, rate: 40 }], 80, 8);
	const axis = lines.at(-2);
	const labels = lines.at(-1);
	assert.deepEqual(labels.trim().split(/\s+/), ["0.3s", "5s", "10s", "15s", "20.0s"]);
	const plotStart = axis.indexOf("└") + 1;
	const plotWidth = axis.length - plotStart;
	for (const label of ["5s", "10s", "15s"]) {
		const column = plotStart + Math.round((Number.parseFloat(label) - 0.25) / 19.75 * (plotWidth - 1));
		assert.equal(axis[column], "┬");
		assert.equal(labels.indexOf(label) + Math.floor(label.length / 2), column);
	}
});

test("time ticks adapt to fractional durations, narrow widths, and retained history offsets", () => {
	for (const [start, end] of [[0.25, 2.25], [125.25, 425.25], [0.25, 3600]]) {
		for (const width of [16, 20, 40, 80]) {
			const lines = renderTpsGraph([{ seconds: start, rate: 20 }, { seconds: end, rate: 40 }], width, 8);
			const labels = lines.at(-1).trim();
			assert.ok(lines.every((line) => line.length <= width));
			assert.ok(labels.startsWith(`${start.toFixed(1)}s`));
			const ticks = labels.split(/\s+/).map(Number.parseFloat);
			assert.ok(ticks.every((seconds, index) => index === 0 || seconds > ticks[index - 1]));
			assert.ok(labels.split(/\d+(?:\.\d+)?s/).slice(1, -1).every((gap) => gap.length >= 2));
		}
	}
});

test("a single sample has one time label rather than duplicate endpoints", () => {
	const lines = renderTpsGraph([{ seconds: 1, rate: 20 }], 80, 8);
	assert.equal(lines.at(-1).trim(), "1.0s");
	assert.equal(lines.at(-2).match(/┬/g).length, 1);
});
