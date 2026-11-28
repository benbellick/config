import type { ExtensionAPI, ExtensionContext } from "@earendil-works/pi-coding-agent";
import asciichart from "asciichart";

const STATUS_KEY = "live-tps";
const UPDATE_INTERVAL_MS = 250;
const CHARACTERS_PER_TOKEN = 4;
const GRAPH_ENTRY_TYPE = "live-tps.graph";

type TpsSample = { seconds: number; rate: number };
type TpsInterval = { start: number; end: number; tokens: number };

function niceTimeStep(minimum: number): number {
	const magnitude = 10 ** Math.floor(Math.log10(minimum));
	const roundingTolerance = minimum * Number.EPSILON * 8;
	return [1, 2, 5, 10].find((multiple) => multiple * magnitude >= minimum - roundingTolerance)! * magnitude;
}

/** Uses session-aligned time buckets, clipping edge buckets to the visible window. */
export function sampleTpsHistory(intervals: TpsInterval[], start: number, end: number, maximumSamples: number): TpsSample[] {
	if (end <= start) return [{ seconds: start, rate: 0 }];
	const bucketSeconds = niceTimeStep(Math.max(1, (end - start) / Math.max(1, maximumSamples)));
	const boundaries = [start];
	for (let tick = Math.floor(start / bucketSeconds) + 1; tick * bucketSeconds < end; tick++) {
		const seconds = tick * bucketSeconds;
		if (seconds > start) boundaries.push(seconds);
	}
	boundaries.push(end);
	const tokenCounts = Array<number>(boundaries.length - 1).fill(0);

	for (const interval of intervals) {
		const from = Math.max(start, interval.start);
		const to = Math.min(end, interval.end);
		if (to <= from) continue;
		let bucket = 0;
		let upperBound = tokenCounts.length;
		while (bucket + 1 < upperBound) {
			const middle = Math.floor((bucket + upperBound) / 2);
			if (boundaries[middle] <= from) bucket = middle;
			else upperBound = middle;
		}
		for (; bucket < tokenCounts.length && boundaries[bucket] < to; bucket++) {
			const overlap = Math.min(to, boundaries[bucket + 1]) - Math.max(from, boundaries[bucket]);
			tokenCounts[bucket] += interval.tokens * overlap / (interval.end - interval.start);
		}
	}
	const samples = tokenCounts.map((tokens, index) => ({
		seconds: boundaries[index],
		rate: tokens / (boundaries[index + 1] - boundaries[index]),
	}));
	samples.push({ seconds: end, rate: samples[samples.length - 1].rate });
	return samples;
}

type SavedTpsGraph = {
	version: 2;
	intervals: TpsInterval[];
	latestRate?: string;
	responseState: string;
};
type LegacyTpsGraph = {
	version: 1;
	samples: TpsSample[];
	latestRate?: string;
	responseState: string;
};

/** Fits an asciichart plot and time labels within the overlay's available columns. */
export function renderTpsGraph(samples: TpsSample[], width: number, height: number): string[] {
	if (samples.length === 0) return ["No samples yet.".slice(0, width)];
	if (width < 16) return ["Widen terminal.".slice(0, width)];

	const rates = samples.map((sample) => Number.isFinite(sample.rate) ? Math.max(0, sample.rate) : 0);
	const maximumRate = Math.max(1, Math.ceil(Math.max(...rates)));
	const labelWidth = Math.max(3, String(maximumRate).length);
	const formatWidth = labelWidth + 1;
	const plotWidth = Math.max(1, width - formatWidth - 2);
	const first = samples[0];
	const last = samples[samples.length - 1];
	let sampleIndex = 0;
	const plottedRates = Array.from({ length: plotWidth }, (_value, column) => {
		const seconds = first.seconds + (last.seconds - first.seconds) * column / Math.max(1, plotWidth - 1);
		while (sampleIndex + 1 < samples.length && samples[sampleIndex + 1].seconds <= seconds) sampleIndex++;
		return rates[sampleIndex];
	});
	const lines = asciichart.plot([plottedRates[0], ...plottedRates], {
		min: 0,
		max: maximumRate,
		height: Math.max(2, height) - 1,
		offset: 2,
		format: (value: number) => `${value.toFixed(0).padStart(labelWidth)} `,
	}).split("\n").map((line: string) => line.trimEnd());
	const axis = Array<string>(plotWidth).fill("─");
	const timeLabels = Array<string>(plotWidth).fill(" ");
	const labelRanges: { start: number; end: number }[] = [];
	const duration = last.seconds - first.seconds;
	const addTick = (seconds: number, label: string) => {
		if (label.length > plotWidth) return;
		const column = duration > 0 ? Math.round((seconds - first.seconds) / duration * (plotWidth - 1)) : 0;
		const start = Math.max(0, Math.min(plotWidth - label.length, column - Math.floor(label.length / 2)));
		const end = start + label.length;
		if (labelRanges.some((range) => start < range.end + 2 && end > range.start - 2)) return;
		labelRanges.push({ start, end });
		axis[column] = "┬";
		for (let index = 0; index < label.length; index++) timeLabels[start + index] = label[index];
	};

	addTick(first.seconds, `${first.seconds.toFixed(1)}s`);
	if (duration > 0) {
		addTick(last.seconds, `${last.seconds.toFixed(1)}s`);
		const targetStep = Math.max(0.1, duration / Math.max(1, Math.floor(plotWidth / 12)));
		const step = niceTimeStep(targetStep);
		for (let seconds = Math.ceil(first.seconds / step) * step; seconds < last.seconds; seconds += step) {
			if (seconds > first.seconds) addTick(seconds, `${seconds.toFixed(step < 1 ? 1 : 0)}s`);
		}
	}
	lines.push(`${" ".repeat(formatWidth)}└${axis.join("")}`);
	lines.push(`${" ".repeat(formatWidth + 1)}${timeLabels.join("")}`);
	return lines;
}

/** Tracks session-wide output TPS while retaining the response average in the footer. */
export default function (pi: ExtensionAPI) {
	let sessionStartedAt = Date.now();
	let clockEpoch = Date.now();
	let clockStartedAt = performance.now();
	let startedAt: number | undefined;
	let outputCharacters = 0;
	let recordedTokens = 0;
	let lastIntervalAt: number | undefined;
	let timer: ReturnType<typeof setInterval> | undefined;
	let isStreaming = false;
	let history: TpsInterval[] = [];
	let responseIntervals: TpsInterval[] = [];
	let latestRate: string | undefined;
	let responseState = "No response yet";
	let view: "full" | "recent" = "full";
	let redrawGraph: (() => void) | undefined;
	let closeGraph: (() => void) | undefined;

	const sessionSeconds = () => Math.max(0,
		(clockEpoch + performance.now() - clockStartedAt - sessionStartedAt) / 1000,
	);

	const stopTimer = () => {
		if (timer !== undefined) clearInterval(timer);
		timer = undefined;
	};

	const resetResponse = () => {
		stopTimer();
		startedAt = undefined;
		outputCharacters = 0;
		recordedTokens = 0;
		lastIntervalAt = undefined;
		isStreaming = false;
		responseIntervals = [];
		latestRate = undefined;
		responseState = "No response yet";
	};

	const saveGraph = () => {
		pi.appendEntry(GRAPH_ENTRY_TYPE, {
			version: 2,
			intervals: responseIntervals.map((interval) => ({ ...interval })),
			latestRate,
			responseState,
		} satisfies SavedTpsGraph);
	};

	const restoreGraph = (ctx: ExtensionContext) => {
		resetResponse();
		history = [];
		clockEpoch = Date.now();
		clockStartedAt = performance.now();
		sessionStartedAt = Date.parse(ctx.sessionManager.getHeader()?.timestamp ?? "") || clockEpoch;
		for (const entry of ctx.sessionManager.getBranch()) {
			if (entry.type !== "custom" || entry.customType !== GRAPH_ENTRY_TYPE) continue;
			const saved = entry.data as SavedTpsGraph | LegacyTpsGraph | undefined;
			if (saved?.version === 2) {
				history.push(...saved.intervals.map((interval) => ({ ...interval })));
			} else if (saved?.version === 1) {
				// Older graphs stored cumulative averages relative to each response.
				const last = saved.samples[saved.samples.length - 1];
				const responseEnd = (Date.parse(entry.timestamp) - sessionStartedAt) / 1000;
				if (last && Number.isFinite(responseEnd)) {
					const responseStart = Math.max(0, responseEnd - last.seconds);
					let previousSeconds = 0;
					let previousTokens = 0;
					for (const sample of saved.samples) {
						const tokens = sample.rate * sample.seconds;
						if (sample.seconds > previousSeconds && tokens > previousTokens) {
							history.push({
								start: responseStart + previousSeconds,
								end: responseStart + sample.seconds,
								tokens: tokens - previousTokens,
							});
						}
						previousSeconds = sample.seconds;
						previousTokens = tokens;
					}
				}
			} else {
				continue;
			}
			latestRate = saved.latestRate;
			responseState = saved.responseState;
		}
		if (ctx.hasUI) ctx.ui.setStatus(STATUS_KEY, responseState === "Complete" ? latestRate : undefined);
		redrawGraph?.();
	};

	const showResponseAverage = (ctx: ExtensionContext, tokens: number, isEstimate: boolean) => {
		if (startedAt === undefined) return;
		const elapsedSeconds = (performance.now() - startedAt) / 1000;
		if (elapsedSeconds < UPDATE_INTERVAL_MS / 1000) return;
		const rate = tokens / elapsedSeconds;
		latestRate = `${isEstimate ? "~" : ""}${rate.toFixed(1)} TPS`;
		ctx.ui.setStatus(STATUS_KEY, latestRate);
	};

	const sampleResponse = (ctx: ExtensionContext) => {
		const end = sessionSeconds();
		if (lastIntervalAt === undefined || end <= lastIntervalAt) return;
		const totalTokens = outputCharacters / CHARACTERS_PER_TOKEN;
		const tokens = totalTokens - recordedTokens;
		if (tokens > 0) {
			const interval = { start: lastIntervalAt, end, tokens };
			history.push(interval);
			responseIntervals.push(interval);
		}
		lastIntervalAt = end;
		recordedTokens = totalTokens;
		showResponseAverage(ctx, totalTokens, true);
		redrawGraph?.();
	};

	const finishResponse = (ctx: ExtensionContext, state: string, reportedTokens = 0) => {
		sampleResponse(ctx);
		stopTimer();
		isStreaming = false;
		responseState = state;
		ctx.ui.setStatus(STATUS_KEY, undefined);
		if (state === "Complete") {
			showResponseAverage(ctx, reportedTokens || outputCharacters / CHARACTERS_PER_TOKEN, !reportedTokens);
		}
		saveGraph();
		redrawGraph?.();
	};

	pi.registerCommand("tps", {
		description: "Graph session TPS; Tab toggles last 60 seconds and full history",
		handler: async (_args, ctx) => {
			if (ctx.mode !== "tui") {
				if (ctx.hasUI) ctx.ui.notify("/tps requires Pi's terminal interface.", "info");
				return;
			}
			if (closeGraph) return;
			let overlayTimer: ReturnType<typeof setInterval> | undefined;
			try {
				await ctx.ui.custom<void>((tui, theme, keybindings, done) => {
					redrawGraph = () => tui.requestRender();
					closeGraph = () => done();
					overlayTimer = setInterval(() => tui.requestRender(), UPDATE_INTERVAL_MS);
					overlayTimer.unref();
					return {
						render(width) {
							if (width < 4) return ["TPS".slice(0, width)];
							const innerWidth = width - 4;
							const availableRows = Math.max(2, Math.floor(tui.terminal.rows * 0.9));
							const height = Math.max(2, Math.min(10, availableRows - 12));
							const title = `TPS - ${view === "full" ? "Full session" : "Last 60s"}`;
							const summary = `${responseState}${latestRate ? ` | Response average ${latestRate}` : ""}`;
							const end = sessionSeconds();
							const start = view === "recent" ? Math.max(0, end - 60) : 0;
							const samples = sampleTpsHistory(history, start, end, Math.max(1, innerWidth - 8));
							const help = "Tab: 60s / full history | Esc: close";
							const lines = availableRows < 14 ? [
								title, summary, "Enlarge terminal to view graph.", help,
							].slice(0, availableRows - 2) : [
								"", title, summary, "",
								...renderTpsGraph(samples, innerWidth, height),
								"",
								"Estimated interval TPS; idle/tool gaps are zero.",
								help,
								"",
							];
							const border = (text: string) => theme.fg("border", text);
							return [
								border(`╭${"─".repeat(width - 2)}╮`),
								...lines.map((line) => border("│ ") +
									theme.fg(line === title ? "accent" : "dim", line.slice(0, innerWidth).padEnd(innerWidth)) + border(" │")),
								border(`╰${"─".repeat(width - 2)}╯`),
							];
						},
						handleInput(data) {
							if (keybindings.matches(data, "tui.select.cancel")) {
								done();
							} else if (keybindings.matches(data, "tui.input.tab")) {
								view = view === "full" ? "recent" : "full";
								tui.requestRender();
							}
						},
						invalidate() {},
					};
				}, { overlay: true, overlayOptions: { anchor: "center", width: "85%", maxHeight: "90%" } });
			} finally {
				if (overlayTimer !== undefined) clearInterval(overlayTimer);
				redrawGraph = undefined;
				closeGraph = undefined;
			}
		},
	});

	pi.on("message_start", (event, ctx) => {
		if (event.message.role !== "assistant" || !ctx.hasUI) return;
		resetResponse();
		isStreaming = true;
		responseState = "Waiting for output";
		ctx.ui.setStatus(STATUS_KEY, "TPS waiting…");
		redrawGraph?.();
	});

	pi.on("message_update", (event, ctx) => {
		if (!isStreaming || !ctx.hasUI) return;
		const update = event.assistantMessageEvent;
		if (
			update.type !== "text_delta" &&
			update.type !== "thinking_delta" &&
			update.type !== "toolcall_delta"
		) return;
		if (!update.delta) return;

		outputCharacters += update.delta.length;
		if (startedAt === undefined) {
			startedAt = performance.now();
			lastIntervalAt = sessionSeconds();
			responseState = "Streaming";
			ctx.ui.setStatus(STATUS_KEY, "TPS measuring…");
			redrawGraph?.();
			timer = setInterval(() => sampleResponse(ctx), UPDATE_INTERVAL_MS);
			timer.unref();
		}
	});

	pi.on("message_end", (event, ctx) => {
		if (event.message.role !== "assistant" || !isStreaming) return;
		const state = event.message.stopReason === "error" ? "Error" :
			event.message.stopReason === "aborted" ? "Aborted" : "Complete";
		finishResponse(ctx, state, event.message.usage.output);
	});

	pi.on("session_start", (_event, ctx) => restoreGraph(ctx));
	pi.on("session_tree", (_event, ctx) => restoreGraph(ctx));
	pi.on("model_select", (_event, ctx) => {
		if (isStreaming) finishResponse(ctx, "Interrupted");
	});
	pi.on("session_shutdown", (_event, ctx) => {
		closeGraph?.();
		if (isStreaming) finishResponse(ctx, "Interrupted");
		resetResponse();
		if (ctx.hasUI) ctx.ui.setStatus(STATUS_KEY, undefined);
	});
	pi.on("agent_settled", (_event, ctx) => {
		if (isStreaming) finishResponse(ctx, "Interrupted");
	});
}
