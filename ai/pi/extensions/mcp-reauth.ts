import type { ExtensionAPI, ExtensionCommandContext } from "@earendil-works/pi-coding-agent";
import { realpathSync } from "node:fs";
import { createRequire } from "node:module";
import { dirname, join } from "node:path";
import { pathToFileURL } from "node:url";

type McpServerDefinition = {
	url?: string;
	[key: string]: unknown;
};

type McpConfig = {
	mcpServers: Record<string, McpServerDefinition>;
};

type McpAdapter = {
	loadMcpConfig(configPath?: string, cwd?: string): McpConfig;
	authenticate(
		serverName: string,
		url: string,
		definition: McpServerDefinition,
	): Promise<string>;
	removeAuth(serverName: string): Promise<void>;
	supportsOAuth(definition: McpServerDefinition): boolean;
};

const loadMcpAdapter = async (): Promise<McpAdapter> => {
	const require = createRequire(import.meta.url);
	const agentDirectory = process.env.PI_CODING_AGENT_DIR ?? join(process.env.HOME ?? "", ".pi", "agent");
	const resolutionPaths = [join(agentDirectory, "npm")];
	const cliPath = process.argv[1] ? realpathSync(process.argv[1]) : undefined;
	const globalNodeModules = cliPath?.match(/^(.*[\\/]node_modules)[\\/]/)?.[1];
	if (globalNodeModules) {
		resolutionPaths.push(dirname(globalNodeModules));
	}
	const configPath = require.resolve("pi-mcp-adapter/config.ts", { paths: resolutionPaths });
	const authPath = require.resolve("pi-mcp-adapter/mcp-auth-flow.ts", { paths: resolutionPaths });
	const [configModule, authModule] = await Promise.all([
		import(pathToFileURL(configPath).href),
		import(pathToFileURL(authPath).href),
	]);

	return {
		loadMcpConfig: configModule.loadMcpConfig,
		authenticate: authModule.authenticate,
		removeAuth: authModule.removeAuth,
		supportsOAuth: authModule.supportsOAuth,
	};
};

const oauthServerNames = (
	config: McpConfig,
	supportsOAuth: McpAdapter["supportsOAuth"],
): string[] =>
	Object.entries(config.mcpServers)
		.filter(([, definition]) => supportsOAuth(definition))
		.map(([name]) => name)
		.sort();

const selectServer = async (
	requestedServer: string,
	config: McpConfig,
	supportsOAuth: McpAdapter["supportsOAuth"],
	ctx: ExtensionCommandContext,
): Promise<string | undefined> => {
	const serverNames = oauthServerNames(config, supportsOAuth);

	if (requestedServer) {
		if (!config.mcpServers[requestedServer]) {
			ctx.ui.notify(`MCP server "${requestedServer}" is not configured.`, "error");
			return undefined;
		}
		if (!serverNames.includes(requestedServer)) {
			ctx.ui.notify(`MCP server "${requestedServer}" does not use OAuth.`, "error");
			return undefined;
		}
		return requestedServer;
	}

	if (serverNames.length === 0) {
		ctx.ui.notify("No OAuth-capable MCP servers are configured.", "warning");
		return undefined;
	}

	return ctx.ui.select("Reset authentication for which MCP server?", serverNames);
};

export default async function (pi: ExtensionAPI) {
	const { authenticate, loadMcpConfig, removeAuth, supportsOAuth } = await loadMcpAdapter();

	pi.registerCommand("mcp-reauth", {
		description: "Reset and reauthenticate an OAuth MCP server",
		getArgumentCompletions: (prefix) => {
			const config = loadMcpConfig(undefined, process.cwd());
			const matches = oauthServerNames(config, supportsOAuth)
				.filter((name) => name.startsWith(prefix))
				.map((name) => ({ value: name, label: name }));
			return matches.length > 0 ? matches : null;
		},
		handler: async (args, ctx) => {
			if (!ctx.hasUI) {
				return;
			}

			const config = loadMcpConfig(undefined, ctx.cwd);
			const serverName = await selectServer(args.trim(), config, supportsOAuth, ctx);
			if (!serverName) {
				return;
			}

			const definition = config.mcpServers[serverName];
			if (!definition?.url) {
				ctx.ui.notify(`MCP server "${serverName}" has no HTTP URL.`, "error");
				return;
			}

			ctx.ui.setStatus("mcp-reauth", `Resetting ${serverName} authentication...`);
			try {
				await removeAuth(serverName);
				const status = await authenticate(serverName, definition.url, definition);
				if (status !== "authenticated") {
					ctx.ui.notify(`Authentication failed for "${serverName}".`, "error");
					return;
				}
				ctx.ui.notify(`Authentication reset for "${serverName}". Reloading MCP connections...`, "info");
			} catch (error) {
				const message = error instanceof Error ? error.message : String(error);
				ctx.ui.notify(`Failed to reset "${serverName}" authentication: ${message}`, "error");
			} finally {
				ctx.ui.setStatus("mcp-reauth", undefined);
				await ctx.reload();
			}
		},
	});
}
