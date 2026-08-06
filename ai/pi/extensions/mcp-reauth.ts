import type { ExtensionAPI, ExtensionCommandContext } from "@earendil-works/pi-coding-agent";
import { loadMcpConfig } from "/opt/homebrew/lib/node_modules/pi-mcp-adapter/config.ts";
import { authenticate, removeAuth, supportsOAuth } from "/opt/homebrew/lib/node_modules/pi-mcp-adapter/mcp-auth-flow.ts";
import type { McpConfig } from "/opt/homebrew/lib/node_modules/pi-mcp-adapter/types.ts";

const oauthServerNames = (config: McpConfig): string[] =>
	Object.entries(config.mcpServers)
		.filter(([, definition]) => supportsOAuth(definition))
		.map(([name]) => name)
		.sort();

const selectServer = async (
	requestedServer: string,
	config: McpConfig,
	ctx: ExtensionCommandContext,
): Promise<string | undefined> => {
	const serverNames = oauthServerNames(config);

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

export default function (pi: ExtensionAPI) {
	pi.registerCommand("mcp-reauth", {
		description: "Reset and reauthenticate an OAuth MCP server",
		getArgumentCompletions: (prefix) => {
			const config = loadMcpConfig(undefined, process.cwd());
			const matches = oauthServerNames(config)
				.filter((name) => name.startsWith(prefix))
				.map((name) => ({ value: name, label: name }));
			return matches.length > 0 ? matches : null;
		},
		handler: async (args, ctx) => {
			if (!ctx.hasUI) {
				return;
			}

			const config = loadMcpConfig(undefined, ctx.cwd);
			const serverName = await selectServer(args.trim(), config, ctx);
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
