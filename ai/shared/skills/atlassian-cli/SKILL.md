---
name: atlassian-cli
description: Use the Atlassian CLI (`acli`) for Jira and supported Confluence tasks, falling back to the Atlassian MCP for unsupported Confluence operations. Load whenever a task involves Jira issues, boards, sprints, or Confluence content.
---

# Atlassian CLI

Use `acli` for Jira operations and for Confluence operations it supports. Use the Atlassian MCP when the installed CLI lacks a required Confluence capability, such as search or content updates.

Discover the installed CLI's commands and flags before acting. Traverse the help tree rather than guessing syntax:

```bash
acli --help
acli jira --help
acli jira <command> --help
acli confluence --help
acli confluence <command> --help
```

Check authentication with `acli jira auth status` or `acli confluence auth status`. If authentication is missing or expired, ask the user to authenticate with the corresponding `auth login` command.

Prefer structured output when the selected command supports it. Confirm with the user before destructive or difficult-to-reverse changes unless the surrounding workflow explicitly authorizes them.
