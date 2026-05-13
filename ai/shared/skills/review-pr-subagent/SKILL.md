---
name: review-pr-subagent
description: Review a GitHub pull request by delegating PR, ticket, and diff exploration to a reviewer subagent
---

## What This Skill Does

Reviews a GitHub PR using a focused `reviewer` subagent. The parent agent should not pre-gather the PR context. Instead, delegate a self-contained investigation task and have the subagent pull PR metadata, associated ticket context, diff context, and relevant code itself.

## Workflow

1. Identify the PR target.
   - If the user provided a PR number or URL, pass it through.
   - Otherwise, tell the reviewer subagent to discover the PR for the current branch with `gh pr view`.

2. Delegate directly to the `reviewer` subagent.
   - Use fresh context by default.
   - Do not run `gh pr view`, `gh pr diff`, Jira searches, or local diff commands in the parent unless needed to resolve ambiguity.
   - The subagent should do the exploration itself using `gh`, `git`, available MCP/Jira tools, and local file reads.

3. Ask the reviewer subagent to return only review feedback.
   - The reviewer should not edit files.
   - The reviewer should include enough evidence to make findings actionable.

4. Summarize the subagent output for the user.
   - Preserve blocking findings and important suggestions.
   - If there are no substantive issues, say so directly.

## Suggested Subagent Task

Use this as the task text when delegating:

```text
Review this GitHub pull request. Do all exploration yourself and return review feedback only.

PR target: <PR number/URL, or "current branch PR">

You are responsible for gathering context:
- Use `gh pr view` to get PR number, title, body, author, base branch, head branch, URL, commits, changed files, and review status.
- Use `gh pr diff` and/or local `git diff <base>...HEAD` to inspect the changes.
- Fetch the base branch if needed.
- Inspect relevant files in the repository, not just the patch, when surrounding context matters.
- Look for an associated ticket or issue from the PR title, body, branch name, commit messages, and linked references.
- If the ticket is GitHub, use `gh issue view` or related `gh` commands.
- If the ticket is Jira, use available Jira/MCP tools or accessible CLI/context to read the ticket title, description, comments, status, and acceptance criteria.
- If another ticket system is referenced, use the available tools or clearly state what could not be accessed.

Review goals:
- Check whether the implementation matches the PR description and associated ticket requirements.
- Look for correctness bugs, edge cases, missing or weak tests, risky behavior, regressions, and unnecessary complexity.
- Prefer actionable findings with file paths and line references when possible.
- Separate blocking issues from non-blocking suggestions.
- Do not nitpick style unless it affects readability or maintainability.
- Do not edit files.

Output format:
- Start with a one-sentence verdict.
- Then list findings grouped by severity.
- For each finding, include evidence and a suggested fix.
- If there are no substantive findings, say that clearly and mention any residual risks from inaccessible context.
```

## When to Use This Skill

Load this skill when the user asks to review a PR, review the current branch's PR, run a PR review with a subagent, or get review feedback for a GitHub pull request.
