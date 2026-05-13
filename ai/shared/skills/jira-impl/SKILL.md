---
name: jira-impl
description: Start implementation work from a Jira ticket: verify ownership, move the issue to In Progress, ensure it is in the current sprint, ask clarifying questions, branch from main unless told otherwise, implement, and ask for user approval before PR creation.
---

# Jira Implementation

Use this skill when the user asks to implement a Jira ticket, especially with `/skill:jira-impl <JIRA-KEY>`.

## Workflow

1. Identify the Jira issue.
   - Require a Jira issue key such as `QSEM-123`.
   - If the user did not provide one, ask for it before doing anything else.

2. Read the issue and confirm ownership.
   - Use the available Jira tools, preferably the Atlassian MCP tools:
     - `atlassian_atlassianUserInfo` to identify the current Jira user.
     - `atlassian_getJiraIssue` to read the ticket title, description, status, assignee, sprint fields, comments, linked issues, and acceptance criteria.
   - Confirm the issue is assigned to the current Jira user.
   - If it is not assigned to the current Jira user, stop and tell the user who it is assigned to. Ask whether they want it reassigned or whether to proceed anyway. Do not change status, sprint, git branches, or code until the user confirms.

3. Move the issue to In Progress.
   - If the issue is already in an in-progress status, leave it as-is.
   - Otherwise, use `atlassian_getTransitionsForJiraIssue` to find the transition whose target status is equivalent to `In Progress`.
   - Use `atlassian_transitionJiraIssue` to move it.
   - If no matching transition exists, report the available transitions and ask the user what status to use.

4. Ensure the issue is attached to the current sprint.
   - Inspect the Jira issue sprint/agile fields from the issue response.
   - If the issue is already in an active/current sprint, leave it as-is.
   - If it is not in the current sprint, try to determine the active sprint for the issue's project/board from available Jira fields, JQL, linked sprint metadata, or existing project conventions.
   - If there is exactly one clear active sprint, update the sprint field with `atlassian_editJiraIssue`.
   - If the sprint field id or active sprint is ambiguous, ask the user which sprint to use instead of guessing.

5. Gather implementation context.
   - Read the ticket carefully: description, acceptance criteria, comments, linked issues, and any linked PRs or docs.
   - Search the repository for the relevant code before planning changes.
   - Ask clarifying questions when necessary to understand the best implementation path or when extra context from the user could materially change the approach.
   - If no clarification is needed, say so briefly and proceed.

6. Create the implementation branch.
   - Unless the user explicitly told you to use another base branch or stay on the current branch, branch from `main`.
   - Before switching branches, check `git status`.
   - If there are uncommitted user changes, stop and ask how to handle them.
   - Run the equivalent of:
     - `git fetch origin main`
     - `git checkout main`
     - `git pull --ff-only origin main`
     - `git checkout -b <jira-key>-<short-description>`
   - Use a concise lowercase branch name, for example `qsem-123-handle-invalid-input`.

7. Implement the ticket.
   - Make the smallest coherent change that satisfies the Jira requirements.
   - Follow project style and existing patterns.
   - Add or update tests when the behavior is non-trivial or acceptance criteria require it.
   - Run focused tests first, then broader checks when appropriate.
   - Do not commit unless the user explicitly asks.
   - Do not open a PR from this skill.

8. Finish by asking for approval.
   - Summarize what changed and what validation was run.
   - Ask: `Are you happy with the results?`
   - If the user interrupted the work or asks to create a PR, switch to the `draft-pr` skill when appropriate.

## Important Constraints

- Do not proceed with implementation if the Jira ticket is not assigned to the current Jira user unless the user explicitly approves.
- Do not guess the current sprint when Jira data is ambiguous.
- Do not overwrite or discard local changes without explicit user approval.
- Do not create commits or PRs unless explicitly asked.
