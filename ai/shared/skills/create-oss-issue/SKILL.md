---
name: create-oss-issue
description: Draft and create concise issues in the appropriate open-source GitHub repository. Use when reporting an upstream bug, API gap, or feature request. Searches for duplicates, cites relevant source with commit permalinks, requires explicit approval before creation, creates as the benbellick GitHub account, and restores the previously active account.
---

# Create an OSS Issue

## Non-negotiable rules

- Never create an issue until the user has reviewed the complete draft and explicitly approved creating that exact draft.
- An initial request to report a problem is not approval to create the issue.
- If the draft changes after approval, show the complete revised draft and ask for approval again.
- Create issues using the `benbellick` GitHub account, never `ben-bellick_ddog`.
- Restore the previously active GitHub account after the creation attempt, including when creation fails.
- Do not expose Datadog-internal code, links, names, credentials, customer information, or other non-public context.

## Workflow

1. Identify the appropriate upstream repository. Confirm the repository with the user if ownership is ambiguous.
2. Search both open and closed issues for the behavior, API, error text, and closely related terminology.
3. If any issue appears potentially overlapping, show its title, state, short relevance summary, and URL. Stop and let the user decide whether to reuse it or continue with a new issue.
4. Inspect the current upstream implementation and issue guidance. Gather only the evidence needed to explain the problem.
5. Use immutable GitHub source permalinks pinned to a commit SHA when calling attention to code. Link relevant words naturally as `[the variant API](https://github.com/owner/repo/blob/<sha>/path/file.go#L10-L20)`.
6. Draft the repository, title, and complete body. Do not create anything yet.
7. Ask for explicit approval to create that exact draft.
8. Only after approval, create the issue as `benbellick`, restore the previous account, and return the issue URL.

## Duplicate search

Use several focused searches rather than one exact phrase. Search all states:

```bash
gh issue list --repo OWNER/REPO --state all --search "KEYWORDS" --limit 30
```

Also search alternate API names, error messages, and the underlying concept. Do not dismiss a candidate based only on its title. Read plausible matches before presenting them.

## Draft style

Keep the issue as short as possible while still making the problem understandable and actionable.

- Prefer one to three short paragraphs.
- Include a small code or YAML example only when it makes the issue clearer.
- Do not add boilerplate sections such as Summary, Motivation, Validation, Expected behavior, or Additional context.
- Do not append test plans, implementation checklists, AI acknowledgments, or unrelated background.
- Do not prescribe a specific API if multiple designs are reasonable. A compact possible signature is fine when it clarifies the request.
- Embed important source links directly in the prose using immutable commit permalinks.

Present drafts in this form:

```markdown
Repository: OWNER/REPO
Title: Concise issue title

Body:
Complete issue body
```

Then ask whether to revise it or create it. Do not run `gh issue create` in the same turn that first presents the draft.

## Account-safe creation

Write the approved body to a temporary file. Perform account capture, switch, verification, creation, and restoration in one shell command so restoration is protected by a trap:

```bash
set -euo pipefail
previous_account="$(gh api user --jq .login)"
restore_account() {
  gh auth switch --hostname github.com --user "$previous_account" >/dev/null 2>&1 || true
}
trap restore_account EXIT

gh auth switch --hostname github.com --user benbellick
active_account="$(gh api user --jq .login)"
if [[ "$active_account" != "benbellick" ]]; then
  echo "expected benbellick, got $active_account" >&2
  exit 1
fi

gh issue create \
  --repo OWNER/REPO \
  --title "APPROVED TITLE" \
  --body-file /tmp/approved-oss-issue.md
```

If `benbellick` is not configured, stop and ask the user to authenticate it. Never bypass authentication safeguards or print tokens.
