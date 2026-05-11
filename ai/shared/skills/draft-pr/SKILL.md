---
name: draft-pr
description: Open a draft pull request using the gh CLI with semantic commit title and linked issue
---

## What This Skill Does

Opens a draft pull request on GitHub using the `gh` CLI. Ensures the PR title follows conventional/semantic commit format and the body references the associated issue.

## Workflow

1. **Check git state**: Run `git status` and `git log` to understand what's on the current branch vs the base branch.
2. **Ensure changes are pushed**: If the branch has unpushed commits, push with `git push -u origin <branch>`.
3. **Find the associated issue**: Look for an issue that matches the work on this branch.
   - Search upstream GitHub issues with `gh issue list` and `gh issue search` using keywords from the branch name or commit messages.
   - Look for Jira issue keys in the branch name or commit messages, such as `QSEM-123`.
   - Present the candidate issue to the user and ask for confirmation before proceeding.
   - If no matching issue is found, inform the user and proceed without an issue reference.
4. **Draft the PR title**: Use semantic commit format matching the project's conventions (see Style section below). If the associated issue is a Jira issue, prefix the title with the issue key like `[QSEM-123] fix: handle invalid input`.
5. **Draft the PR body**: Keep it concise and focus on why the change is needed (see Style section below). Include `Closes #<number>` if there is an associated GitHub issue.
6. **Create the PR**: Run `gh pr create --draft` with the title and body.
7. **Return the PR URL** to the user.

## PR Style

The PR title and body should match the style of existing PRs by this author. Here are the conventions:

### Title

Semantic commit format:
- `feat: <description>` or `feat(<scope>): <description>` for new features
- `fix: <description>` or `fix(<scope>): <description>` for bug fixes
- `refactor:`, `docs:`, `chore:`, `test:`, etc. as appropriate

Scope is typically a module name like `core`, `isthmus`, `spark`.

If the associated issue is a Jira issue, prefix the semantic title with the Jira key in square brackets:
- `[QSEM-123] feat(core): expose metadata from YAML extension files`
- `[QSEM-123] fix: handle invalid input`

Do not add a Jira prefix for GitHub issues.

### Body

Write prose, not templates. Do NOT use section headers like `## Summary`, `## Test plan`, or `## Changes`. Do NOT use bullet lists unless they genuinely add clarity. Do NOT use em dashes.

Assume reviewers know the repository well, but do not know the specific issue or failure mode this branch addresses. The body should primarily explain why the change is needed: what problem was encountered, what made the old behavior insufficient or incorrect, and what outcome the PR is intended to produce.

Only describe what changed when it helps explain the why. Mention implementation details if they are surprising, non-obvious, or important for reviewers to evaluate the approach. Avoid restating the diff or listing routine code changes.

Multiple short paragraphs are fine. Typical patterns:
- Just the issue reference: `Closes #123`
- A one-sentence problem-focused summary followed by `Closes #123`
- A few paragraphs explaining the issue, why it happens, and any non-obvious approach taken to fix it, followed by the issue reference

Think of the PR body like a good commit message: explain the "why" naturally. Don't pad it with structure for structure's sake.

### AI Acknowledgment

Always end the PR body with the following acknowledgment, separated by a horizontal rule:

```
---
Note: This PR was developed with AI assistance. All changes have been reviewed, and I take full responsibility for this contribution.
```

## Example PRs for Reference

```
Title: feat(core): expose metadata from YAML extension files
Body:  Extension metadata in YAML files was not available to callers, which meant
       downstream tooling had to either duplicate parsing logic or ignore that
       information entirely.

       Closes #690
```

```
Title: fix(core): enforce NullLiteral nullable invariant
Body:  A null literal is inherently nullable, but callers could construct one
       with a non-nullable type and get inconsistent nullability answers later.
       Validating the invariant at construction time keeps invalid plans from
       moving further through the system.

       Closes #685
```

```
Title: fix: namespace -> urn
Body:  The spec identifies extensions by `urn`, but these paths were still using
       `namespace`, which made the generated metadata disagree with the expected
       schema.

       Closes #583
```

## When to Use This Skill

Load this skill when:
- The user asks to open or create a pull request
- The user asks to open a draft PR
- The user wants to submit their work for review
