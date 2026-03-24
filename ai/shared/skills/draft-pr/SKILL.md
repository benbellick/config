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
   - Search upstream issues with `gh issue list` and `gh issue search` using keywords from the branch name or commit messages.
   - Present the candidate issue to the user and ask for confirmation before proceeding.
   - If no matching issue is found, inform the user and proceed without an issue reference.
4. **Draft the PR title**: Use semantic commit format matching the project's conventions (see Style section below).
5. **Draft the PR body**: Keep it concise (see Style section below). Include `Closes #<number>` if there is an associated issue.
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

### Body

Keep it short. Typical patterns:
- Just the issue reference: `Closes #123`
- A one-sentence summary followed by `Closes #123`
- A short paragraph of context plus a bullet list of key changes, followed by the issue reference

Do NOT write verbose AI-style descriptions with excessive headers, bullet points, or sections. Match the author's concise style.

### AI Acknowledgment

Always end the PR body with the following acknowledgment, separated by a horizontal rule:

```
---
Note: This PR was developed with AI assistance. All changes have been reviewed, and I take full responsibility for this contribution.
```

## Example PRs for Reference

```
Title: feat(core): expose metadata from YAML extension files
Body:  Closes #690
```

```
Title: fix(core): enforce NullLiteral nullable invariant
Body:  A null literal is inherently nullable so `NullLiteral.nullable()` now
       always returns true and construction validates the type is nullable.

       Closes #685
```

```
Title: fix: namespace -> urn
Body:  Change usages of `namespace` to correctly be `urn`.

       Closes #583
```

## When to Use This Skill

Load this skill when:
- The user asks to open or create a pull request
- The user asks to open a draft PR
- The user wants to submit their work for review
