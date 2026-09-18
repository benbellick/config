---
name: review-pr-subagent
description: Review a GitHub pull request, especially an open-source contribution, by delegating context gathering and checking correctness, API documentation, simplicity, comments, tests, and scope.
---

# Review a GitHub PR

## Workflow

1. Resolve the target:
   - A bare number means that PR in the current repository.
   - A URL identifies its own repository and PR.
   - With no target, use the PR for the current branch.
2. Delegate the review to the `reviewer` subagent with fresh context and the task below. Do not gather context in the parent unless the target is ambiguous.
3. Return a concise summary that preserves every substantive finding.

Never edit files or modify GitHub state. Do not post comments or reviews, approve, request changes, merge, or push.

## Reviewer Task

```text
Review this GitHub pull request. Gather all context yourself and return review feedback only.

PR target: <number, URL, or current branch PR>

A bare number refers to the current repository. A URL identifies its own repository. With no target, discover the current branch's PR.

Investigation:
- Run `gh pr checkout <pr-num>` before reviewing so the local worktree contains the PR's code. Never discard or overwrite existing local changes to do this.
- Use read-only `gh` and `git` commands to inspect PR metadata, description, commits, changed files, diff, base branch, and review status.
- Inspect surrounding repository code when the patch alone is insufficient.
- Find and read linked issues or tickets when accessible.
- Check that the implementation matches the PR description and linked requirements.

Review standards:
- Find correctness bugs, regressions, risky behavior, and missed edge cases.
- Every public function and method should have a short, readable docstring explaining intention rather than implementation.
- Code should be as simple as reasonably possible. Flag unnecessary complexity; accept essential complexity only when its purpose or justification is clear.
- Non-obvious logic should have concise explanatory comments. Straightforward code should not be narrated with comments.
- Tests should cover changed behavior, important edge cases, and regressions while remaining easy to understand. Suggest a clearer structure for hard-to-read tests, or explain why their complexity is unavoidable.
- The PR should contain one coherent change. Flag unrelated refactors, cleanup, or behavior changes that should be separate.
- Treat maintainability concerns as substantive, but do not nitpick personal style.

Safety:
- Do not edit files.
- Use GitHub only for read-only investigation. Never post comments or reviews, approve, request changes, merge, push, or otherwise modify GitHub state.

Output:
- Start with a one-sentence verdict.
- Group findings by severity.
- Give each finding actionable evidence with file and line references when possible, plus the smallest reasonable fix.
- If there are no substantive findings, say so and mention any residual risk from inaccessible context.
```
