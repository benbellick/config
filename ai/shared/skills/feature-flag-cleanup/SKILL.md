---
name: feature-flag-cleanup
description: Remove a rolled-out feature flag by keeping its enabled behavior and deleting the lookup, disabled branch, and obsolete tests. Use when asked to clean up or retire a feature flag, find an existing cleanup PR, or open a flag-removal PR. Check for equivalent PRs, establish rollout age without overstating evidence, and keep the PR body short.
---

# Feature flag cleanup

## Establish the intended change

Treat cleanup as deleting the gate and retaining the enabled behavior, not assigning the flag variable `true` or adding environment-specific defaults. Preserve unrelated conditions, session options, and precedence rules.

If the desired retained behavior is ambiguous, ask. If the user explicitly requests unconditional removal, do not substitute a commercial-only cleanup or retain Gov/staging exceptions. Call out any known behavior change in those environments without silently expanding or narrowing the request.

## Check for an existing PR

- Resolve the actual repository with `gh repo view --json nameWithOwner,defaultBranchRef`. Do not infer its GitHub owner from the local directory name.
- Search open PRs by exact flag name, constant name, and related feature terms. Inspect candidate bodies and diffs rather than relying on titles.
- A PR retaining environment gates or the disabled branch is not equivalent to unconditional removal. Explain that distinction briefly.
- If an equivalent PR exists, return its link. Otherwise proceed only within the user's authorization to implement, commit, push, and open a PR.

## Establish rollout age

Link to the flag's authoritative configuration page. For Datadog, follow the repository's Mosaic guidance for the correct environment URL.

Distinguish these dates rather than calling all of them “live”:

- Introduction or merge date.
- First observed enabled evaluation.
- Verified full rollout date.

Use flag history or evaluation metrics when available. Existing PR evidence is useful but must be attributed and bounded by its observation window. Successful true-only evaluations do not prove unconditional targeting or the absence of exclusions. Failed evaluations returning a false fallback are not enabled traffic.

Report the strongest supported timeline in plain language, such as “Enabled commercial traffic has been observed for roughly two weeks.” Never turn a merge date into an asserted full-rollout date. If the timeline cannot be established, say so rather than inventing one. Do not change live targeting or archive the flag as part of a code cleanup unless separately requested.

## Make the smallest cleanup

1. Read applicable repository instructions and locate all flag-string, constant, lookup, and test references.
2. Inspect Git state and use a clean branch without including unrelated work. Do not rewrite history or create worktrees without authorization.
3. Delete the lookup, flag variable, constant, disabled branch, and obsolete comments. Inline the enabled behavior directly; do not leave `if true`, a replacement boolean, or an environment helper.
4. Replace flag-state test matrices with focused behavior tests. Preserve coverage of meaningful precedence, unaffected dialects/backends, and callers without a flag client. Check downstream and stored-view expectations for the changed type or behavior.
5. Remove obsolete imports and build dependencies only where necessary. Avoid unrelated cleanup.
6. Format, inspect the diff, check for remaining references, and run affected tests using repository-supported tooling. In dd-source use `bzl`, never `bazel`, and never run `bzl` commands concurrently.

If tests are blocked by infrastructure, report the specific blocker. Do not introduce dependency overrides or persistent workarounds merely to get a green result, and do not claim the tests passed.

## Open a concise PR

Follow the `draft-pr` skill and applicable repository rules. Open a draft unless the user requests otherwise. Do not post comments or request reviews without separate approval.

Keep the substantive body to one or two short sentences. Include the feature-flag link, supported rollout age, and any material environment caveat. Do not add investigation logs, evidence tables, routine testing summaries, section headers, or a walkthrough of the diff. Retain any required AI acknowledgment separately.

### PR body template

```markdown
Remove [`<flag-name>`](<flag-url>) and always use its enabled behavior. The flag has been live in <environment> for <duration> (since <date>)<optional environment caveat>.

---
Note: This PR was developed with AI assistance. All changes have been reviewed, and I take full responsibility for this contribution.
```

Replace the placeholders with the actual flag name, URL, environment, elapsed duration, and supported start date. Always include how long the flag has been live, not just a rollout date. If only evaluation evidence is available, qualify the sentence as “Enabled traffic has been observed in <environment> for <duration> (since <date>)” rather than claiming verified full rollout. Omit the environment caveat when it does not apply. If the duration cannot be established, explicitly say “How long the flag has been live has not been verified” rather than inventing a duration.

Return the PR link and any validation blocker briefly.
