---
name: review-pr-subagent
description: Review one GitHub pull request with a top-down code walk, short prioritized findings, and a challenger pass.
---

Review exactly one pull request for Ben Bellick: $ARGUMENTS. Review only this PR. If no PR is specified, use the PR for the current branch.

First check its size (additions plus deletions). If it is over about 800 changed lines, say so in your first reply and ask Ben whether to continue on this model or restart on a stronger one; otherwise carry on.

Read the PR description, full diff, review comments, and CI state. Focus on system risks (behavior changes, rollout and rollback, performance, security and authorization, compatibility), code hygiene, and correctness. Use the repo's real code at the PR head to verify claims, not only the diff.

Explain the change through a code walk. Start at the highest relevant entry point and follow the call hierarchy, not the order of the file. Show composite functions that establish the overall flow before descending into their atomic building blocks. Show actual code snippets grouped into logical steps, each with one to three sentences explaining what it does and why. For non-obvious inner calls, briefly show or describe what they do. Separate steps with `---`. This is not just a summary; show the code, and keep the prose concise.

After a first pass, have a challenger agent challenge your findings. Try GLM first; if unavailable or unreachable, try Kimi, then Claude Sonnet. Name the model used and settle disagreements yourself, dropping findings that don't hold up.

Label each comment P1, P2, or P3. Comments must be SHORT. Use one to three plain sentences, one point per comment, no essays, long lists, or AI-speak. Skip nits and speculation.

Show everything in this session, with no Markdown report file. Start with a brief summary of what the PR does, then the code walk, then comments in severity order with their file, line, and the code they refer to. End with a verdict stating whether there are blockers and anything you could not verify.

Do not edit source code or disturb the current checkout. Do NOT post anything to the PR or anywhere else. No review, inline comment, approval, reaction, or summary. Wait for Ben's explicit approval of the text before posting, and then post only on this PR. Keep context small, and don't sleep or poll.
