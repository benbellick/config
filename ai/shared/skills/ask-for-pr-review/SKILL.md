---
name: ask-for-pr-review
description: Draft and, only after explicit confirmation, send Slack requests for GitHub PR reviews. Resolve the PR from a supplied reference, clear conversation context, or the current branch; map requested reviewer teams to their Packs review/contact channels; match the user's previous public Slack requests. Use when asked to ask for a PR review, request reviews in Slack, or ping reviewer teams.
---

# Ask for PR review

Prepare a review request, show the exact message and destinations, and wait for confirmation before sending anything. This is not a code review or a PR creation workflow.

## 1. Resolve the PR

Use this order:

1. An explicit PR URL or number in the request. Resolve a bare number against the current repository; ask if the repository is unclear.
2. An unambiguous PR already established in the conversation.
3. The PR associated with the checked-out branch. Use read-only Git commands to identify the repository and branch, then `gh pr view` without a PR argument from that repository.

If there are conflicting candidates, no associated PR, a detached HEAD without a clear target, or no repository context, ask for the PR URL. Do not silently pick a different PR or create one. Do not checkout branches, push, or modify GitHub state.

Fetch metadata using `gh pr view` with these JSON fields:

```bash
gh pr view <PR> --json url,title,body,state,isDraft,author,reviewRequests,reviews
```

Use the title and description to understand the change well enough to write a short request. Inspect file names or a focused diff only if necessary to clarify the summary; do not perform a code review or run tests.

If the PR is closed, merged, or still a draft, flag that before proceeding and ask whether the user wants to request reviews anyway. Do not mark it ready yourself.

## 2. Resolve reviewer teams to Slack channels

Read team entries from `reviewRequests`; keep individual reviewer requests separate. Requested reviewers are not proof that merge rules require all of those approvals. Do not claim a team still owes approval merely because it owns related files.

For each requested team, use its team slug without the organization prefix to query Packs:

```bash
ddtool packs get <team-slug> --format json
```

Discover `ddtool packs get --help` if the installed CLI differs. The output may be an array of packs; select the record whose `id` exactly matches the team slug. Never assume the first record is the right one.

Choose the destination exactly as Doggo does:

```typescript
pack.communication_channels?.review_channel ||
pack.communication_channels?.contact_channel
```

The values are Slack channel IDs. Never use `notification_channel` for review requests. Never hardcode a team-to-channel mapping or substitute a channel inferred from Slack activity when a Pack is missing. If lookup fails, authentication is unavailable, or neither field is set, report the unresolved team and ask the user for a destination or permission to omit it. Do not change authentication or Pack configuration.

Resolve each channel ID to its current name using Slack metadata. Treat the ID as authoritative; a similar channel name is not sufficient. Deduplicate destinations by channel ID and retain which teams map to each destination. Show whether each mapping used `review_channel` or the `contact_channel` fallback.

Do not automatically DM individual requested reviewers or expand to unrelated channels. If only individual reviewers are requested, or no team is requested, ask the user where the request should go.

The implementation reference is Doggo's `getReviewSlackUrl` in [reviewersSlackLink.tsx](https://github.com/ddoghq/web-ui/blob/fedf0d1aa314b83a0b5a39fe024d4f904439e97a/packages/apps/doggo-extension/private/runtime/extension/sources/github/reviewersSlackLink.tsx#L40-L48). It reads Pack metadata from Holocene rather than maintaining mappings in the extension.

## 3. Learn the user's style and check for duplicates

Discover the available Slack tools and their current schemas rather than guessing tool names or arguments. Determine the authenticated Slack user's identity from tool metadata or an identity lookup. Do not assume the PR author is the user requesting reviews, and do not hardcode a Slack user ID.

Search a small sample of the user's previous review requests in public Slack channels. For example, use public message search with `from:<@USER_ID> has:link` and the keyword `review`. Prefer examples from the destination channels, broadening to the user's other public messages if necessary. Three to five relevant examples are enough. Skip empty results and do not read unrelated conversations.

Use public search only by default. Do not search private channels or DMs, or read private destination history, without explicit permission. If examples are unavailable, say so briefly and use the style below rather than blocking the draft.

Also check public destination channels for a recent post of this exact PR URL. If a request already exists, surface its permalink and ask whether to send a reminder, reply in its thread, omit that channel, or do nothing. Do not silently post duplicates. Any thread destination must appear in the confirmation preview.

Ben's observed style is concise and conversational:

- A direct ask such as “Can I have a review here?” with the PR linked.
- One or two sentences explaining the problem and why the change matters.
- Occasional `:wave:`, `:review:`, or `:small_pr:` and a short thank-you. Do not label a PR small without evidence.
- No formal headings, canned review checklist, or long testing summary in the Slack message.

Examples of his public requests are [this retriever-cli request](https://dd.enterprise.slack.com/archives/C09EWGQ5C1W/p1792610739230449) and [this test-runner request](https://dd.enterprise.slack.com/archives/C09EWGQ5C1W/p1792195247197769). Match the tone, not their technical details. Prefer fresh examples when available.

Draft from the PR's actual context. Do not invent urgency, test results, deployment status, personal involvement, or claims that the user authored or validated the change. Do not add `@channel`, `@here`, user-group pings, or individual mentions unless specifically requested and included in the approved text.

## 4. Show the preview and wait

Present:

1. The resolved PR URL and title.
2. A destination table containing each channel name/link, the mapped reviewer team(s), and whether the source is `review_channel` or `contact_channel`.
3. The exact Slack message to send, including links, emojis, and any mentions. Use Slack-compatible formatting, such as `<https://github.com/org/repo/pull/123|here>`, and show the exact payload in a code block so link syntax is visible.
4. Any unresolved mappings, duplicate requests, or private-history limitations.
5. A direct confirmation question, such as “Send this exact message to these three channels?”

Use the same message for all destinations unless different versions are genuinely useful. If proposing different messages, show the exact version for each destination. For a thread reply, include the parent message permalink in the destination list.

Stop and wait for explicit approval. Invoking this skill, asking to request reviews, approving an earlier request, or approving only the wording does not authorize an unseen destination list. If the user changes the wording, channels, or thread targets, show the revised preview and obtain confirmation of the final plan before sending. Omitted teams must be disclosed and approved.

## 5. Send only after approval

Re-read the Slack sending tool's schema. Send only the exact approved text to the approved channel IDs and thread targets, once per destination. Never turn the preview into a send in the same turn without the user's intervening approval.

Check every result for errors. If a send times out or has an ambiguous outcome, do not blindly retry and risk a duplicate; verify whether it posted, or report the uncertainty and ask how to proceed. If only some sends succeed, report those successes and stop rather than resending the entire batch. Do not fall back to another channel or DM.

Return the message permalinks and a concise delivery summary. Do not post GitHub comments, change requested reviewers, join channels, or modify anything else as part of this workflow.
