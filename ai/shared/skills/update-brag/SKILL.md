---
name: update-brag
description: Update brag.md with accomplishments from GitHub, Jira, and Confluence, organized by quarter/month/week with value-oriented summaries
---

# update-brag

Maintain a living brag document that captures accomplishments with appropriate source links, organized hierarchically with value-oriented summaries that percolate upward.

## Team Context

Ben is on the **Query Semantics (QSEM)** team at Datadog, responsible for Substrait adoption across the query infrastructure:

- **Retriever** parses SQL and converts to Substrait
- **Substrait** is the cross-language query representation (QSEM's domain)
- **DataFusion** executes Substrait plans

QSEM doesn't own services; it owns the communication protocol between services. The team enables other teams in A&E (Access & Enrichment) within ODP (Observability Data Platform) to communicate queries effectively.

Value typically manifests as:
- Correctness improvements (fixing Substrait representations)
- Developer experience (tooling, docs, easier APIs)
- Upstream contributions (substrait-io, apache/datafusion)
- Unblocking other teams

**Job start date:** July 7, 2025

## OKRs

Team OKRs provide context for framing accomplishments. Reference these when writing quarter/month summaries to connect individual work to larger goals.

| Year | Quarter | Document ID | Link |
|------|---------|-------------|------|
| 2025 | Q4 | `1w1bx3snrdEl8uDpdGcBgIX8LImb6gC1u4jyQrmX-DTY` | [Google Doc](https://docs.google.com/document/d/1w1bx3snrdEl8uDpdGcBgIX8LImb6gC1u4jyQrmX-DTY/edit) |
| 2026 | Q1 | `1L6odBioO8m2o_0G8LmzWaCXyiFji0l_7WF-GhKOaPjA` | [Google Doc](https://docs.google.com/document/d/1L6odBioO8m2o_0G8LmzWaCXyiFji0l_7WF-GhKOaPjA/edit) |
| 2026 | Q2 | `1JVn-DbNzqNGTX0qVuLVET65yqOeyesiMybLAV07rsk8` | [Google Doc](https://docs.google.com/document/d/1JVn-DbNzqNGTX0qVuLVET65yqOeyesiMybLAV07rsk8/edit) |

Use `datadog-google-workspace_read_document` with the document ID to fetch OKR content.

When writing summaries:
- Connect accomplishments to relevant KRs where applicable
- Frame value in terms of OKR progress (e.g., "contributed to KR: reduce Trino query failures by X%")
- Don't force connections; only reference OKRs when the work genuinely contributes

## Sources

| Source | Tool | Scope |
|--------|------|-------|
| GitHub | `gh` CLI | DataDog, substrait-io, apache orgs (PRs authored, reviews given) |
| Jira | `acli` CLI | QSEM board primarily, also cross-team tickets |
| Confluence | Atlassian MCP tools | Pages authored |

## Document Structure

The brag doc lives at `brag.md` in the notes directory. Structure:

```markdown
<!-- last-updated: YYYY-MM-DD -->
# Brag Doc

## 2025

### Q3
- Value-oriented summary synthesizing multiple items (links)

#### Jul
- Month-level synthesis of related work (links)

##### W1
- Individual item (link)

##### W2
- Another item (link)
```

### Content at Each Level

| Level | Content Type |
|-------|--------------|
| Quarter | Impact/outcome summaries framed as value delivered |
| Month | Synthesized themes from related work |
| Week | Individual items with source links |

Weeks are numbered relative to the month (W1 = first week of that month).

Higher levels should **percolate up** value-oriented summaries, not just promote individual items. Synthesize related work into outcome statements that convey impact.

## CLI Commands

All commands use a `${SINCE_DATE}` parameter (YYYY-MM-DD format).

### GitHub PRs Authored (merged)

```bash
gh search prs --author=@me \
  --owner=DataDog --owner=substrait-io --owner=apache \
  --merged --created=">=${SINCE_DATE}" \
  --json number,title,repository,url,createdAt,closedAt \
  --limit 100
```

### GitHub Reviews Given

The basic search API doesn't reliably return reviews. Use the GraphQL API instead.

**Public repos (substrait-io, apache):**

```bash
gh api graphql -f query='
{
  user(login: "benbellick") {
    contributionsCollection(from: "${FROM_ISO}", to: "${TO_ISO}") {
      pullRequestReviewContributions(first: 100) {
        totalCount
        nodes {
          occurredAt
          pullRequest {
            title
            url
            author { login }
            repository { nameWithOwner }
            mergedAt
          }
        }
      }
    }
  }
}'
```

Where `${FROM_ISO}` and `${TO_ISO}` are ISO 8601 timestamps (e.g., `2025-07-01T00:00:00Z`).

Filter to exclude self-reviews and personal repos:
```bash
| jq '[.data.user.contributionsCollection.pullRequestReviewContributions.nodes[] 
      | select(.pullRequest.author.login != "benbellick") 
      | select(.pullRequest.repository.nameWithOwner | startswith("benbellick/") | not)]'
```

Summarize by repo:
```bash
| jq 'group_by(.pullRequest.repository.nameWithOwner) 
     | map({repo: .[0].pullRequest.repository.nameWithOwner, count: length, 
            authors: [.[].pullRequest.author.login] | unique})'
```

**Private repos (DataDog/dd-source, DataDog/logs-backend):**

The GraphQL contributions API only covers public repos. For private repos, use the REST search API:

```bash
gh api "search/issues?q=type:pr+reviewed-by:benbellick+repo:DataDog/dd-source+repo:DataDog/logs-backend&per_page=100" \
  | jq '.items | map(select(.user.login != "benbellick")) 
              | map({title: .title, url: .html_url, author: .user.login, created: .created_at}) 
              | sort_by(.created) | reverse'
```

### Jira Tickets

```bash
acli jira workitem search \
  --jql "project = QSEM AND assignee = currentUser() AND created >= ${SINCE_DATE} ORDER BY created DESC" \
  --json --paginate
```

For completed tickets specifically:

```bash
acli jira workitem search \
  --jql "project = QSEM AND assignee = currentUser() AND status = Done AND resolved >= ${SINCE_DATE} ORDER BY resolved DESC" \
  --json --paginate
```

### Confluence Pages

Use the Atlassian MCP tool `atlassian_searchConfluenceUsingCql`:

```
cql: creator = currentUser() AND created >= "${SINCE_DATE}" ORDER BY created DESC
cloudId: datadoghq.atlassian.net
```

## Workflow

1. **Read brag.md** to find existing content and the `last-updated` date
   - If empty or missing, use job start date: `2025-07-07`
   - If `last-updated` comment exists, use that date
   - If invoked with a date argument, use that instead

2. **Fetch incrementally** from sources since the determined date:
   - Run the GitHub, Jira, and Confluence queries above
   - Collect all items with their dates and source links

3. **If invoked with a manual item argument**, add that to the appropriate week

4. **Slot new items into weeks** based on their dates
   - Calculate which quarter, month, and week each item belongs to
   - Week 1 of a month starts on the first Monday of that month

5. **Identify clusters** of related work that could percolate up
   - Group items by theme, project, or impact area
   - Look for items that span multiple weeks or relate to each other

6. **For major clusters, ask questions** to clarify value/impact:
   - "I found N items related to X in [month]. How would you characterize the value of this work?"
   - "Who benefited from this? What did it enable?"
   - Suggest a draft summary and let the user refine it
   - Only ask for items that look like potential month/quarter highlights

7. **Write/update summaries**:
   - Month summaries: synthesize themes from weeks
   - Quarter highlights: frame as value-oriented outcome statements

8. **Update brag.md**:
   - Preserve existing content
   - Add new items in the correct locations
   - Update the `last-updated` comment to today's date

## Format Rules

- All items include source links
- Jira ticket references should be clickable links: `[[QSEM-XXX](https://datadoghq.atlassian.net/browse/QSEM-XXX)]`
- No em dashes (use "to" or commas instead)
- No `**Bold**: description` pattern
- Higher levels focus on outcomes and value, not activities
- Keep it scannable
- Use plain, direct language
- Quarter summaries should start with an OKR context line, e.g., "OKR context: O3 (Lambda Support), O5 (Library Parity)"

## Interactive Clarification

When encountering clusters of related work that could become month/quarter summaries, ask questions to understand impact:

- "I found N items related to [topic] in [timeframe]. How would you characterize the value of this work?"
- "Who benefited from this? What did it enable?"
- "Should these be summarized together? Here's a suggested summary: [draft]"

Only ask for major items that look like potential highlights. For routine items, slot them into weeks without asking.

## Invocation

The skill can be invoked in several ways:

1. **Basic update**: Just run the skill to do an incremental update from last-updated date
2. **With date override**: Specify a start date to fetch from (e.g., "update brag since 2026-01-01")
3. **With manual item**: Add a specific accomplishment (e.g., "add to brag: presented Substrait overview to Platform team")

When a manual item is provided, add it to the appropriate week AND run the incremental update.

## When to Use

Load this skill when:
- The user asks to update their brag doc
- The user wants to add something to their brag doc
- The user asks to review or summarize recent accomplishments
- The user mentions "brag" in the context of tracking work
