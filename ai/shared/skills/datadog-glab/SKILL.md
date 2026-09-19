---
name: datadog-glab
description: Inspect Datadog GitLab CI with glab. Use when checking pipeline status, finding failed jobs, reading job logs, following downstream pipelines, or retrying CI jobs.
---

# Datadog glab

Use the existing `glab` setup as-is. Do not inspect, configure, or troubleshoot authentication.

Run commands through interactive zsh so the configured `glab` wrapper is available:

```bash
zsh -lic 'glab <command>'
```

## Check CI

Find pipelines for a commit:

```bash
zsh -lic 'glab ci list --sha <commit-sha> --output json'
```

Inspect a pipeline and its jobs:

```bash
zsh -lic 'glab ci get --pipeline-id <pipeline-id> --with-job-details --output json'
```

Show only failed jobs:

```bash
zsh -lic 'glab ci get --pipeline-id <pipeline-id> --status failed --with-job-details --output json'
```

## Follow downstream pipelines

A failed parent may contain failed bridge-triggered pipelines:

```bash
zsh -lic 'glab api projects/<url-encoded-project>/pipelines/<pipeline-id>/bridges'
```

Inspect each failed `downstream_pipeline.id` with `glab ci get`.

## Read logs

```bash
zsh -lic 'glab ci trace <job-id>'
```

Read the full log or search it for the first meaningful error. The final lines often contain only a generic exit-status failure.

## Retry

Retry only when the user explicitly requests it:

```bash
zsh -lic 'glab ci retry <job-id>'
```

## Workflow

1. Find the exact pipeline using its commit SHA.
2. Inspect its jobs and bridges.
3. Follow failed downstream pipelines.
4. Read failed job logs.
5. Report the pipeline, job links, and root cause.
