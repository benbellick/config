---
name: retriever-cli
description: Execute SQL queries against Datadog Retriever (DDSQL/Beagle, Trino, DataFusion)
---

# retriever-cli

Run SQL queries against Datadog's Retriever service.

## Usage

When the user asks to run a query, execute it with `retriever-cli`:

```bash
# DDSQL/Beagle (default)
retriever-cli -d us1.staging.dog -q 'SELECT ...'

# Trino
retriever-cli -d us1.staging.dog -e trino -q 'SELECT ...'

# DataFusion
retriever-cli -d us1.staging.dog -e substrait -q 'SELECT ...'
```

## Engines

- `ddsql` (default) - DDSQL/Beagle
- `trino` - Trino
- `substrait` - DataFusion

## Options

- `-e <engine>` - execution engine
- `-d <datacenter>` - default `us1.staging.dog`
- `-q '<sql>'` - the query
- `--timeout <duration>` - e.g. `10m`, `1h` (default 5m)
- `--json` - output JSON for parsing
