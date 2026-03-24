---
name: run-query-trino
description: Run SQL queries against a local Trino instance using Docker
---

# run-query:trino

Run SQL queries against a local Trino server using Docker.

## Workflow

1. **Ensure Docker container is running**: Check if the `trino` container exists and is running.
2. **Start container if needed**: If not running, start it with the appropriate command.
3. **Execute the query**: Run the query using `docker exec`.
4. **Return results** to the user.

## Commands

### Check if container is running

```bash
docker ps --filter "name=trino" --format "{{.Names}}"
```

### Start the Trino container (if not running)

If no container exists:

```bash
docker run -d -p 8080:8080 --name trino trinodb/trino
```

If container exists but is stopped (you get a "name already in use" error):

```bash
docker start trino
```

Wait a few seconds for Trino to initialize before running queries.

### Run a query

```bash
docker exec trino trino --execute 'SELECT * FROM tpch.tiny.region'
```

For queries with a specific catalog/schema:

```bash
docker exec trino trino --catalog tpch --schema tiny --execute 'SELECT * FROM region LIMIT 10'
```

### Output formats

Default is aligned columns. For CSV output:

```bash
docker exec trino trino --output-format CSV --execute 'SELECT ...'
```

## Available Catalogs

The default `trinodb/trino` image includes:

- `tpch` - TPC-H benchmark data (schemas: `tiny`, `sf1`, `sf10`, etc.)
- `tpcds` - TPC-DS benchmark data
- `memory` - In-memory tables
- `system` - System information

## Example Queries

```sql
-- List available catalogs
SHOW CATALOGS;

-- List schemas in tpch
SHOW SCHEMAS FROM tpch;

-- List tables in tpch.tiny
SHOW TABLES FROM tpch.tiny;

-- Sample query
SELECT * FROM tpch.tiny.nation LIMIT 5;

-- Check cluster nodes
SELECT * FROM system.runtime.nodes;
```

## Cleanup

To stop and remove the container:

```bash
docker stop trino && docker rm trino
```

## When to Use This Skill

Load this skill when:
- The user asks to run a Trino query locally
- The user wants to test SQL against Trino
- The user mentions "run-query:trino"
