---
name: trino-function-migration
description: Migrate Trino functions to work in the DDSQL/Substrait/DataFusion stack
---

# Trino Function Migration

Migrate Trino SQL functions to the DDSQL -> Substrait -> DataFusion stack.

## Core Principle

**Substrait is the source of truth for function semantics.** 

The goal is to map Trino functions to clean Substrait primitives. DataFusion is just the execution layer - its API should not pollute the Substrait design.

Flow: `Trino SQL` → `Substrait function` → `DataFusion execution`

## Workflow

### Step 1: Understand Trino Function Semantics

**Read the docs:**
- https://trino.io/docs/current/functions.html
- Specific pages: array.html, string.html, datetime.html, regexp.html, etc.

**Experiment with queries:**
```bash
retriever-cli -e trino -q "SELECT <function>(...)" -d us1.staging.dog
```

Test edge cases:
- NULL inputs (each argument position)
- Empty strings/arrays
- Boundary values
- Error conditions

### Step 2: Find or Define Substrait Representation

Look for existing Substrait functions that capture the same semantics:

**A. Substrait core spec** (https://github.com/substrait-io/substrait/tree/main/extensions):
- `functions_arithmetic.yaml`
- `functions_string.yaml`
- `functions_comparison.yaml`
- `functions_datetime.yaml`
- `functions_aggregate_generic.yaml`

**B. Datadog extensions** (`domains/substrait/extensions/yaml/`):
- `functions_datadog.yaml` - General cross-dialect functions
- `trino.yaml` - Trino-specific functions (when no clean primitive exists)

**Decision tree:**
1. Does an existing Substrait core function match the semantics? → Use it
2. Does an existing `functions_datadog.yaml` function match? → Use it
3. Can the Trino function be expressed as a composition of existing primitives? → Consider that
4. Is this a Trino-specific behavior with no clean primitive? → Add to `trino.yaml`

### Step 3: Wire Up the Parser

**If using existing Substrait function with different name**, add manual mapping in `trino_builtins.go`:
```go
"trino_fn_name": MakeBuiltin(trinoFnProp,
    tree.Overload{
        Types: tree.ParamTypes{...},
        ReturnType: tree.FixedReturnType(types.String),
        Volatility: volatility.Immutable,
        Substrait: tree.FunctionSubstraitInfo{
            ID: tree.SubstraitID{
                Name: "substrait_fn:type_signature",
                URN:  "extension:com.datadog:functions_datadog",
            },
        },
        Type:  tree.SubstraitRoutine,
        Info:  "description",
        Class: tree.NormalClass,
        Fn:    EvaluationUnimplemented,
    },
),
```

**If defining new function in trino.yaml**:
```yaml
- name: "function_name"
  description: "Description -- https://trino.io/docs/current/functions/..."
  impls:
    - args:
        - value: string
          name: input
        - value: i64
          name: count
      return: string
```

Then regenerate the parser bindings:
```bash
bzl run //domains/xpq/shared/libs/crdb/sql/sem/builtins:trino_builtins_gen.go.update
```

After regeneration, verify the plan generates correctly:
```bash
bzl run //domains/xpq/scripts/substrait-cli:substrait-cli -- plan \
  --sql "SELECT function_name(col) FROM t" \
  --schema "CREATE TABLE t (col TYPE)"
```

### Step 4: Ensure DataFusion Execution

Now that Substrait representation is defined, ensure DataFusion can execute it.

**Check if DataFusion already has a matching function:**
- Docs: https://datafusion.apache.org/user-guide/sql/scalar_functions.html
- Test: `datafusion-cli -c "SELECT <function>(...)"`

**Two layers handle Substrait → DataFusion:**

1. **Substrait Consumers** (`libs/rust/dd-datafusion/substrait/`): Convert Substrait function calls to DataFusion logical plans. Special consumers exist for complex types:
   - `GroupSubstraitConsumer` - group type handling
   - `JsonSubstraitConsumer` - JSON operations
   - `ArraySortSubstraitConsumer` - array sorting
   - Registered in `runtime.rs:new_substrait_consumer()`

2. **UDF Registration** (`libs/rust/dd-datafusion/runtime/src/runtime.rs`): Makes DataFusion functions available. UDFs are registered in `DDDataFusionQueryPlanner::default().with_udfs(vec![...])`.

**If DataFusion has a matching built-in function:**
- The function name in Substrait must match the DataFusion UDF name (or an alias)
- Check `runtime.rs` for existing aliases, e.g., `array_length_udf().with_aliases(["group_length"])`

**If DataFusion doesn't have the function:**
1. Implement UDF in `libs/rust/dd-datafusion/functions/src/`
   - Follow `libs/rust/dd-datafusion/functions/CLAUDE.md` guidelines
   - Key rules: O(batch) allocations, enum iterators, proper NULL handling
2. Register in `libs/rust/dd-datafusion/runtime/src/runtime.rs` in the `with_udfs(vec![...])` block
3. If the function needs special Substrait consumption logic, add a consumer in `libs/rust/dd-datafusion/substrait/`

### Step 5: End-to-End Verification

The goal is to prove the function behaves identically in Trino and the Substrait/DataFusion stack.

#### 5a. Get Trino Results (Ground Truth)

Run test cases on Trino to establish expected behavior:
```bash
retriever-cli -e trino -q "SELECT fn(...)" -d us1.staging.dog
```

Test various cases: normal inputs, NULLs, empty values, edge cases.

#### 5b. Generate Substrait Plan

Always use `bzl run` to get the latest build with your parser changes. Use `--schema` to avoid network calls:

```bash
bzl run //domains/xpq/scripts/substrait-cli:substrait-cli -- plan \
  --sql "SELECT fn(col) FROM t" \
  --schema "CREATE TABLE t (col TYPE)"
```

For more complex schemas:
```bash
bzl run //domains/xpq/scripts/substrait-cli:substrait-cli -- plan \
  --sql "SELECT array_join(tags, ',') FROM t" \
  --schema "CREATE TABLE t (id INT, tags STRING[], name STRING)"
```

To get JSON format for df-executor:
```bash
bzl run //domains/xpq/scripts/substrait-cli:substrait-cli -- plan \
  --sql "SELECT fn(col) FROM t" \
  --schema "CREATE TABLE t (col TYPE)" \
  --format json \
  --output /tmp/plan.json
```

#### 5c. Execute on df-executor

**Option 1: Local df-executor**

Terminal 1 - Start the service:
```bash
DD_CLOUDPROVIDER=aws AWS_REGION=us-east-1 bzl run //domains/xpq/apps/df-executor -- \
  -l -c $DATADOG_ROOT/dd-source/domains/xpq/apps/df-executor/local_config.toml
```

Terminal 2 - Submit the plan:
```bash
bzl run //domains/xpq/apps/df-executor:example.submit-plan -- /tmp/plan.json
```

**Option 2: Staging df-executor**
```bash
bzl run //domains/xpq/apps/df-executor:example.submit-plan -- /tmp/plan.json --url https://df-executor.us1.staging.dog
```

#### 5d. Compare Results

Results from Trino and df-executor must match for all test cases.

## Key Files

| Purpose | Path |
|---------|------|
| Substrait core extensions | https://github.com/substrait-io/substrait/tree/main/extensions |
| Datadog Substrait functions | `domains/substrait/extensions/yaml/functions_datadog.yaml` |
| Trino Substrait functions | `domains/substrait/extensions/yaml/trino.yaml` |
| Trino parser mappings (manual) | `domains/xpq/shared/libs/crdb/sql/sem/builtins/trino_builtins.go` |
| Trino parser mappings (generated) | `domains/xpq/shared/libs/crdb/sql/sem/builtins/trino_builtins_gen.go` |
| DDSQL parser mappings | `domains/xpq/shared/libs/crdb/sql/sem/builtins/substrait_builtins.go` |
| DataFusion UDFs | `libs/rust/dd-datafusion/functions/src/` |
| DataFusion UDF guidelines | `libs/rust/dd-datafusion/functions/CLAUDE.md` |
| DataFusion runtime & UDF registration | `libs/rust/dd-datafusion/runtime/src/runtime.rs` |
| Substrait consumers (Substrait→DF mapping) | `libs/rust/dd-datafusion/substrait/` |

## Verification

Create a comparison table proving identical behavior:

| Test Case | Trino | Substrait/DF | Match |
|-----------|-------|--------------|-------|
| Basic: `fn('hello', 2)` | result | result | Y |
| NULL arg1: `fn(NULL, 2)` | NULL | NULL | Y |
| NULL arg2: `fn('hello', NULL)` | NULL | NULL | Y |
| Empty: `fn('', 0)` | result | result | Y |
| Edge case | ... | ... | Y |

## Running Local Retriever (for parser testing)

```bash
# Terminal 1: Port forwards
kubectl port-forward service/orgstore-retriever-pg-proxy --context gizmo.us1.staging.dog --namespace orgstore-retriever 5432:5432 &
kubectl port-forward service/trino-datadog-472-a-0 --context oddish-a.us1.staging.dog --namespace apm-trino 8443:8443 &

# Terminal 2: Local retriever
rapid run -d xpq -s retriever --reload
```

## Common Patterns

### Pattern: Trino function aliases existing Substrait function
Example: Trino `cardinality(array)` → Substrait `array_length:list`

Just add mapping in `trino_builtins.go` pointing to existing function.

### Pattern: Trino function needs new Substrait definition
Example: Trino `regexp_extract` with group parameter

1. Add to `trino.yaml` with clear semantics
2. Implement/map in DataFusion
3. Wire up consumer

### Pattern: Trino function maps to composition
Sometimes a Trino function can be expressed as existing primitives. Consider if this is cleaner than a new function.
