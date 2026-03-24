---
name: substrait
description: Substrait specification knowledge for cross-language relational algebra serialization
---

## What is Substrait

Substrait is a format for describing compute operations on structured data. It provides a well-defined, cross-language specification for data compute operations, enabling interoperability between different query engines, databases, and data processing systems.

## Key Resources

- Specification docs: https://substrait.io/
- Main repo (spec + extensions): https://github.com/substrait-io/substrait
- Protobuf definitions: https://github.com/substrait-io/substrait-protobuf

### Language Implementations

- Go: https://github.com/substrait-io/substrait-go
- Java: https://github.com/substrait-io/substrait-java
- Python: https://github.com/substrait-io/substrait-py
- Rust: https://github.com/substrait-io/substrait-rs

## Core Concepts

### Type System

Types have four components:
- **Class**: `i8`, `i16`, `i32`, `i64`, `fp32`, `fp64`, `string`, `binary`, `boolean`, `timestamp`, `date`, `decimal<P,S>`, `varchar<L>`, `struct<...>`, `list<T>`, `map<K,V>`
- **Nullability**: `NULLABLE` (suffix `?`) or `REQUIRED` (no suffix)
- **Variation**: Physical variations of the same type class
- **Parameters**: For compound types (e.g., `decimal<38,10>`)

Substrait has a strict type system with no implicit coercion - all type changes must be explicit via cast expressions.

### Relations (Operators)

Logical relations form the query plan tree:
- **Read**: Source data (named tables, local files, virtual tables, Iceberg)
- **Filter**: Eliminates records based on boolean expression
- **Project**: Adds computed expressions to output
- **Sort**: Reorders data by sort fields
- **Fetch**: Limit/offset (corresponds to SQL LIMIT/OFFSET)
- **Aggregate**: Groups and computes measures (SUM, COUNT, etc.)
- **Join**: Combines inputs (inner, outer, left, right, semi, anti, mark)
- **Set**: Union, intersection, minus operations
- **Cross**: Cartesian product
- **Write**: INSERT/UPDATE/DELETE/CTAS operations

### Expressions

- **Field References**: Access record fields by index or name
- **Scalar Functions**: Transform values (arithmetic, string ops, etc.)
- **Aggregate Functions**: Collapse multiple records (SUM, AVG, COUNT)
- **Window Functions**: Compute over partitions (RANK, ROW_NUMBER)
- **Literals**: Constant values
- **Casts**: Explicit type conversions
- **If/Then/Else**: Conditional expressions

### Extensions

Extensions are defined in YAML files and referenced by URI:
- Extension URI format: `extension:<OWNER>:<ID>` (e.g., `extension:io.substrait:functions_arithmetic`)
- Function signature format: `<name>:<short_arg_type0>_<short_arg_type1>_...`

Standard extension files:
- `functions_arithmetic.yaml` - add, subtract, multiply, divide
- `functions_comparison.yaml` - lt, gt, equal, not_equal
- `functions_boolean.yaml` - and, or, not
- `functions_string.yaml` - concat, substring, like
- `functions_datetime.yaml` - date/time operations
- `functions_aggregate_generic.yaml` - count, sum, avg, min, max

## Serialization

- **Binary**: Protobuf-based, compact and performant
- **Text**: Human-readable format for debugging

## When to Use This Skill

Load this skill when:
- Working on any Substrait implementation (substrait-go, substrait-java, substrait-py, substrait-rs)
- Building query plans programmatically
- Implementing new relation types or expressions
- Working with extension functions or custom types
- Debugging Substrait plan serialization/deserialization
- Understanding how SQL maps to Substrait plans

## Quick Reference

### Type Short Names (for function signatures)

| Type | Short | Type | Short |
|------|-------|------|-------|
| i8 | i8 | i16 | i16 |
| i32 | i32 | i64 | i64 |
| fp32 | fp32 | fp64 | fp64 |
| string | str | binary | vbin |
| boolean | bool | timestamp | ts |
| decimal | dec | list | list |
| struct | struct | map | map |
| any | any | required enum | req |

### Join Types

| Type | Description |
|------|-------------|
| Inner | Only matching records from both sides |
| Outer | All records, nulls for non-matches |
| Left/Right | All from one side, matches from other |
| Left/Right Semi | Records with match on other side |
| Left/Right Anti | Records without match on other side |
| Left/Right Single | At most one match (error if multiple) |
| Left/Right Mark | Appends boolean "mark" column |
