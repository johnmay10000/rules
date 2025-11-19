---
title: Rust Functional Programming Libraries
language: rust
category: code_guidelines
type: libraries
applies_to: [cursor, kimi, claude, gemini]
version: 1.0.0
last_updated: 2025-11-19
---

# Rust Functional Programming Libraries

Recommended libraries for functional programming in Rust.

## Core FP Utilities

### `either`
- **Version**: 1.9
- **Purpose**: General purpose Sum Type for error handling and other dual-value scenarios.
- **Use Case**: When you need a type that can be one of two things, but not necessarily Success/Failure (use Result for that).

### `itertools`
- **Version**: 0.12
- **Purpose**: Extra iterator adaptors, functions, and macros.
- **Use Case**: Advanced iteration patterns like `zip_longest`, `group_by`, `unique`, `chunks`.

### `rayon`
- **Version**: 1.8
- **Purpose**: Data parallelism library.
- **Use Case**: Converting sequential iterators to parallel iterators with `par_iter()` for free performance gains on pure operations.

## Async Functional Programming

### `futures`
- **Version**: 0.3
- **Purpose**: Zero-cost asynchronous programming.
- **Use Case**: Composing async operations, combinators like `join_all`, `select`, `map`.

### `tokio`
- **Version**: 1.0 (features = ["full"])
- **Purpose**: Asynchronous runtime.
- **Use Case**: Running async FP pipelines.

## Advanced Patterns

### `frunk`
- **Version**: 0.4
- **Purpose**: Functional programming toolbelt.
- **Use Case**: HList (heterogeneous lists), Coproduct (anonymous enums), Generic (boilerplate-free conversions).

### `im`
- **Purpose**: Immutable data structures.
- **Use Case**: Efficient immutable collections (Vector, HashMap, Sets) that are structurally shared.

## Serialization

### `serde`
- **Version**: 1.0
- **Purpose**: Serialization framework.
- **Use Case**: Declarative serialization/deserialization for immutable data structures.
