---
title: Kotlin Functional Programming Libraries
language: kotlin
category: code_guidelines
type: libraries
applies_to: [cursor, kimi, claude, gemini]
version: 1.0.0
last_updated: 2025-11-19
---

# Kotlin Functional Programming Libraries

Recommended libraries for functional programming in Kotlin.

## Arrow (The Standard)

Arrow is the definitive FP library for Kotlin, providing a comprehensive suite of types and abstractions.

### `arrow-core`
- **Version**: 1.2.0+
- **Purpose**: Essential FP types (`Either`, `Option`, `Ior`, `Validated`) and type classes.
- **Use Case**: Error handling, validation, functional data structures.

### `arrow-fx-coroutines`
- **Version**: 1.2.0+
- **Purpose**: Functional effects and coroutine integration.
- **Use Case**: Resource safety (`Resource`), parallel execution (`parZip`, `parTraverse`), atomic state (`Atomic`).

### `arrow-optics`
- **Version**: 1.2.0+
- **Purpose**: Lenses, Prisms, and other optics.
- **Use Case**: Deep immutable updates, accessing nested data structures without boilerplate.

## Standard Library

Kotlin's standard library includes many FP primitives out of the box.

- **Collections**: `map`, `filter`, `fold`, `reduce`, `flatMap`.
- **Sequences**: Lazy collections (`asSequence`).
- **Result**: Built-in `Result<T>` type (though `Either` is often preferred for typed errors).
- **Scope Functions**: `let`, `run`, `with`, `apply`, `also`.

## Coroutines

### `kotlinx-coroutines-core`
- **Version**: 1.7.3+
- **Purpose**: Asynchronous programming.
- **Use Case**: Structured concurrency, `Flow` (reactive streams), `Channel`.
