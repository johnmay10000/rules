---
title: Swift Functional Programming Libraries
language: swift
category: code_guidelines
type: libraries
applies_to: [cursor, kimi, claude, gemini]
version: 1.0.0
last_updated: 2025-11-19
---

# Swift Functional Programming Libraries

Recommended libraries for functional programming in Swift.

## The Composable Architecture (TCA)

The gold standard for functional architecture in Swift, created by Point-Free.

- **Repository**: `pointfreeco/swift-composable-architecture`
- **Purpose**: State management, composition, side effects, testing.
- **Key Concepts**:
    - **State**: Immutable struct describing the state of your feature.
    - **Action**: Enum describing all possible actions.
    - **Reducer**: Pure function evolving state based on actions.
    - **Store**: Runtime that drives the feature.

```swift
import ComposableArchitecture
```

## Point-Free Ecosystem

A collection of focused, composable libraries.

### `swift-tagged`
- **Purpose**: Type-safe wrappers for primitive types (Newtype pattern).
- **Use Case**: Distinguishing `UserID` from `PostID` even if both are `UUID`.

```swift
import Tagged
typealias UserID = Tagged<User, UUID>
```

### `swift-overture`
- **Purpose**: Function composition operators and helpers.
- **Use Case**: `pipe`, `with`, `update`, `curry`.

```swift
import Overture
```

### `swift-case-paths`
- **Purpose**: Key paths for enum cases.
- **Use Case**: Accessing enum associated values dynamically and safely.

## Bow

A comprehensive FP library for Swift (similar to Cats for Scala or fp-ts for TypeScript).

- **Repository**: `bow-arch/bow`
- **Purpose**: Higher-kinded types emulation, standard FP data types (`Option`, `Either`, `Try`, `IO`).
- **Note**: Less commonly used in modern SwiftUI apps compared to TCA, but powerful for pure FP.

## Standard Library

Swift's standard library is already rich in FP primitives.

- **Result**: Built-in `Result<Success, Failure>`.
- **Sequence/Collection**: `map`, `filter`, `reduce`, `flatMap`, `compactMap`.
- **Combine**: Functional reactive programming framework (built-in).
- **Concurrency**: `async`/`await` with structured concurrency.
