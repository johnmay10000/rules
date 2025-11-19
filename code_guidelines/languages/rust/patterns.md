---
title: Rust Functional Programming Patterns Reference
language: rust
category: code_guidelines
type: patterns
applies_to: [cursor, kimi, claude, gemini]
version: 1.0.0
last_updated: 2025-11-19
---

# Rust Functional Programming Patterns Reference

Quick reference for common functional programming patterns in Rust.

## Error Handling Patterns

### Result Combinators

Chain operations that return `Result`.

```rust
fn process(data: &str) -> Result<User, Error> {
    parse(data)
        .and_then(validate)
        .and_then(transform)
}

// Using ? operator (Do Notation equivalent)
fn process_do(data: &str) -> Result<User, Error> {
    let parsed = parse(data)?;
    let validated = validate(parsed)?;
    Ok(transform(validated)?)
}
```

### Option Combinators

Chain operations that return `Option`.

```rust
let username = find_user(id)
    .map(|u| u.name)
    .filter(|n| !n.is_empty())
    .unwrap_or_else(|| "Anonymous".to_string());
```

## Iterator Patterns (Foldable)

Rust's iterators are zero-cost abstractions for FP data processing.

### Basic Operations

```rust
let numbers = vec![1, 2, 3, 4, 5];

// Map and Filter
let evens_doubled: Vec<i32> = numbers
    .iter()
    .filter(|&&x| x % 2 == 0)
    .map(|&x| x * 2)
    .collect();

// Fold (Reduce)
let sum = numbers.iter().fold(0, |acc, &x| acc + x);
```

### Advanced Fold

```rust
use itertools::Itertools;

// Fold with early exit
let result = numbers.iter().fold_while(0, |acc, &x| {
    if acc + x > 100 {
        itertools::FoldWhile::Done(acc)
    } else {
        itertools::FoldWhile::Continue(acc + x)
    }
}).into_inner();
```

## Collection Patterns (Traversable)

### Collect with Result (Traverse)

Turn `Vec<Result<T, E>>` into `Result<Vec<T>, E>`.

```rust
fn validate_all(items: Vec<String>) -> Result<Vec<u32>, ParseError> {
    items
        .into_iter()
        .map(|s| s.parse::<u32>())
        .collect() // Magic!
}
```

### Collect with Option

Turn `Vec<Option<T>>` into `Option<Vec<T>>`.

```rust
fn find_all(ids: Vec<u64>) -> Option<Vec<User>> {
    ids.into_iter()
        .map(find_user)
        .collect()
}
```

### Partition

Split a collection into two based on a predicate.

```rust
let (evens, odds): (Vec<_>, Vec<_>) = numbers
    .into_iter()
    .partition(|&x| x % 2 == 0);
```

## Ownership & Borrowing Patterns

### Cow (Clone-on-Write)

Avoid cloning until necessary.

```rust
use std::borrow::Cow;

fn sanitize(input: Cow<str>) -> Cow<str> {
    if input.contains("bad") {
        Cow::Owned(input.replace("bad", "good"))
    } else {
        input // No allocation!
    }
}
```

### Builder Pattern (Immutable)

Functional updates for configuration.

```rust
#[derive(Clone)]
struct Config {
    port: u16,
    host: String,
}

impl Config {
    fn new() -> Self { Config { port: 80, host: "localhost".into() } }
    
    fn with_port(self, port: u16) -> Self {
        Config { port, ..self }
    }
}

let config = Config::new().with_port(8080);
```

## Function Composition

### Composition Helpers

```rust
fn compose<A, B, C, F, G>(f: F, g: G) -> impl Fn(A) -> C
where
    F: Fn(A) -> B,
    G: Fn(B) -> C,
{
    move |x| g(f(x))
}

let add_one = |x| x + 1;
let double = |x| x * 2;
let pipeline = compose(add_one, double);
```

## Newtype Pattern

Type-safe wrappers.

```rust
struct UserId(u64);
struct OrderId(u64);

fn find_order(id: OrderId) -> Option<Order> { ... }

// find_order(UserId(1)) // Compile error!
```