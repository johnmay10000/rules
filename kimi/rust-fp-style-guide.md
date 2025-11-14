# Rust Functional Programming Style Guide for Kimi CLI

**Version**: 2.0.0  
**Last Updated**: 2025-11-14  
**Part of**: [KIMI.md](KIMI.md) Global Rule Set  
**Target**: Rust projects (systems, web, CLI tools)

> **📖 Global Rules**: This document extends [KIMI.md](KIMI.md) with Rust-specific guidance. For mandatory universal rules (Git, documentation, testing, file size), see [KIMI.md](KIMI.md).

---

## Quick Links

- **Mandatory Rules**: See [KIMI.md](KIMI.md) sections 1-4  
- **FP Principles Deep Dive**: See [KIMI_FP_PRINCIPLES.md](KIMI_FP_PRINCIPLES.md)  
- **Workflow Guide**: See [KIMI_WORKFLOW_GUIDE.md](KIMI_WORKFLOW_GUIDE.md)  
- **Integration**: See [KIMI.md Integration](#kimi-integration) below  

---

## Core Rust FP Principles

Rust's ownership system naturally enforces many FP principles:

1. **Immutability by Default**: `let x = 5;` is immutable by default
2. **No Null Values**: Use `Option<T>` and `Result<T, E>` instead of null pointers
3. **Type Safety**: Strong static typing with zero-cost abstractions
4. **Ownership**: Prevents data races and side effects at compile time
5. **Pattern Matching**: Powerful match expressions for control flow

---

## Required Dependencies

```toml
[dependencies]
# Core FP utilities
thiserror = "1.0"
anyhow = "1.0"

# Async FP
futures = "0.3"
tokio = { version = "1", features = ["full"] }

# Functional data structures
im = "15.0"  # Immutable data structures

# Type-level programming (optional)
frunk = "0.4"
```

---

## Error Handling with Result

Rust's `Result` type is central to FP in Rust:

```rust
use anyhow::Result;

// Pure function: Parses without side effects
fn parse_config(path: &str) -> Result<Config> {
    let content = std::fs::read_to_string(path)?;
    let config: Config = serde_json::from_str(&content)?;
    Ok(config)
}

// Railway-oriented error handling
fn process_data(path: &str) -> Result<ProcessedData> {
    let config = parse_config(path)?;
    let raw_data = load_data(&config.input_path)?;
    let validated = validate_data(raw_data)?;
    let transformed = transform_data(validated)?;
    Ok(transformed)
}
```

---

## Kimi-Specific Rust Patterns

### Using Kimi's Task Tool for Complex Compilation

For complex Rust projects with multiple crates, use Kimi's subagents:

```rust
// Complex workspace setup that benefits from parallel verification
// Kimi can spawn subagents to:
// - Verify each crate compiles independently
// - Run tests in parallel across crates
// - Check feature flag combinations

[workspace]
members = [
    "core",
    "api",
    "cli",
    "ml",
]

[package]
name = "my_project"
version = "0.1.0"
edition = "2021"

[features]
default = ["std"]
std = []
nostd = []
```

### Parallel Verification with Kimi

Kimi can run multiple checks simultaneously:

```bash
# Kimi can run these in parallel:
cargo check --all-features    # Type checking
cargo clippy -- -D warnings   # Linting
cargo test --workspace        # Testing
cargo doc --no-deps          # Documentation
```

---

## Immutable Data Structures

```rust
use im::HashMap;  // Immutable HashMap

#[derive(Clone, Debug)]
struct AppState {
    users: HashMap<UserId, User>,
    config: Config,
}

impl AppState {
    // Returns new state, doesn't mutate self
    fn add_user(&self, user: User) -> Self {
        let mut new_users = self.users.clone();
        new_users.insert(user.id, user);
        
        Self {
            users: new_users,
            config: self.config.clone(),
        }
    }
    
    // Immutable update
    fn update_config(&self, f: impl Fn(Config) -> Config) -> Self {
        Self {
            users: self.users.clone(),
            config: f(self.config.clone()),
        }
    }
}
```

---

## Type-Driven Design

```rust
// Newtype pattern for type safety
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct UserId(u64);

#[derive(Debug, Clone)]
struct User {
    id: UserId,
    name: String,
    email: Email,  // Another newtype
}

// Compile-time guarantees with traits
pub trait Validator {
    type Error;
    fn validate(&self) -> Result<(), Self::Error>;
}

impl Validator for User {
    type Error = ValidationError;
    
    fn validate(&self) -> Result<(), Self::Error> {
        if self.name.is_empty() {
            return Err(ValidationError::EmptyName);
        }
        if !self.email.is_valid() {
            return Err(ValidationError::InvalidEmail);
        }
        Ok(())
    }
}
```

---

## Functional Control Flow

### Pattern Matching

```rust
enum ProcessingResult {
    Success(Data),
    Skipped(String),
    Failed(Error),
}

fn handle_result(result: ProcessingResult) -> Option<Data> {
    match result {
        ProcessingResult::Success(data) => Some(data),
        ProcessingResult::Skipped(reason) => {
            log::info!("Skipped: {}", reason);
            None
        }
        ProcessingResult::Failed(err) => {
            log::error!("Processing failed: {}", err);
            None
        }
    }
}
```

### Railway-Oriented Programming

```rust
pub fn process_file(path: &Path) -> Result<ProcessedData> {
    // Each step returns Result<T, E>
    std::fs::read_to_string(path)
        .map_err(|e| format!("Failed to read: {}", e))
        .and_then(|content| parse_json(&content))
        .and_then(|json| validate_schema(json))
        .and_then(|validated| transform_data(validated))
}

// Using the `?` operator (syntactic sugar)
pub fn process_file_sugar(path: &Path) -> Result<ProcessedData> {
    let content = std::fs::read_to_string(path)?;
    let json = parse_json(&content)?;
    let validated = validate_schema(json)?;
    let transformed = transform_data(validated)?;
    Ok(transformed)
}
```

---

## Iterator Patterns

```rust
// Functional iteration
let numbers = vec![1, 2, 3, 4, 5];

let doubled: Vec<i32> = numbers
    .iter()
    .map(|x| x * 2)
    .collect();

let evens: Vec<i32> = numbers
    .into_iter()
    .filter(|x| x % 2 == 0)
    .collect();

// Chaining operations
let result: Option<i32> = vec![Some(1), Some(2), None, Some(3)]
    .into_iter()
    .flatten()  // Removes Nones
    .reduce(|acc, x| acc + x);

// Folding with initial value
let sum: i32 = numbers.iter().fold(0, |acc, x| acc + x);
```

---

## Async FP

```rust
use futures::future::{ok, err, FutureResult};
use futures::prelude::*;

// Async functions return Result wrapped in Future
async fn fetch_user(id: UserId) -> Result<User, ApiError> {
    let url = format!("https://api.example.com/users/{}", id.0);
    let response = reqwest::get(&url).await?;
    let user = response.json().await?;
    Ok(user)
}

// Compose async operations
async fn fetch_user_with_posts(id: UserId) -> Result<(User, Vec<Post>), ApiError> {
    let user = fetch_user(id).await?;
    let posts = fetch_posts_by_user(id).await?;
    Ok((user, posts))
}

// Parallel composition
async fn fetch_multiple_users(ids: Vec<UserId>) -> Result<Vec<User>, ApiError> {
    let futures: Vec<_> = ids.iter()
        .map(|id| fetch_user(*id))
        .collect();
    
    let users: Result<Vec<_>, _> = try_join_all(futures).await;
    users
}
```

---

## Testing FP Code

```rust
#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_pure_function() {
        // Pure function: same input → same output
        let input = vec![1, 2, 3];
        let result1 = process_data(&input);
        let result2 = process_data(&input);
        assert_eq!(result1, result2);
    }
    
    #[test]
    fn test_immutability() {
        let original = AppState::default();
        let modified = original.add_user(test_user());
        
        // Original unchanged
        assert_eq!(original.users.len(), 0);
        // New state has user
        assert_eq!(modified.users.len(), 1);
    }
    
    #[test]
    fn test_result_type() -> Result<()> {
        // Tests can use Result<T, E> for ergonomic error handling
        let config = parse_config("test.json")?;
        let data = load_data(&config.input_path)?;
        assert!(!data.is_empty());
        Ok(())
    }
}
```

---

## Kimi CLI Integration

### Project Setup

1. **Environment Variable**:
   ```bash
   export KIMI_RULES_PATH="$HOME/projects/rules"
   ```

2. **Project-Specific Rules** (`.kimirules`):
   ```markdown
   # .kimirules for Rust Project
   
   ## Rust FP Rules
   - All functions must be pure unless marked with `#[impure]`
   - Use `Result<T, E>` for all fallible operations
   - No `unwrap()` in production code (use `?` or pattern matching)
   - Prefer immutable references (`&T`) over mutable (`&mut T`)
   - Use `Option<T>` instead of null checks
   
   ## Kimi-Specific
   - Use Task tool to verify workspace compilation
   - Spawn subagents for each crate in workspace
   - Run clippy, test, doc generation in parallel
   - Verify feature flags are properly configured
   ```

3. **Verification Script** (`kimi-verify.sh`):
   ```bash
   #!/bin/bash
   # Run all verification in parallel (Kimi can parallelize this)
   
   cargo check --all-features &
   cargo clippy -- -D warnings &
   cargo test --workspace &
   cargo doc --no-deps &
   
   wait
   echo "All checks passed!"
   ```

---

## Common Patterns for Systems Projects

### CLI Application Example

```rust
use anyhow::Result;
use clap::Parser;
use std::path::PathBuf;

#[derive(Parser, Debug)]
#[command(name = "data-processor")]
#[command(about = "Processes data using functional pipelines")]
struct Args {
    #[arg(short, long)]
    input: PathBuf,
    
    #[arg(short, long)]
    output: PathBuf,
    
    #[arg(short, long, default_value = "config.json")]
    config: PathBuf,
}

fn main() -> Result<()> {
    let args = Args::parse();
    
    // Railway-oriented pipeline
    let config = load_config(&args.config)?;
    let data = read_data(&args.input)?;
    let validated = validate_data(data, &config)?;
    let processed = process_data(validated, &config)?;
    let output = format_output(processed, &config.output_format)?;
    
    write_output(&args.output, output)?;
    
    Ok(())
}

// Each step is a pure function (except IO operations)
fn load_config(path: &Path) -> Result<Config> { /* ... */ }
fn read_data(path: &Path) -> Result<Data> { /* ... */ }
fn validate_data(data: Data, config: &Config) -> Result<ValidatedData> { /* ... */ }
fn process_data(data: ValidatedData, config: &Config) -> Result<ProcessedData> { /* ... */ }
fn format_output(data: ProcessedData, format: &OutputFormat) -> Result<String> { /* ... */ }
```

---

## Anti-Patterns to Avoid

### ❌ Avoid: `unwrap()` in Production

```rust
// BAD: Panics instead of proper error handling
fn process_data(path: &str) -> Data {
    let content = std::fs::read_to_string(path).unwrap();  // Panics on error!
    serde_json::from_str(&content).unwrap()  // Panics!
}

// GOOD: Propagate errors properly
fn process_data(path: &str) -> Result<Data> {
    let content = std::fs::read_to_string(path)?;
    Ok(serde_json::from_str(&content)?)
}
```

### ❌ Avoid: Interior Mutability

```rust
// BAD: Hides mutation
use std::cell::Cell;

struct Counter {
    count: Cell<u32>,  // Looks immutable, but isn't!
}

// GOOD: Explicit state transformation
#[derive(Clone)]
struct Counter {
    count: u32,
}

impl Counter {
    fn increment(self) -> Self {
        Self { count: self.count + 1 }
    }
}
```

---

## Kimi CLI Commands for Rust Projects

### Standard Verification

```bash
# Type checking
cargo check --all-features

# Linting
cargo clippy -- -D warnings

# Testing
cargo test --workspace

# Documentation
cargo doc --no-deps

# Format checking
cargo fmt -- --check

# Kimi can run these in parallel with one command:
kimi verify-rust-project
```

---

## Summary

Rust's type system and ownership model naturally enforce FP principles. Use `Result` and `Option` for error handling, immutable data structures for state management, and pattern matching for control flow. Kimi CLI can verify all these properties in parallel.

**Key Takeaways**:
- Use `Result<T, E>` for all fallible operations
- Immutable by default (no `mut` unless necessary)
- Newtype pattern for type safety
- Pattern matching instead of null checks
- `Option<T>` and `Result<T, E>` instead of exceptions
- Iterator methods for functional transformations
- Kimi's parallel verification capabilities

**Next**: See [KIMI_FP_PRINCIPLES.md](KIMI_FP_PRINCIPLES.md) for deeper FP concepts.

---

**Last Updated**: 2025-11-14  
**Maintained By**: Kimi CLI Global Rules System  
**Status**: Active
