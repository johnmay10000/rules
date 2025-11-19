---
title: Rust Functional Programming Style Guide
language: rust
category: code_guidelines
type: language
applies_to: [cursor, kimi, claude, gemini]
version: 2.0.0
last_updated: 2025-11-19
---

# Rust Functional Programming Style Guide

**Target**: Rust projects (systems programming, web services, CLI tools, embedded)

> **📖 Universal Rules**: This document provides Rust-specific functional programming guidance. For mandatory universal rules (Git, testing, documentation, project structure), see the `universal_rules/` directory.

---

## Quick Links

- **FP Principles Deep Dive**: See `code_guidelines/principles/functional_programming.md`
- **Pattern Reference**: See `code_guidelines/languages/rust/patterns.md`
- **Code Examples**: See `code_guidelines/languages/rust/examples.md`
- **Library Guide**: See `code_guidelines/languages/rust/libraries.md`

---

## Core Principles

1. **Immutability by Default**: All variables are immutable unless explicitly marked `mut`
2. **Ownership & Borrowing**: Use the type system to enforce memory safety without GC
3. **Pattern Matching**: Exhaustive matching for error handling and data decomposition
4. **Type Safety**: Leverage the type system to prevent errors at compile time
5. **Zero-Cost Abstractions**: FP patterns with no runtime overhead
6. **Explicit Error Handling**: Use `Result<T, E>` and `Option<T>` instead of exceptions

---

## Required Crates

```toml
# Cargo.toml dependencies

[dependencies]
# Core FP utilities
either = "1.9"          # Either type for error handling
itertools = "0.12"      # Iterator extensions
rayon = "1.8"           # Parallel iterators
serde = { version = "1.0", features = ["derive"] }  # Serialization

# For async FP
futures = "0.3"
tokio = { version = "1", features = ["full"] }

# For advanced patterns
frunk = "0.4"           # HList, Coproduct, Generic
```

---

## 1. Error Handling with Result

### ❌ Avoid: Panicking

```rust
// BAD: Panic on error - not recoverable
fn divide(a: f64, b: f64) -> f64 {
    if b == 0.0 {
        panic!("Division by zero!");  // Crashes the program
    }
    a / b
}
```

### ✅ Prefer: Result Type

```rust
// GOOD: Errors are part of the return type
fn divide(a: f64, b: f64) -> Result<f64, String> {
    if b == 0.0 {
        return Err("Division by zero".to_string());
    }
    Ok(a / b)
}

// Usage
let result = divide(10.0, 2.0);  // Ok(5.0)
let error = divide(10.0, 0.0);   // Err("Division by zero")

// Pattern matching
match divide(10.0, 2.0) {
    Ok(value) => println!("Result: {}", value),
    Err(e) => println!("Error: {}", e),
}

// Or use the ? operator for early return
fn calculate(a: f64, b: f64, c: f64) -> Result<f64, String> {
    let x = divide(a, b)?;  // Returns early if Err
    let y = divide(x, c)?;  // Returns early if Err
    Ok(y)
}
```

**Key Benefits**:
- Errors are explicit in the type signature
- Compiler forces error handling
- No hidden exceptions or runtime crashes
- Composable with `?` operator and combinators

---

## 2. Option for Nullable Values

### ❌ Avoid: Null Pointers

```rust
// BAD: Nullable pointer (like null in other languages)
fn find_user(id: &str) -> Option<&User> {
    // Could return None, but caller might forget to check
    database.get(id)
}

fn process_user(id: &str) {
    let user = find_user(id);
    println!("User: {}", user.name);  // Runtime panic if None!
}
```

### ✅ Prefer: Explicit Option Handling

```rust
// GOOD: Option forces explicit handling
fn find_user(id: &str) -> Option<User> {
    database.get(id).cloned()
}

// Usage - must handle None case
match find_user("123") {
    Some(user) => println!("Found: {}", user.name),
    None => println!("User not found"),
}

// Or use combinators
let email = find_user("123")
    .map(|user| user.email)           // Transform if Some
    .unwrap_or("guest@example.com".to_string());  // Default if None

// Pattern matching in function signature
fn greet_user(user: Option<User>) -> String {
    match user {
        Some(u) => format!("Hello, {}!", u.name),
        None => "Hello, guest!".to_string(),
    }
}
```

**Benefits**:
- Null safety at compile time
- Forces handling of missing values
- Rich combinator API
- No null pointer exceptions

---

## 3. Iterator Patterns

### ✅ Functional Iteration

```rust
// Immutable iteration - original collection unchanged
let numbers = vec![1, 2, 3, 4, 5];

// Map: transform each element
let doubled: Vec<i32> = numbers.iter().map(|x| x * 2).collect();
// doubled = [2, 4, 6, 8, 10]
// numbers unchanged = [1, 2, 3, 4, 5]

// Filter: keep elements matching predicate
let evens: Vec<i32> = numbers.iter().filter(|x| x % 2 == 0).cloned().collect();
// evens = [2, 4]

// Fold: reduce to single value
let sum: i32 = numbers.iter().fold(0, |acc, x| acc + x);
// sum = 15

// Chain operations
let result: Vec<i32> = numbers
    .iter()
    .filter(|x| x % 2 == 0)      // Keep evens: [2, 4]
    .map(|x| x * x)              // Square: [4, 16]
    .collect();

// Find first matching element
let first_even = numbers.iter().find(|x| x % 2 == 0);
// first_even = Some(&2)

// Check if all/any match
let all_positive = numbers.iter().all(|x| x > 0);  // true
let any_negative = numbers.iter().any(|x| x < 0);  // false
```

**Principle**: Iterators are lazy and immutable - chain operations without intermediate collections.

---

## 4. Immutability Patterns

### ✅ Immutable by Default

```rust
// Variables are immutable by default
let x = 5;
// x = 6;  // Compile error!

// Must explicitly mark as mutable
let mut y = 5;
y = 6;  // OK

// Immutability extends to references
let data = vec![1, 2, 3];
let immutable_ref = &data;  // Immutable reference
// immutable_ref.push(4);  // Compile error!

let mutable_ref = &mut data;  // Mutable reference
mutable_ref.push(4);  // OK

// Data structures are immutable when shared
use std::sync::Arc;  // Atomic Reference Counted

let data = Arc::new(vec![1, 2, 3]);  // Immutable shared data
let data_clone = Arc::clone(&data);  // Share ownership
// Cannot modify data - thread-safe by default
```

### ✅ Immutable Updates with Struct Update Syntax

```rust
#[derive(Clone)]  // Enable cloning for immutable updates
struct Config {
    host: String,
    port: u16,
    timeout: u64,
}

let config = Config {
    host: "localhost".to_string(),
    port: 8080,
    timeout: 30,
};

// Immutable update - create new instance
let new_config = Config {
    port: 9090,  // Change port
    ..config    // Copy remaining fields from config
};

// config unchanged: port = 8080
// new_config: port = 9090, other fields same as config
```

---

## 5. Function Composition

### ✅ Compose Small Functions

```rust
// Small, focused functions
fn validate_email(email: &str) -> bool {
    email.contains('@') && email.len() > 3
}

fn normalize_email(email: &str) -> String {
    email.to_lowercase().trim().to_string()
}

fn create_user(email: String) -> User {
    User { id: generate_id(), email }
}

// Compose into pipeline
fn register_user(raw_email: &str) -> Option<User> {
    if validate_email(raw_email) {
        let email = normalize_email(raw_email);
        Some(create_user(email))
    } else {
        None
    }
}

// Or using Option combinators
fn register_user_functional(raw_email: &str) -> Option<User> {
    Some(raw_email)
        .filter(|e| validate_email(e))
        .map(normalize_email)
        .map(create_user)
}
```

---

## 6. Railway-Oriented Programming

### ✅ Chain Operations with Result

```rust
use std::fs::File;
use std::io::{self, Read};

// Each step returns Result
fn read_file(path: &str) -> Result<String, io::Error> {
    let mut file = File::open(path)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    Ok(contents)
}

fn parse_json(contents: &str) -> Result<Value, serde_json::Error> {
    serde_json::from_str(contents)
}

fn extract_user(json: Value) -> Result<User, String> {
    json.get("user")
        .ok_or("Missing user field".to_string())
        .and_then(|v| serde_json::from_value(v.clone())
            .map_err(|e| e.to_string()))
}

// Railway-oriented pipeline
fn load_user_from_file(path: &str) -> Result<User, String> {
    read_file(path)
        .map_err(|e| e.to_string())
        .and_then(|contents| parse_json(&contents)
            .map_err(|e| e.to_string()))
        .and_then(extract_user)
}

// Using the ? operator (syntactic sugar for railway)
fn load_user_from_file_sugar(path: &str) -> Result<User, String> {
    let contents = read_file(path).map_err(|e| e.to_string())?;
    let json = parse_json(&contents).map_err(|e| e.to_string())?;
    let user = extract_user(json)?;
    Ok(user)
}
```

---

## 7. Advanced Patterns: Traits as Type Classes

### ✅ Generic Abstractions with Traits

```rust
// Functor trait (simplified)
trait Functor {
    type Item;
    fn map<F, B>(self, f: F) -> Self::Output
    where
        F: FnMut(Self::Item) -> B;
}

// Implement for Option
impl<T> Functor for Option<T> {
    type Item = T;
    type Output<B> = Option<B>;
    
    fn map<F, B>(self, f: F) -> Option<B>
    where
        F: FnMut(T) -> B,
    {
        self.map(f)
    }
}

// Generic function using Functor pattern
fn double_if_present<F: Functor>(container: F) -> F::Output<i32>
where
    F::Item: std::ops::Mul<Output = i32> + From<i32>,
{
    container.map(|x| x * 2)
}

// Usage
let maybe_num = Some(21);
let doubled = double_if_present(maybe_num);  // Some(42)

let no_num: Option<i32> = None;
let doubled_none = double_if_present(no_num);  // None
```

---

## 8. Async FP with Futures

### ✅ Functional Async Patterns

```rust
use futures::{future, Future, TryFutureExt};
use std::io;

// Async function returning Result
async fn fetch_data(url: &str) -> Result<String, io::Error> {
    // Implementation
    Ok("data".to_string())
}

// Compose async operations
async fn process_multiple(urls: &[&str]) -> Result<Vec<String>, io::Error> {
    // Map each URL to a future
    let futures: Vec<_> = urls.iter()
        .map(|url| fetch_data(url))
        .collect();
    
    // Wait for all to complete (fail fast)
    let results = future::try_join_all(futures).await?;
    Ok(results)
}

// Railway-oriented async
async fn load_and_process(url: &str) -> Result<String, io::Error> {
    let data = fetch_data(url).await?;  // ? propagates errors
    Ok(process_data(&data))
}

fn process_data(data: &str) -> String {
    data.to_uppercase()
}
```

---

## 9. Testing FP Code

### ✅ Testing Pure Functions

```rust
// Pure function - easy to test
fn add(a: i32, b: i32) -> i32 {
    a + b
}

#[test]
fn test_add_is_pure() {
    // Same input → same output
    assert_eq!(add(2, 3), 5);
    assert_eq!(add(2, 3), add(2, 3)); // Idempotent
    
    // No side effects
    let result1 = add(2, 3);
    let result2 = add(2, 3);
    assert_eq!(result1, result2);
}

// Test Result types
fn divide(a: f64, b: f64) -> Result<f64, String> {
    if b == 0.0 {
        Err("Division by zero".to_string())
    } else {
        Ok(a / b)
    }
}

#[test]
fn test_divide_handles_errors() {
    assert_eq!(divide(10.0, 2.0), Ok(5.0));
    assert_eq!(divide(10.0, 0.0), Err("Division by zero".to_string()));
}
```

---

## 10. Complete Example: Functional Web Service

```rust
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::{Arc, Mutex};

// Domain types (immutable)
#[derive(Clone, Debug, Serialize, Deserialize)]
struct User {
    id: String,
    email: String,
    name: String,
}

// Error types
#[derive(Debug)]
enum UserError {
    NotFound(String),
    AlreadyExists(String),
    InvalidEmail(String),
}

// Repository trait (pure interface)
trait UserRepository {
    fn find(&self, id: &str) -> Result<Option<User>, UserError>;
    fn save(&self, user: User) -> Result<User, UserError>;
    fn exists(&self, email: &str) -> Result<bool, UserError>;
}

// In-memory implementation
struct InMemoryRepository {
    users: Arc<Mutex<HashMap<String, User>>>,
}

impl UserRepository for InMemoryRepository {
    fn find(&self, id: &str) -> Result<Option<User>, UserError> {
        let users = self.users.lock().unwrap();
        Ok(users.get(id).cloned())
    }
    
    fn save(&self, user: User) -> Result<User, UserError> {
        let mut users = self.users.lock().unwrap();
        users.insert(user.id.clone(), user.clone());
        Ok(user)
    }
    
    fn exists(&self, email: &str) -> Result<bool, UserError> {
        let users = self.users.lock().unwrap();
        Ok(users.values().any(|u| u.email == email))
    }
}

// Pure validation functions
fn validate_email(email: &str) -> Result<String, UserError> {
    if email.contains('@') && email.len() > 3 {
        Ok(email.to_string())
    } else {
        Err(UserError::InvalidEmail("Invalid email format".to_string()))
    }
}

fn validate_user_data(email: &str, name: &str) -> Result<(String, String), UserError> {
    let valid_email = validate_email(email)?;
    if name.len() < 2 {
        return Err(UserError::InvalidEmail("Name too short".to_string()));
    }
    Ok((valid_email, name.to_string()))
}

// Railway-oriented service logic
fn create_user(
    repo: &dyn UserRepository,
    email: &str,
    name: &str,
) -> Result<User, UserError> {
    // Validate input
    let (valid_email, valid_name) = validate_user_data(email, name)?;
    
    // Check if email exists
    if repo.exists(&valid_email)? {
        return Err(UserError::AlreadyExists(valid_email));
    }
    
    // Create and save user
    let user = User {
        id: generate_id(),
        email: valid_email,
        name: valid_name,
    };
    
    repo.save(user)
}

// Usage
fn main() {
    let repo = InMemoryRepository {
        users: Arc::new(Mutex::new(HashMap::new())),
    };
    
    // Create a user (railway-oriented)
    match create_user(&repo, "alice@example.com", "Alice") {
        Ok(user) => println!("Created: {:?}", user),
        Err(e) => println!("Error: {:?}", e),
    }
    
    // Try to create duplicate (will fail)
    match create_user(&repo, "alice@example.com", "Alice") {
        Ok(user) => println!("Created: {:?}", user),
        Err(UserError::AlreadyExists(email)) => {
            println!("User already exists: {}", email)
        }
        Err(e) => println!("Error: {:?}", e),
    }
}

fn generate_id() -> String {
    use uuid::Uuid;
    Uuid::new_v4().to_string()
}
```

---

## Tool-Specific Notes

### For Kimi Users

Kimi provides enhanced Rust FP validation:
- **Parallel Verification**: Verify multiple pure functions simultaneously
- **Borrow Checker Assistance**: Help with lifetime and ownership issues
- **Macro Understanding**: Explain and generate declarative/procedural macros
- **Crate Recommendations**: Suggest optimal FP crates for your use case

Example Kimi-specific pattern:
```rust
// Kimi can verify these properties in parallel:
// - Function purity (no unsafe, no side effects)
// - Proper error handling with Result
// - Ownership and borrowing correctness
// - Iterator usage efficiency
// - Trait bound correctness
```

### For Cursor Users

Cursor provides IDE-integrated Rust support:
- **Inline Suggestions**: Real-time FP pattern recommendations
- **Borrow Checker Hints**: Help resolve lifetime issues
- **Refactoring**: Automated conversion to FP patterns
- **Cargo Integration**: Native IDE support for Cargo.toml

### For Claude Users

Claude excels at Rust code generation:
- **Pattern Generation**: Create complex FP pipelines from descriptions
- **Error Handling**: Automatic Result/Option wrapping
- **Lifetime Management**: Suggest optimal lifetime annotations
- **Best Practices**: Detailed ownership and borrowing guidance

### For Gemini Users

Gemini provides comprehensive Rust FP education:
- **Concept Explanations**: Detailed ownership, borrowing, and lifetime theory
- **Code Examples**: Extensive example libraries
- **Pattern Recognition**: Identify FP opportunities in existing code
- **Crate Ecosystem**: Guide through the Rust FP ecosystem

---

## Quick Reference Card

### Result Patterns

```rust
// Creating Results
Ok(value)           // Success
Err(error)          // Failure

// Transforming Results
result.map(|x| x * 2)           // Transform Ok value
result.map_err(|e| format!("{}", e))  // Transform Err value
result.and_then(|x| Ok(x + 1))  // Chain Result-returning functions

// Chaining
let result = Ok(10)
    .and_then(|x| divide(x, 2))
    .and_then(|x| divide(x, 5));
```

### Option Patterns

```rust
// Creating Options
Some(value)         // Present value
None                // Absent value

// Transforming Options
option.map(|x| x * 2)           // Transform if Some
option.and_then(|x| Some(x + 1)) // Chain Option-returning functions
option.unwrap_or(default)       // Provide default

// Pattern matching
match option {
    Some(value) => println!("Found: {}", value),
    None => println!("Not found"),
}
```

### Iterator Patterns

```rust
// Creating iterators
vec.iter()          // Iterate by reference
vec.iter_mut()      // Iterate by mutable reference
vec.into_iter()     // Consume and iterate by value

// Adapters (lazy)
iter.map(|x| x * 2)
    .filter(|x| x > 10)
    .take(5)
    .skip(2);

// Consumers (eager)
iter.collect::<Vec<_>>()  // Collect into collection
iter.fold(0, |acc, x| acc + x)  // Reduce to single value
iter.find(|x| x > 10)      // Find first match
iter.all(|x| x > 0)        // Check if all match
```

---

## Further Reading

- **Core Principles**: `code_guidelines/principles/functional_programming.md`
- **Rust Patterns**: `code_guidelines/languages/rust/patterns.md`
- **Code Examples**: `code_guidelines/languages/rust/examples.md`
- **Library Guide**: `code_guidelines/languages/rust/libraries.md`

---

**Last Updated**: 2025-11-19  
**Maintained By**: Global AI Rules System  
**Status**: Active  
**Applies To**: All AI assistants (Cursor, Kimi, Claude, Gemini)