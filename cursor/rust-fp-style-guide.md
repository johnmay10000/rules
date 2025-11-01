# Rust Functional Programming Style Guide

**Version**: 1.0.0  
**Last Updated**: 2025-11-01  
**Part of**: [CURSOR.md](CURSOR.md) Global Rule Set  
**Target**: Rust projects (systems, backend, CLI, WASM, high-performance)

> **📖 Global Rules**: This document extends [CURSOR.md](CURSOR.md) with Rust-specific guidance. For mandatory universal rules (Git, documentation, testing, file size), see [CURSOR.md](CURSOR.md).

---

## Quick Links

- **Mandatory Rules**: See [CURSOR.md](CURSOR.md) sections 1-4
- **FP Principles Deep Dive**: See [CURSOR_FP_PRINCIPLES.md](CURSOR_FP_PRINCIPLES.md)
- **Workflow Guide**: See [CURSOR_WORKFLOW_GUIDE.md](CURSOR_WORKFLOW_GUIDE.md)
- **Data Structure Patterns**: See [guides/traversable-foldable-guide.md](guides/traversable-foldable-guide.md#rust-implementation)
- **Integration**: See [.cursorrules Integration](#cursorrules-integration) below

---

## For Systems and Performance-Critical Projects

### Core Principles

1. **Zero-Cost Abstractions**: FP patterns compile to optimal code with no runtime overhead
2. **Ownership-Aware FP**: Embrace ownership as a feature, use borrowing and cloning strategically
3. **Immutability by Default**: Use `let` bindings, prefer value types
4. **Explicit Error Handling**: Use `Result<T, E>` and `Option<T>` instead of exceptions
5. **Type Safety**: Leverage Rust's strong type system and compiler guarantees
6. **Composability**: Build complex operations from small, pure functions
7. **Memory Safety**: No null pointers, no data races, no undefined behavior

**Unique Rust Strengths**:
- ⭐ Best memory safety (ownership + borrow checker)
- ⭐ Best performance (zero-cost abstractions)
- ⭐ Best for systems programming
- ⭐ Compile-time correctness guarantees

---

## Required Libraries

```toml
# Cargo.toml

[dependencies]
# Core async runtime
tokio = { version = "1.35", features = ["full"] }

# Parallel iterators (data parallelism)
rayon = "1.8"

# Serialization
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"

# Optional: Iterator utilities
itertools = "0.12"

# Optional: Async utilities
futures = "0.3"

[dev-dependencies]
# Property-based testing
proptest = "1.4"

# Benchmarking
criterion = "0.5"
```

**Standard Library** (no external dependencies needed):
```rust
// Core FP primitives (always available)
use std::iter::Iterator;
use std::result::Result;
use std::option::Option;

// Error handling
use std::error::Error;

// Collections
use std::collections::{HashMap, HashSet, BTreeMap};
```

---

## 1. Error Handling with Result and Option

### ❌ Avoid: Panic and Unwrap

```rust
// BAD: Panics on error
fn divide(a: f64, b: f64) -> f64 {
    if b == 0.0 {
        panic!("Division by zero!");
    }
    a / b
}

// BAD: Unwrap can panic
fn parse_age(s: &str) -> u32 {
    s.parse().unwrap()  // Panics on invalid input
}
```

### ✅ Prefer: Result Type

```rust
#[derive(Debug)]
enum MathError {
    DivisionByZero,
    Overflow,
}

impl std::fmt::Display for MathError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            MathError::DivisionByZero => write!(f, "Division by zero"),
            MathError::Overflow => write!(f, "Overflow"),
        }
    }
}

impl std::error::Error for MathError {}

// GOOD: Returns Result
fn divide(a: f64, b: f64) -> Result<f64, MathError> {
    if b == 0.0 {
        Err(MathError::DivisionByZero)
    } else {
        Ok(a / b)
    }
}

// Usage
match divide(10.0, 2.0) {
    Ok(result) => println!("Result: {}", result),
    Err(e) => eprintln!("Error: {}", e),
}
```

### ✅ Use the ? Operator

```rust
fn calculate(a: f64, b: f64, c: f64) -> Result<f64, MathError> {
    let x = divide(a, b)?;  // Early return on error
    let y = divide(x, c)?;  // Propagates error up
    Ok(y)
}

// Equivalent to:
fn calculate_explicit(a: f64, b: f64, c: f64) -> Result<f64, MathError> {
    match divide(a, b) {
        Ok(x) => match divide(x, c) {
            Ok(y) => Ok(y),
            Err(e) => Err(e),
        },
        Err(e) => Err(e),
    }
}
```

### ✅ Option for Nullable Values

```rust
fn find_user(id: u64) -> Option<User> {
    // Returns Some(user) if found, None if not
    database.get(&id).cloned()
}

// Chaining with map
let username = find_user(42)
    .map(|user| user.name)
    .unwrap_or_else(|| "Unknown".to_string());

// Using ? with Option
fn get_user_email(id: u64) -> Option<String> {
    let user = find_user(id)?;  // Returns None if user not found
    Some(user.email)
}
```

---

## 2. Monadic Composition

### ✅ Chain Operations with and_then

```rust
fn validate_age(age: u32) -> Result<u32, ValidationError> {
    if age > 0 && age < 150 {
        Ok(age)
    } else {
        Err(ValidationError::InvalidAge)
    }
}

fn validate_email(email: &str) -> Result<String, ValidationError> {
    if email.contains('@') {
        Ok(email.to_string())
    } else {
        Err(ValidationError::InvalidEmail)
    }
}

// Monadic composition with and_then (flatMap)
fn create_user(
    name: String,
    email: &str,
    age: u32
) -> Result<User, ValidationError> {
    validate_email(email).and_then(|valid_email| {
        validate_age(age).map(|valid_age| User {
            name,
            email: valid_email,
            age: valid_age,
        })
    })
}

// Or using ? operator (cleaner)
fn create_user_with_operator(
    name: String,
    email: &str,
    age: u32
) -> Result<User, ValidationError> {
    let valid_email = validate_email(email)?;
    let valid_age = validate_age(age)?;
    Ok(User {
        name,
        email: valid_email,
        age: valid_age,
    })
}
```

### ✅ Railway-Oriented Programming

```rust
// Result forms a "railway" - success track or error track
fn process_user_data(data: &UserData) -> Result<ProcessedUser, Error> {
    parse_data(data)?           // Exit on error
        .validate()?            // Exit on error
        .transform()?           // Exit on error
        .enrich()?              // Exit on error
        .finalize()             // Returns Result
}

// All operations succeed → Ok(ProcessedUser)
// Any operation fails → Err(Error) immediately
```

### ✅ Combining Results

```rust
// Collect multiple Results into single Result
fn validate_all(items: Vec<String>) -> Result<Vec<u32>, ValidationError> {
    items
        .into_iter()
        .map(|s| s.parse::<u32>())
        .collect::<Result<Vec<_>, _>>()
        .map_err(|_| ValidationError::ParseError)
}

// Or using try_fold for early exit
fn sum_valid(items: Vec<String>) -> Result<u32, ValidationError> {
    items
        .iter()
        .try_fold(0, |acc, s| {
            s.parse::<u32>()
                .map(|n| acc + n)
                .map_err(|_| ValidationError::ParseError)
        })
}
```

---

## 3. Algebraic Data Types (ADTs)

### ✅ Enums as Sum Types

```rust
// Sum type: A value is ONE of these variants
#[derive(Debug, Clone)]
enum PaymentMethod {
    CreditCard { number: String, cvv: String },
    BankTransfer { account: String, routing: String },
    Crypto { address: String, chain: String },
}

// Pattern matching is exhaustive (compiler-checked)
fn process_payment(method: PaymentMethod, amount: f64) -> Result<Receipt, PaymentError> {
    match method {
        PaymentMethod::CreditCard { number, cvv } => {
            process_credit_card(&number, &cvv, amount)
        }
        PaymentMethod::BankTransfer { account, routing } => {
            process_bank_transfer(&account, &routing, amount)
        }
        PaymentMethod::Crypto { address, chain } => {
            process_crypto(&address, &chain, amount)
        }
    }
    // Compiler ensures all variants are handled!
}
```

### ✅ Structs as Product Types

```rust
// Product type: A value contains ALL of these fields
#[derive(Debug, Clone)]
struct User {
    id: u64,
    name: String,
    email: String,
    age: u32,
}

// Combine sum and product types
#[derive(Debug)]
enum UserStatus {
    Active { since: DateTime, logins: u64 },
    Suspended { reason: String, until: DateTime },
    Deleted { when: DateTime },
}
```

### ✅ Newtype Pattern

```rust
// Type-safe wrappers (zero runtime cost)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct UserId(u64);

#[derive(Debug, Clone, PartialEq, Eq)]
struct Email(String);

// Can't accidentally mix up types
fn get_user(id: UserId) -> Option<User> {
    // id.0 to access inner value
    database.get(&id.0)
}

// Won't compile: can't pass u64 where UserId expected
// let user = get_user(42);  // ERROR!
let user = get_user(UserId(42));  // OK!
```

---

## 4. Pattern Matching

### ✅ Match Expressions (Exhaustive)

```rust
fn describe_result<T, E>(result: Result<T, E>) -> String {
    match result {
        Ok(_) => "Success".to_string(),
        Err(_) => "Failure".to_string(),
    }
    // Compiler ensures both variants are handled
}

// Nested pattern matching
fn process_response(response: ApiResponse) -> String {
    match response {
        ApiResponse::Success { data, status: 200 } => {
            format!("OK: {}", data)
        }
        ApiResponse::Success { data, status } => {
            format!("Success {}: {}", status, data)
        }
        ApiResponse::Error { message, code: 404 } => {
            format!("Not found: {}", message)
        }
        ApiResponse::Error { message, code } => {
            format!("Error {}: {}", code, message)
        }
    }
}
```

### ✅ Guards and Destructuring

```rust
fn categorize_number(n: i32) -> &'static str {
    match n {
        n if n < 0 => "negative",
        0 => "zero",
        n if n < 10 => "small positive",
        n if n < 100 => "medium positive",
        _ => "large positive",
    }
}

// Destructuring complex types
fn handle_user(user: User) -> String {
    match user {
        User { name, age, email } if age < 18 => {
            format!("{} (minor) - {}", name, email)
        }
        User { name, age: 18..=64, email } => {
            format!("{} (adult) - {}", name, email)
        }
        User { name, email, .. } => {
            format!("{} (senior) - {}", name, email)
        }
    }
}
```

### ✅ if let and while let

```rust
// if let for single pattern
if let Some(user) = find_user(42) {
    println!("Found: {}", user.name);
}

// Equivalent to:
match find_user(42) {
    Some(user) => println!("Found: {}", user.name),
    None => {}
}

// while let for iteration
let mut stack = vec![1, 2, 3];
while let Some(value) = stack.pop() {
    println!("{}", value);
}
```

---

## 5. Iterator Patterns (Foldable)

### ✅ Basic Iterator Operations

```rust
let numbers = vec![1, 2, 3, 4, 5];

// Sum (zero-cost abstraction)
let sum: i32 = numbers.iter().sum();

// Product
let product: i32 = numbers.iter().product();

// Fold (most general)
let sum_fold = numbers.iter().fold(0, |acc, x| acc + x);

// Map and collect
let doubled: Vec<i32> = numbers.iter().map(|x| x * 2).collect();

// Filter and collect
let evens: Vec<i32> = numbers.iter().filter(|&&x| x % 2 == 0).cloned().collect();
```

### ✅ Consuming vs Borrowing Iterators

```rust
let numbers = vec![1, 2, 3, 4, 5];

// Borrowing iterator (can reuse numbers)
let sum1: i32 = numbers.iter().sum();
let sum2: i32 = numbers.iter().sum();  // OK! numbers still valid

// Consuming iterator (moves numbers)
let doubled: Vec<i32> = numbers.into_iter().map(|x| x * 2).collect();
// Can't use numbers anymore - it was moved

// Mutable iterator (for mutation)
let mut numbers = vec![1, 2, 3];
numbers.iter_mut().for_each(|x| *x *= 2);
// numbers is now [2, 4, 6]
```

### ✅ Custom Iterators

```rust
struct Fibonacci {
    curr: u64,
    next: u64,
}

impl Fibonacci {
    fn new() -> Self {
        Fibonacci { curr: 0, next: 1 }
    }
}

impl Iterator for Fibonacci {
    type Item = u64;

    fn next(&mut self) -> Option<Self::Item> {
        let current = self.curr;
        self.curr = self.next;
        self.next = current + self.next;
        Some(current)
    }
}

// Usage (zero-cost abstraction!)
let fib_sum: u64 = Fibonacci::new()
    .take(10)
    .sum();
```

### ✅ Fold with Early Exit

```rust
use itertools::Itertools;

// Fold that can exit early
let result = numbers.iter().fold_while(0, |acc, &x| {
    if acc + x > 100 {
        itertools::FoldWhile::Done(acc)
    } else {
        itertools::FoldWhile::Continue(acc + x)
    }
}).into_inner();
```

---

## 6. Collection Patterns (Traversable)

### ✅ collect() with Result

```rust
// Validate all items, stop at first error
fn validate_all(items: Vec<String>) -> Result<Vec<u32>, ParseIntError> {
    items
        .into_iter()
        .map(|s| s.parse::<u32>())
        .collect()  // collect::<Result<Vec<_>, _>>()
}
// If any parse fails, returns Err immediately
// If all succeed, returns Ok(Vec<u32>)

// Usage
match validate_all(vec!["1".into(), "2".into(), "3".into()]) {
    Ok(numbers) => println!("All valid: {:?}", numbers),
    Err(e) => eprintln!("Parse error: {}", e),
}
```

### ✅ collect() with Option

```rust
fn find_all_users(ids: Vec<u64>) -> Option<Vec<User>> {
    ids
        .into_iter()
        .map(|id| find_user(id))
        .collect()  // collect::<Option<Vec<_>>>()
}
// If any user not found, returns None
// If all found, returns Some(Vec<User>)
```

### ✅ FromIterator Trait

```rust
use std::iter::FromIterator;

// Custom collection implementing FromIterator
#[derive(Debug)]
struct MyCollection<T>(Vec<T>);

impl<T> FromIterator<T> for MyCollection<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        MyCollection(iter.into_iter().collect())
    }
}

// Now can use collect()
let my_coll: MyCollection<i32> = vec![1, 2, 3].into_iter().collect();
```

### ✅ Partition and Group

```rust
// Partition by predicate
let numbers = vec![1, 2, 3, 4, 5, 6];
let (evens, odds): (Vec<_>, Vec<_>) = numbers
    .into_iter()
    .partition(|&x| x % 2 == 0);

// Group by key (requires itertools)
use itertools::Itertools;

let items = vec!["apple", "apricot", "banana", "blueberry"];
let grouped = items
    .into_iter()
    .group_by(|s| s.chars().next().unwrap())
    .into_iter()
    .map(|(key, group)| (key, group.collect::<Vec<_>>()))
    .collect::<Vec<_>>();
```

---

## 7. Ownership & Borrowing (FP Context)

### ✅ Immutable by Default

```rust
// Immutable binding (default)
let x = 42;
// x = 43;  // ERROR: cannot assign twice to immutable variable

// Mutable binding (explicit)
let mut y = 42;
y = 43;  // OK

// Prefer immutable for FP
fn calculate(input: i32) -> i32 {
    let intermediate = input * 2;  // Immutable
    let result = intermediate + 10;  // Immutable
    result
}
```

### ✅ Move Semantics

```rust
// By default, values are moved
let s1 = String::from("hello");
let s2 = s1;  // s1 moved to s2
// println!("{}", s1);  // ERROR: value used after move

// Use references to avoid move
let s1 = String::from("hello");
let len = calculate_length(&s1);  // Borrow, not move
println!("{}", s1);  // OK! s1 still valid

fn calculate_length(s: &String) -> usize {
    s.len()  // Read-only access
}
```

### ✅ Clone When Needed

```rust
// Clone when ownership complicates composition
let items = vec![1, 2, 3, 4, 5];

// Option 1: Clone (simple but allocates)
let doubled: Vec<i32> = items
    .clone()  // Clone to avoid move
    .into_iter()
    .map(|x| x * 2)
    .collect();

println!("Original: {:?}", items);  // OK! items still valid

// Option 2: Borrow (no allocation, better performance)
let doubled: Vec<i32> = items
    .iter()  // Borrow iterator
    .map(|&x| x * 2)  // Dereference
    .collect();

// Note: Clone is acceptable for clarity in non-critical paths
// Optimize later if needed
```

### ✅ Cow (Clone-on-Write)

```rust
use std::borrow::Cow;

// Conditional cloning: only clone if mutation needed
fn process_string(s: Cow<str>) -> Cow<str> {
    if s.contains("old") {
        Cow::Owned(s.replace("old", "new"))  // Clone + modify
    } else {
        s  // No clone, return as-is
    }
}

// Usage
let s1 = Cow::Borrowed("hello world");
let result1 = process_string(s1);  // No clone

let s2 = Cow::Borrowed("old world");
let result2 = process_string(s2);  // Clones
```

---

## 8. Immutable Data Structures

### ✅ Persistent Data Structures

```rust
// Standard collections (mutable, but use immutably)
let mut users = HashMap::new();
users.insert(1, User::new("Alice"));
users.insert(2, User::new("Bob"));

// Create new map instead of mutating
fn add_user(users: HashMap<u64, User>, user: User) -> HashMap<u64, User> {
    let mut new_users = users.clone();
    new_users.insert(user.id, user);
    new_users
}

// Functional update
let users2 = add_user(users, User::new("Charlie"));
```

### ✅ Builder Pattern for Immutability

```rust
#[derive(Debug, Clone)]
struct Config {
    host: String,
    port: u16,
    timeout: u64,
}

impl Config {
    fn new(host: String) -> Self {
        Config {
            host,
            port: 8080,
            timeout: 30,
        }
    }

    // Functional updates (return new instance)
    fn with_port(self, port: u16) -> Self {
        Config { port, ..self }
    }

    fn with_timeout(self, timeout: u64) -> Self {
        Config { timeout, ..self }
    }
}

// Usage (fluent API)
let config = Config::new("localhost".into())
    .with_port(3000)
    .with_timeout(60);
```

### ✅ Copy Types (Zero Cost)

```rust
// Types that implement Copy are duplicated, not moved
#[derive(Debug, Clone, Copy)]
struct Point {
    x: i32,
    y: i32,
}

let p1 = Point { x: 1, y: 2 };
let p2 = p1;  // Copy, not move
println!("{:?} {:?}", p1, p2);  // Both valid!

// Copy works for small, stack-only types
// Don't implement Copy for types with heap data (String, Vec, etc.)
```

---

## 9. Function Composition

### ✅ compose and pipe Functions

```rust
// Manual composition
fn compose<A, B, C, F, G>(f: F, g: G) -> impl Fn(A) -> C
where
    F: Fn(A) -> B,
    G: Fn(B) -> C,
{
    move |x| g(f(x))
}

// Usage
let add_one = |x: i32| x + 1;
let double = |x: i32| x * 2;
let add_one_then_double = compose(add_one, double);

assert_eq!(add_one_then_double(5), 12);  // (5 + 1) * 2

// Pipe (reverse composition)
fn pipe<A, B, C, F, G>(f: F, g: G) -> impl Fn(A) -> C
where
    F: Fn(A) -> B,
    G: Fn(B) -> C,
{
    move |x| g(f(x))
}
```

### ✅ Method Chaining

```rust
// Iterator chains (zero-cost)
let result = numbers
    .iter()
    .filter(|&&x| x > 0)
    .map(|&x| x * 2)
    .take(10)
    .sum::<i32>();

// Result chains
let result = parse_data(input)?
    .validate()?
    .transform()?
    .save()?;
```

### ✅ Currying (Manual)

```rust
// Currying: Transform f(a, b) → f(a) → f(b)
fn add(a: i32) -> impl Fn(i32) -> i32 {
    move |b| a + b
}

let add_5 = add(5);
assert_eq!(add_5(3), 8);

// Generic currying
fn curry<A, B, C, F>(f: F) -> impl Fn(A) -> Box<dyn Fn(B) -> C>
where
    A: 'static,
    F: Fn(A, B) -> C + 'static,
{
    move |a| Box::new(move |b| f(a, b))
}
```

---

## 10. Async Patterns (tokio/futures)

### ✅ Basic async/await

```rust
use tokio;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let result = fetch_data().await?;
    println!("Fetched: {}", result);
    Ok(())
}

async fn fetch_data() -> Result<String, reqwest::Error> {
    let response = reqwest::get("https://api.example.com/data").await?;
    let body = response.text().await?;
    Ok(body)
}
```

### ✅ Concurrent Operations (join!)

```rust
use tokio;

async fn fetch_multiple() -> Result<(User, Profile, Settings), Error> {
    // Run all three concurrently
    let (user, profile, settings) = tokio::join!(
        fetch_user(42),
        fetch_profile(42),
        fetch_settings(42)
    );

    Ok((user?, profile?, settings?))
}

// try_join! for early exit on error
async fn fetch_multiple_try() -> Result<(User, Profile, Settings), Error> {
    tokio::try_join!(
        fetch_user(42),
        fetch_profile(42),
        fetch_settings(42)
    )
}
```

### ✅ Stream Processing

```rust
use futures::stream::{self, StreamExt};

async fn process_stream() {
    let items = vec![1, 2, 3, 4, 5];
    
    let mut stream = stream::iter(items)
        .map(|x| async move { x * 2 })
        .buffer_unordered(5);  // Process up to 5 concurrently

    while let Some(result) = stream.next().await {
        println!("Result: {}", result);
    }
}

// Collect stream results
async fn collect_stream() -> Vec<i32> {
    let items = vec![1, 2, 3, 4, 5];
    
    stream::iter(items)
        .then(|x| async move { x * 2 })
        .collect()
        .await
}
```

### ✅ FuturesUnordered (Dynamic Concurrency)

```rust
use futures::stream::{FuturesUnordered, StreamExt};

async fn fetch_all_users(ids: Vec<u64>) -> Vec<Result<User, Error>> {
    let futures: FuturesUnordered<_> = ids
        .into_iter()
        .map(|id| fetch_user(id))
        .collect();

    futures.collect().await
}

// With error handling
async fn fetch_all_users_checked(ids: Vec<u64>) -> Result<Vec<User>, Error> {
    let futures: FuturesUnordered<_> = ids
        .into_iter()
        .map(|id| fetch_user(id))
        .collect();

    let results: Vec<_> = futures.collect().await;
    results.into_iter().collect()  // Stops at first error
}
```

---

## 11. Parallel Patterns (rayon)

### ✅ Parallel Iterators

```rust
use rayon::prelude::*;

// Parallel sum (automatic work stealing)
let numbers: Vec<i32> = (1..=1000).collect();
let sum: i32 = numbers.par_iter().sum();

// Parallel map
let doubled: Vec<i32> = numbers
    .par_iter()
    .map(|&x| x * 2)
    .collect();

// Parallel filter
let evens: Vec<i32> = numbers
    .par_iter()
    .filter(|&&x| x % 2 == 0)
    .cloned()
    .collect();
```

### ✅ Parallel fold

```rust
use rayon::prelude::*;

// Parallel fold (associative operation)
let sum = numbers
    .par_iter()
    .fold(|| 0, |acc, &x| acc + x)
    .sum::<i32>();

// Custom parallel reduction
let max = numbers
    .par_iter()
    .fold(|| i32::MIN, |max, &x| max.max(x))
    .reduce(|| i32::MIN, |a, b| a.max(b));
```

### ✅ When to Use Parallel

```rust
// Use parallel when:
// ✅ CPU-bound operations
// ✅ Large collections (>1000 items)
// ✅ Independent computations (no shared mutable state)
// ✅ Operations take >1ms each

// Example: Image processing
let processed: Vec<Image> = images
    .par_iter()
    .map(|img| process_image(img))  // CPU-intensive
    .collect();

// Don't use parallel for:
// ❌ I/O-bound operations (use async instead)
// ❌ Small collections (<100 items)
// ❌ Fast operations (<1μs each)
// ❌ Operations with synchronization overhead
```

### ✅ Parallel collect with Results

```rust
use rayon::prelude::*;

fn process_all(items: Vec<Item>) -> Result<Vec<Processed>, Error> {
    items
        .par_iter()
        .map(|item| process_item(item))
        .collect()  // Stops at first error
}

// All results or all errors
fn process_all_partition(items: Vec<Item>) -> (Vec<Processed>, Vec<Error>) {
    let results: Vec<_> = items
        .par_iter()
        .map(|item| process_item(item))
        .collect();

    results
        .into_iter()
        .partition_map(|r| match r {
            Ok(v) => itertools::Either::Left(v),
            Err(e) => itertools::Either::Right(e),
        })
}
```

---

## 12. Type-Driven Development

### ✅ Types First, Implementation Second

```rust
// 1. Define types first
#[derive(Debug)]
struct User {
    id: UserId,
    email: Email,
    age: Age,
}

#[derive(Debug)]
struct UserId(u64);

#[derive(Debug)]
struct Email(String);

#[derive(Debug)]
struct Age(u32);

#[derive(Debug)]
enum ValidationError {
    InvalidEmail,
    InvalidAge,
}

// 2. Write function signatures
fn validate_email(s: &str) -> Result<Email, ValidationError>;
fn validate_age(n: u32) -> Result<Age, ValidationError>;
fn create_user(id: u64, email: &str, age: u32) -> Result<User, ValidationError>;

// 3. Let compiler guide implementation
fn create_user(id: u64, email: &str, age: u32) -> Result<User, ValidationError> {
    let validated_email = validate_email(email)?;
    let validated_age = validate_age(age)?;
    Ok(User {
        id: UserId(id),
        email: validated_email,
        age: validated_age,
    })
}
```

### ✅ Phantom Types

```rust
use std::marker::PhantomData;

// State machine with phantom types
struct Locked;
struct Unlocked;

struct Door<State> {
    _state: PhantomData<State>,
}

impl Door<Locked> {
    fn new() -> Self {
        Door { _state: PhantomData }
    }

    fn unlock(self) -> Door<Unlocked> {
        Door { _state: PhantomData }
    }
}

impl Door<Unlocked> {
    fn open(self) {
        println!("Door opened");
    }

    fn lock(self) -> Door<Locked> {
        Door { _state: PhantomData }
    }
}

// Usage (compiler enforces state)
let door = Door::<Locked>::new();
// door.open();  // ERROR: no method `open` on Door<Locked>
let unlocked = door.unlock();
unlocked.open();  // OK!
```

---

## 13. Real-World Examples

### Example 1: User Registration with Validation

```rust
use std::result::Result;

#[derive(Debug)]
struct User {
    id: u64,
    username: String,
    email: String,
    age: u32,
}

#[derive(Debug)]
enum RegistrationError {
    InvalidUsername(String),
    InvalidEmail(String),
    InvalidAge(String),
    UserExists,
}

impl std::fmt::Display for RegistrationError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            RegistrationError::InvalidUsername(msg) => write!(f, "Invalid username: {}", msg),
            RegistrationError::InvalidEmail(msg) => write!(f, "Invalid email: {}", msg),
            RegistrationError::InvalidAge(msg) => write!(f, "Invalid age: {}", msg),
            RegistrationError::UserExists => write!(f, "User already exists"),
        }
    }
}

impl std::error::Error for RegistrationError {}

fn validate_username(username: &str) -> Result<String, RegistrationError> {
    if username.len() < 3 {
        Err(RegistrationError::InvalidUsername("Too short".into()))
    } else if username.len() > 20 {
        Err(RegistrationError::InvalidUsername("Too long".into()))
    } else {
        Ok(username.to_string())
    }
}

fn validate_email(email: &str) -> Result<String, RegistrationError> {
    if !email.contains('@') {
        Err(RegistrationError::InvalidEmail("Missing @".into()))
    } else {
        Ok(email.to_string())
    }
}

fn validate_age(age: u32) -> Result<u32, RegistrationError> {
    if age < 13 {
        Err(RegistrationError::InvalidAge("Too young".into()))
    } else if age > 120 {
        Err(RegistrationError::InvalidAge("Invalid".into()))
    } else {
        Ok(age)
    }
}

fn register_user(
    id: u64,
    username: &str,
    email: &str,
    age: u32
) -> Result<User, RegistrationError> {
    let valid_username = validate_username(username)?;
    let valid_email = validate_email(email)?;
    let valid_age = validate_age(age)?;

    // Check if user exists (simulated)
    if user_exists(id) {
        return Err(RegistrationError::UserExists);
    }

    Ok(User {
        id,
        username: valid_username,
        email: valid_email,
        age: valid_age,
    })
}

// Usage
fn main() {
    match register_user(1, "alice", "alice@example.com", 25) {
        Ok(user) => println!("Registered: {:?}", user),
        Err(e) => eprintln!("Registration failed: {}", e),
    }
}
```

### Example 2: ETL Pipeline

```rust
#[derive(Debug)]
struct RawRecord {
    data: String,
}

#[derive(Debug)]
struct ParsedRecord {
    id: u64,
    value: f64,
}

#[derive(Debug)]
struct ValidatedRecord {
    id: u64,
    value: f64,
}

#[derive(Debug)]
struct EnrichedRecord {
    id: u64,
    value: f64,
    category: String,
    timestamp: String,
}

fn parse_record(raw: RawRecord) -> Result<ParsedRecord, EtlError> {
    // Parse CSV or JSON
    let parts: Vec<&str> = raw.data.split(',').collect();
    let id = parts.get(0)
        .and_then(|s| s.parse().ok())
        .ok_or(EtlError::ParseError)?;
    let value = parts.get(1)
        .and_then(|s| s.parse().ok())
        .ok_or(EtlError::ParseError)?;

    Ok(ParsedRecord { id, value })
}

fn validate_record(parsed: ParsedRecord) -> Result<ValidatedRecord, EtlError> {
    if parsed.value < 0.0 {
        Err(EtlError::ValidationError("Negative value".into()))
    } else {
        Ok(ValidatedRecord {
            id: parsed.id,
            value: parsed.value,
        })
    }
}

fn enrich_record(validated: ValidatedRecord) -> Result<EnrichedRecord, EtlError> {
    Ok(EnrichedRecord {
        id: validated.id,
        value: validated.value,
        category: categorize(validated.value),
        timestamp: chrono::Utc::now().to_rfc3339(),
    })
}

// Full pipeline
fn etl_pipeline(raw_records: Vec<RawRecord>) -> Result<Vec<EnrichedRecord>, EtlError> {
    raw_records
        .into_iter()
        .map(parse_record)
        .collect::<Result<Vec<_>, _>>()?
        .into_iter()
        .map(validate_record)
        .collect::<Result<Vec<_>, _>>()?
        .into_iter()
        .map(enrich_record)
        .collect()
}

// Parallel version
use rayon::prelude::*;

fn etl_pipeline_parallel(raw_records: Vec<RawRecord>) -> Result<Vec<EnrichedRecord>, EtlError> {
    raw_records
        .par_iter()
        .map(|r| {
            parse_record(r.clone())
                .and_then(validate_record)
                .and_then(enrich_record)
        })
        .collect()
}
```

### Example 3: HTTP API Client with Retries

```rust
use tokio;
use reqwest;

#[derive(Debug)]
enum ApiError {
    NetworkError(reqwest::Error),
    RateLimited,
    NotFound,
    ServerError,
}

async fn fetch_with_retry<T, F>(
    f: F,
    max_retries: u32
) -> Result<T, ApiError>
where
    F: Fn() -> tokio::task::JoinHandle<Result<T, ApiError>>,
{
    let mut attempts = 0;
    loop {
        match f().await {
            Ok(Ok(value)) => return Ok(value),
            Ok(Err(e)) if attempts < max_retries => {
                attempts += 1;
                tokio::time::sleep(tokio::time::Duration::from_millis(100 * attempts as u64)).await;
            }
            Ok(Err(e)) => return Err(e),
            Err(_) => return Err(ApiError::ServerError),
        }
    }
}

async fn get_user(id: u64) -> Result<User, ApiError> {
    let response = reqwest::get(format!("https://api.example.com/users/{}", id))
        .await
        .map_err(ApiError::NetworkError)?;

    match response.status() {
        reqwest::StatusCode::OK => {
            response.json().await.map_err(ApiError::NetworkError)
        }
        reqwest::StatusCode::NOT_FOUND => Err(ApiError::NotFound),
        reqwest::StatusCode::TOO_MANY_REQUESTS => Err(ApiError::RateLimited),
        _ => Err(ApiError::ServerError),
    }
}
```

---

## 14. Testing Patterns

### ✅ Unit Tests

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_divide_success() {
        let result = divide(10.0, 2.0);
        assert_eq!(result, Ok(5.0));
    }

    #[test]
    fn test_divide_by_zero() {
        let result = divide(10.0, 0.0);
        assert!(result.is_err());
        match result {
            Err(MathError::DivisionByZero) => {}
            _ => panic!("Expected DivisionByZero error"),
        }
    }

    #[test]
    fn test_validate_email_valid() {
        let result = validate_email("test@example.com");
        assert!(result.is_ok());
    }

    #[test]
    fn test_validate_email_invalid() {
        let result = validate_email("invalid");
        assert!(result.is_err());
    }
}
```

### ✅ Property-Based Testing

```rust
use proptest::prelude::*;

proptest! {
    #[test]
    fn test_divide_commutative(a in -1000.0..1000.0, b in -1000.0..1000.0) {
        if b != 0.0 {
            let result1 = divide(a, b);
            let result2 = divide(a, b);
            prop_assert_eq!(result1.is_ok(), result2.is_ok());
        }
    }

    #[test]
    fn test_parse_roundtrip(n in 0u32..1000) {
        let s = n.to_string();
        let parsed = s.parse::<u32>().unwrap();
        prop_assert_eq!(n, parsed);
    }

    #[test]
    fn test_fold_associative(nums in prop::collection::vec(any::<i32>(), 0..100)) {
        let result1 = nums.iter().fold(0, |acc, x| acc + x);
        let result2 = nums.iter().sum::<i32>();
        prop_assert_eq!(result1, result2);
    }
}
```

### ✅ Benchmarking

```rust
// benches/my_benchmark.rs
use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn fibonacci_recursive(n: u64) -> u64 {
    match n {
        0 => 0,
        1 => 1,
        n => fibonacci_recursive(n - 1) + fibonacci_recursive(n - 2),
    }
}

fn fibonacci_iterative(n: u64) -> u64 {
    let mut a = 0;
    let mut b = 1;
    for _ in 0..n {
        let temp = a;
        a = b;
        b = temp + b;
    }
    a
}

fn benchmark_fibonacci(c: &mut Criterion) {
    c.bench_function("fib recursive 20", |b| {
        b.iter(|| fibonacci_recursive(black_box(20)))
    });

    c.bench_function("fib iterative 20", |b| {
        b.iter(|| fibonacci_iterative(black_box(20)))
    });
}

criterion_group!(benches, benchmark_fibonacci);
criterion_main!(benches);
```

---

## Data Structure Patterns (Rust)

**For Foldable and Traversable patterns in Rust**, see:

- **Quick Reference**: [DATA_STRUCTURE_PATTERNS.md](DATA_STRUCTURE_PATTERNS.md#rust) - Fast lookup for common patterns
- **Full Guide**: [guides/traversable-foldable-guide.md](guides/traversable-foldable-guide.md#rust-implementation) - Comprehensive guide with examples
- **CURSOR.md Section 8**: [Data Structure Guidelines](CURSOR.md#8-data-structure-guidelines-recommended)

### When to Use

✅ **Use Foldable** (fold/reduce) when:
- Aggregating collections: sum, product, concat
- Converting between collection types
- Building accumulations

✅ **Use Traversable** (collect) when:
- Validating collections with early exit
- Performing effects on collections (IO, async)
- Need "all-or-nothing" semantics
- Parallel operations on collections

### Rust Implementation

**Foldable** (native):
```rust
// Sum numbers (zero-cost abstraction)
let total: i32 = numbers.iter().fold(0, |acc, n| acc + n);

// Or using sum
let total: i32 = numbers.iter().sum();

// Custom fold
let result = items.iter().fold(Vec::new(), |mut acc, item| {
    acc.push(process(item));
    acc
});
```

**Traversable** (native collect):
```rust
// Validate all items, early exit on first error
let validated: Result<Vec<i32>, String> = numbers
    .into_iter()
    .map(validate_positive)
    .collect();
// collect() stops at first Err! ⭐ Native Traversable support
```

**Parallel** (rayon):
```rust
use rayon::prelude::*;

// Parallel fold (zero-cost, automatic work stealing)
let sum: i32 = numbers.par_iter().sum();

// Parallel map + collect
let results: Vec<Processed> = items
    .par_iter()
    .map(|item| process(item))
    .collect();
```

**Async** (tokio):
```rust
use futures::future::try_join_all;

// Parallel async operations
let futures: Vec<_> = ids.iter().map(|id| fetch_user(*id)).collect();
let users = try_join_all(futures).await?;
// All operations run concurrently
```

### Common Patterns

**Form Validation** (all fields must pass):
```rust
fn validate_form(data: &FormData) -> Result<User, ValidationError> {
    let name = validate_name(&data.name)?;
    let email = validate_email(&data.email)?;
    let age = validate_age(data.age)?;
    
    Ok(User { name, email, age })
}
```

**ETL Pipeline** (parse → validate → enrich):
```rust
fn etl_pipeline(raw: Vec<RawRecord>) -> Result<Vec<EnrichedRecord>, Error> {
    raw.into_iter()
        .map(parse_record)
        .collect::<Result<Vec<_>, _>>()?
        .into_iter()
        .map(validate_record)
        .collect::<Result<Vec<_>, _>>()?
        .into_iter()
        .map(enrich_record)
        .collect()
}
```

**Parallel API Calls**:
```rust
use rayon::prelude::*;

// CPU-bound: Use rayon
let results: Vec<Processed> = items
    .par_iter()
    .map(|item| expensive_computation(item))
    .collect();

// I/O-bound: Use tokio
let users = try_join_all(ids.iter().map(|id| fetch_user(*id))).await?;
```

### Why Rust Excels

**Zero-Cost Abstractions** ⭐⭐⭐⭐⭐:
- Iterator chains compile to optimal machine code
- No runtime overhead for FP patterns
- As fast as hand-written loops

**Native Traversable** ⭐⭐⭐⭐⭐:
- `collect()` with `Result`/`Option` is built-in
- Better than Python, comparable to TypeScript
- Early exit semantics automatic

**Performance** ⭐⭐⭐⭐⭐:
- Fastest of all 5 languages
- No garbage collection
- Predictable performance

See the [full guide](guides/traversable-foldable-guide.md#rust-implementation) for comprehensive examples and patterns.

---

## Mandatory Rules Reference

From [CURSOR.md](CURSOR.md):

1. **Git Checkpoints** (Section 1) - Commit every 30-60 min
2. **Documentation** (Section 2) - 3-tier hierarchy
3. **Testing** (Section 3) - Comprehensive coverage, all passing
4. **File Size** (Section 4) - 250-300 lines maximum

See [CURSOR.md](CURSOR.md) for complete details.

---

**Version**: 1.0.0  
**Last Updated**: 2025-11-01  
**Maintained By**: Global Rules Repository

---

This guide transforms Rust into an excellent functional programming language while leveraging its unique strengths: zero-cost abstractions, ownership system, and memory safety.

