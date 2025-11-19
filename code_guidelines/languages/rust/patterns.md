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

Quick reference for common functional programming patterns in Rust. For detailed explanations, see `fp_style_guide.md`.

---

## Error Handling Patterns

### Result Type for Explicit Errors

Wrap success/failure in a type-safe container. `Ok` for success, `Err` for failure.

```rust
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
enum DivisionError {
    DivideByZero,
    InvalidInput(String),
}

impl fmt::Display for DivisionError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            DivisionError::DivideByZero => write!(f, "Cannot divide by zero"),
            DivisionError::InvalidInput(msg) => write!(f, "Invalid input: {}", msg),
        }
    }
}

// Function that might fail
fn divide(a: f64, b: f64) -> Result<f64, DivisionError> {
    if b == 0.0 {
        Err(DivisionError::DivideByZero)
    } else {
        Ok(a / b)
    }
}

// Usage
let result1 = divide(10.0, 2.0);  // Ok(5.0)
let result2 = divide(10.0, 0.0);  // Err(DivideByZero)

// Transform with map
let doubled = result1.map(|x| x * 2.0);  // Ok(10.0)
let doubled_err = result2.map(|x| x * 2.0);  // Err(DivideByZero)

// Chain with and_then
let chained = divide(10.0, 2.0)
    .and_then(|x| divide(x, 2.0));  // Ok(2.5)

// Early return with ?
fn calculate(x: f64, y: f64) -> Result<f64, DivisionError> {
    let quotient = divide(x, y)?;
    Ok(quotient * 2.0)
}
```

**When to use**: Any operation that can fail. Replaces exceptions with type safety.

---

### Option Type for Nullable Values

Represent values that might be absent with `Some` or `None`.

```rust
// Safe head that returns Option
fn safe_head<T>(list: &[T]) -> Option<&T> {
    list.first()
}

// Safe lookup in a slice of key-value pairs
fn lookup<'a>(key: &str, pairs: &[(&'a str, &'a str)]) -> Option<&'a str> {
    pairs.iter()
        .find(|(k, _)| k == &key)
        .map(|(_, v)| *v)
}

// Usage
let numbers = vec![1, 2, 3];
let head1 = safe_head(&numbers);  // Some(&1)
let head2 = safe_head(&[]);       // None

// Compose Option functions
fn get_first_user_email(users: &[(&str, &str)]) -> Option<&str> {
    safe_head(users).and_then(|(name, email)| {
        if name.is_empty() {
            None
        } else {
            Some(*email)
        }
    })
}

// Usage with pattern matching
match head1 {
    Some(val) => println!("First element: {}", val),
    None => println!("List is empty"),
}

// Or use if let
if let Some(email) = get_first_user_email(&[("Alice", "alice@example.com")]) {
    println!("Email: {}", email);
}
```

**When to use**: When a value might not exist. More lightweight than Result when no error details needed.

---

## Iterator Patterns

### Functional Iteration

Rust's iterators are lazy and composable.

```rust
// Map: transform each element
let numbers = vec![1, 2, 3, 4, 5];
let doubled: Vec<i32> = numbers.iter().map(|x| x * 2).collect();
// [2, 4, 6, 8, 10]

// Filter: keep elements that satisfy predicate
let evens: Vec<i32> = numbers.iter().filter(|x| x % 2 == 0).copied().collect();
// [2, 4]

// Fold: reduce to single value
let sum: i32 = numbers.iter().fold(0, |acc, x| acc + x);
// 15

// Find: get first matching element
let first_even = numbers.iter().find(|x| x % 2 == 0);
// Some(&2)

// Chain iterators
let result: Vec<i32> = numbers.iter()
    .filter(|x| x % 2 == 0)
    .map(|x| x * 2)
    .collect();
// [4, 8]

// Custom iterator adapters
trait IteratorExt: Iterator {
    fn filter_map_ok<T, F>(self, f: F) -> impl Iterator<Item = Result<T, Self::Item>>
    where
        Self: Sized,
        Self::Item: Result<T, E>,
        F: FnMut(T) -> Result<T, E>,
        E: From<Self::Item>,
    {
        self.map(move |item| item.and_then(&mut f))
    }
}
```

**When to use**: For transforming, filtering, and aggregating collections. Lazy evaluation avoids intermediate allocations.

---

### Iterator Chaining and Method Chaining

Build complex pipelines from simple operations.

```rust
#[derive(Debug, Clone)]
struct User {
    name: String,
    age: u32,
    email: String,
    active: bool,
}

// Process a collection of users
fn process_users(users: &[User]) -> Vec<String> {
    users.iter()
        .filter(|u| u.active)                    // Only active users
        .filter(|u| u.age >= 18)                 // Only adults
        .map(|u| u.email.clone())                // Extract emails
        .filter(|email| email.contains('@'))     // Validate emails
        .collect()
}

// Complex aggregation
fn user_stats(users: &[User]) -> (u32, f64) {
    let (total_age, count) = users.iter()
        .filter(|u| u.active)
        .map(|u| u.age as u64)
        .fold((0u64, 0u32), |(sum, count), age| (sum + age, count + 1));
    
    let avg_age = if count > 0 {
        total_age as f64 / count as f64
    } else {
        0.0
    };
    
    (count, avg_age)
}

// Find specific user
fn find_admin(users: &[User]) -> Option<&User> {
    users.iter()
        .find(|u| u.email.starts_with("admin@"))
}
```

**When to use**: To build declarative data processing pipelines. Each step is a transformation.

---

## Function Composition Patterns

### Compose Small Functions

Build complex functions from simple ones.

```rust
// Small, focused functions
fn validate_email(email: &str) -> bool {
    email.contains('@') && email.contains('.')
}

fn normalize_email(email: &str) -> String {
    email.trim().to_lowercase()
}

fn is_business_email(email: &str) -> bool {
    email.ends_with(".com") || email.ends_with(".org")
}

// Compose them
fn process_email(email: &str) -> Option<String> {
    let normalized = normalize_email(email);
    
    if validate_email(&normalized) && is_business_email(&normalized) {
        Some(normalized)
    } else {
        None
    }
}

// Using the ? operator for early return
fn create_user(name: &str, email: &str) -> Result<User, String> {
    let normalized_email = normalize_email(email);
    
    if !validate_email(&normalized_email) {
        return Err("Invalid email".to_string());
    }
    
    Ok(User {
        name: name.to_string(),
        email: normalized_email,
    })
}
```

**When to use**: To build complex logic from small, reusable, testable functions.

---

### Railway-Oriented Programming

Chain operations that can fail, branching on success/failure.

```rust
#[derive(Debug)]
enum ValidationError {
    InvalidEmail(String),
    InvalidName(String),
}

struct User {
    name: String,
    email: String,
}

// Validation functions that return Result
fn validate_name(name: &str) -> Result<String, ValidationError> {
    let trimmed = name.trim();
    if trimmed.is_empty() {
        Err(ValidationError::InvalidName("Name cannot be empty".to_string()))
    } else if trimmed.len() < 2 {
        Err(ValidationError::InvalidName("Name too short".to_string()))
    } else {
        Ok(trimmed.to_string())
    }
}

fn validate_email(email: &str) -> Result<String, ValidationError> {
    let normalized = email.trim().to_lowercase();
    if normalized.contains('@') && normalized.contains('.') {
        Ok(normalized)
    } else {
        Err(ValidationError::InvalidEmail("Invalid email format".to_string()))
    }
}

// Railway-oriented pipeline
fn create_user_pipeline(name: &str, email: &str) -> Result<User, ValidationError> {
    validate_name(name)
        .and_then(|valid_name| {
            validate_email(email)
                .map(|valid_email| User {
                    name: valid_name,
                    email: valid_email,
                })
        })
}

// Using the ? operator (cleaner)
fn create_user_railway(name: &str, email: &str) -> Result<User, ValidationError> {
    let valid_name = validate_name(name)?;
    let valid_email = validate_email(email)?;
    
    Ok(User {
        name: valid_name,
        email: valid_email,
    })
}

// Usage
fn main() {
    match create_user_railway("Alice", "alice@example.com") {
        Ok(user) => println!("Created: {:?}", user),
        Err(e) => println!("Error: {:?}", e),
    }
    
    match create_user_railway("", "invalid-email") {
        Ok(user) => println!("Created: {:?}", user),
        Err(e) => println!("Error: {:?}", e),  // Multiple errors, fails fast
    }
}
```

**When to use**: For validation pipelines and business logic with multiple steps that can fail.

---

## Immutable Data Patterns

### Use Immutable by Default

Rust variables are immutable by default. Use `mut` sparingly.

```rust
// Immutable by default
let x = 5;
// x = 6;  // ERROR!

// Explicitly mutable
let mut y = 5;
y = 6;  // OK

// Immutable data structures
#[derive(Debug, Clone)]
struct Config {
    host: String,
    port: u16,
    debug: bool,
}

// Return new instance for updates
impl Config {
    fn with_host(&self, host: String) -> Self {
        Config {
            host,
            port: self.port,
            debug: self.debug,
        }
    }
    
    fn with_port(&self, port: u16) -> Self {
        Config {
            host: self.host.clone(),
            port,
            debug: self.debug,
        }
    }
}

// Usage
let config = Config {
    host: "localhost".to_string(),
    port: 8080,
    debug: false,
};

let new_config = config.with_host("example.com".to_string());
// config is unchanged, new_config has new host
```

**When to use**: Always prefer immutability. Use `mut` only when necessary for performance or ergonomics.

---

### Immutable Updates with Struct Update Syntax

Create new instances based on existing ones.

```rust
#[derive(Debug, Clone)]
struct GameState {
    player_pos: (i32, i32),
    score: u32,
    inventory: Vec<String>,
    health: u32,
}

// Update operations return new state
impl GameState {
    fn move_player(&self, dx: i32, dy: i32) -> Self {
        let (x, y) = self.player_pos;
        GameState {
            player_pos: (x + dx, y + dy),
            ..self.clone()  // Copy all other fields
        }
    }
    
    fn add_score(&self, points: u32) -> Self {
        GameState {
            score: self.score + points,
            ..self.clone()
        }
    }
    
    fn take_damage(&self, damage: u32) -> Self {
        let new_health = self.health.saturating_sub(damage);
        GameState {
            health: new_health,
            ..self.clone()
        }
    }
    
    fn add_item(&self, item: String) -> Self {
        let mut new_inventory = self.inventory.clone();
        new_inventory.push(item);
        
        GameState {
            inventory: new_inventory,
            ..self.clone()
        }
    }
}

// Usage
let initial_state = GameState {
    player_pos: (0, 0),
    score: 0,
    inventory: vec!["sword".to_string()],
    health: 100,
};

let after_move = initial_state.move_player(1, 1);
let after_score = after_move.add_score(100);
let after_damage = after_score.take_damage(20);
let final_state = after_damage.add_item("potion".to_string());

// Each intermediate state is preserved
println!("Initial: {:?}", initial_state);
println!("Final: {:?}", final_state);
```

**When to use**: For state transitions in games, simulations, or any stateful computation that benefits from immutability.

---

## Advanced Patterns

### Generic Functional Patterns

Write functions that work over any type.

```rust
// Functor-like map for Option
fn option_map<T, U, F>(opt: Option<T>, f: F) -> Option<U>
where
    F: FnOnce(T) -> U,
{
    match opt {
        Some(x) => Some(f(x)),
        None => None,
    }
}

// Functor-like map for Result
fn result_map<T, U, E, F>(res: Result<T, E>, f: F) -> Result<U, E>
where
    F: FnOnce(T) -> U,
{
    match res {
        Ok(x) => Ok(f(x)),
        Err(e) => Err(e),
    }
}

// Traverse: apply function to each element, collect results
fn traverse_option<T, U, F>(items: Vec<T>, mut f: F) -> Option<Vec<U>>
where
    F: FnMut(T) -> Option<U>,
{
    let mut results = Vec::with_capacity(items.len());
    for item in items {
        results.push(f(item)?);
    }
    Some(results)
}

// Usage
let numbers = vec![Some(1), Some(2), Some(3)];
let sum: Option<i32> = traverse_option(numbers, |x| x).map(|v| v.iter().sum());
// Some(6)

let numbers_with_none = vec![Some(1), None, Some(3)];
let sum_none = traverse_option(numbers_with_none, |x| x);
// None
```

**When to use**: When building reusable functional abstractions that work across different types.

---

### Fold for Custom Types

Implement fold for your own data structures.

```rust
enum Tree<T> {
    Leaf(T),
    Node(Box<Tree<T>>, Box<Tree<T>>),
}

impl<T> Tree<T> {
    fn fold<F, R>(&self, f_leaf: &F, f_node: &dyn Fn(R, R) -> R) -> R
    where
        F: Fn(&T) -> R,
    {
        match self {
            Tree::Leaf(value) => f_leaf(value),
            Tree::Node(left, right) => {
                let left_result = left.fold(f_leaf, f_node);
                let right_result = right.fold(f_leaf, f_node);
                f_node(left_result, right_result)
            }
        }
    }
    
    fn sum(&self) -> i32
    where
        T: Into<i32> + Copy,
    {
        self.fold(
            &|x| (*x).into(),
            &|a, b| a + b,
        )
    }
    
    fn size(&self) -> usize {
        self.fold(
            &|_| 1,
            &|a, b| a + b,
        )
    }
}

// Usage
let tree = Tree::Node(
    Box::new(Tree::Node(
        Box::new(Tree::Leaf(1)),
        Box::new(Tree::Leaf(2)),
    )),
    Box::new(Tree::Leaf(3)),
);

println!("Sum: {}", tree.sum());     // 6
println!("Size: {}", tree.size());   // 3
```

**When to use**: For aggregating data in custom recursive data structures.

---

## Testing Patterns

### Testing Pure Functions

Pure functions are easy to test - no setup, no mocking.

```rust
// Pure function: same input always gives same output
fn add(a: i32, b: i32) -> i32 {
    a + b
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_add_is_pure() {
        // Same input always same output
        assert_eq!(add(2, 3), 5);
        assert_eq!(add(2, 3), 5);  // Always 5
        
        // Test properties
        assert_eq!(add(0, 5), 5);  // Identity
        assert_eq!(add(5, 0), 5);
        assert_eq!(add(2, 3), add(3, 2));  // Commutative
    }
    
    #[test]
    fn test_divide_result() {
        assert_eq!(divide(10.0, 2.0), Ok(5.0));
        assert_eq!(divide(10.0, 0.0), Err(DivisionError::DivideByZero));
    }
    
    #[test]
    fn test_railway_composition() {
        let user = create_user_railway("Alice", "alice@example.com");
        assert!(user.is_ok());
        
        let invalid = create_user_railway("", "bad-email");
        assert!(invalid.is_err());
    }
}
```

**When to use**: For all pure functions. Write property-based tests when possible.

---

## Summary

These patterns form the core of functional programming in Rust:

1. **Error Handling**: Use `Result` for errors, `Option` for optional values
2. **Iteration**: Use iterator adapters for transformations
3. **Composition**: Compose small functions into larger ones
4. **Immutability**: Prefer immutability, use `mut` sparingly
5. **Updates**: Return new instances for state changes
6. **Generics**: Write reusable code with generic types
7. **Testing**: Test pure functions easily without mocks

**Key Principle**: Leverage the type system and ownership model to write safe, composable code.

---

**Last Updated**: 2025-11-19  
**Maintained By**: Global Rules System  
**Status**: Active