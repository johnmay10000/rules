# Rust FP Example Project

**Purpose**: Demonstrate Rust functional programming patterns with Cursor rules

This example project shows how to structure a Rust project following the global Cursor rules with emphasis on:
- ⭐ Zero-cost abstractions
- ⭐ Railway-oriented programming (? operator)
- ⭐ Foldable patterns (fold, sum, product)
- ⭐ Traversable patterns (collect with Result/Option)
- ⭐ Parallel operations (rayon)
- ⭐ Async operations (tokio)
- ⭐ Comprehensive testing

## Project Structure

```
rust_project/
├── Cargo.toml              # Dependencies and project metadata
├── README.md               # This file
├── .cursorrules            # Cursor rules (references global rules)
├── src/
│   ├── main.rs             # Entry point with examples
│   ├── lib.rs              # Library root (public API)
│   └── types.rs            # Type definitions and errors
└── tests/
    └── integration_test.rs # Integration tests
```

## Quick Start

```bash
# Build project
cargo build

# Run all tests
cargo test

# Run with output
cargo run

# Run tests with output
cargo test -- --nocapture

# Check code
cargo clippy -- -D warnings

# Format code
cargo fmt
```

## Examples Demonstrated

### 1. Railway-Oriented Programming

```rust
async fn process_data_pipeline(input: &str) -> Result<String, AppError> {
    let parsed = parse_input(input)?;      // Exit on error
    let validated = validate_data(&parsed)?; // Exit on error
    let transformed = transform_data(&validated)?; // Exit on error
    let result = save_result(&transformed).await?; // Exit on error
    Ok(result)
}
```

### 2. Foldable (Reduce/Fold)

```rust
let numbers = vec![1, 2, 3, 4, 5];
let sum: i32 = numbers.iter().sum(); // 15
let product = numbers.iter().fold(1, |acc, &x| acc * x); // 120
```

### 3. Traversable (collect with Result)

```rust
fn validate_all(numbers: &[i32]) -> Result<Vec<i32>, AppError> {
    numbers
        .iter()
        .map(|&n| validate_positive(n))
        .collect()  // Stops at first Err! Native Traversable!
}
```

### 4. Parallel Processing (rayon)

```rust
use rayon::prelude::*;

fn parallel_compute(numbers: &[i32]) -> Result<i32, AppError> {
    let sum: i32 = numbers
        .par_iter()
        .map(|&x| x * 2)
        .sum();
    Ok(sum)
}
```

### 5. Async Operations (tokio)

```rust
async fn fetch_multiple_async() -> Result<Vec<String>, AppError> {
    let futures: Vec<_> = ids
        .iter()
        .map(|&id| fetch_data(id))
        .collect();
    
    try_join_all(futures).await // All parallel, early exit on error
}
```

## Key Patterns

### Error Handling
- ✅ Use `Result<T, E>` for all fallible operations
- ✅ Use `Option<T>` for nullable values
- ✅ Use `?` operator for early returns
- ✅ Custom error types with `thiserror`
- ✅ No `.unwrap()` except in tests

### Performance
- ✅ Zero-cost abstractions (FP = imperative performance)
- ✅ Use rayon for CPU-bound parallel
- ✅ Use tokio for I/O-bound async
- ✅ Iterators are lazy (zero allocation until collect)

### Testing
- ✅ Unit tests in each module
- ✅ Integration tests in `tests/`
- ✅ Property-based tests with quickcheck
- ✅ 100% test coverage

## Dependencies

```toml
[dependencies]
tokio = { version = "1.35", features = ["full"] }
rayon = "1.8"
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"
thiserror = "1.0"
futures = "0.3"

[dev-dependencies]
quickcheck = "1.0"
```

## See Also

- [CURSOR.md](../../CURSOR.md) - Mandatory universal rules
- [rust-fp-style-guide.md](../../rust-fp-style-guide.md) - Rust FP patterns
- [DATA_STRUCTURE_PATTERNS.md](../../DATA_STRUCTURE_PATTERNS.md) - Foldable/Traversable reference

## License

This example is part of the Cursor Global Rules repository.

