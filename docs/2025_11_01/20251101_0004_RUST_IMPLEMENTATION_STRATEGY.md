# Rust Implementation Strategy

**Date**: 2025-11-01  
**Phase**: Phase 0 - Task 0.5  
**Purpose**: Define implementation approach for Rust FP guide

---

## Strategy Overview

Create comprehensive Rust FP support following the proven pattern from Python, TypeScript, Kotlin, and Swift guides. Emphasize Rust's unique strengths (zero-cost abstractions, ownership, memory safety) while maintaining consistency with existing guides.

---

## Section Ordering Strategy

### rust-fp-style-guide.md (18 sections)

**Order Rationale**: Move from basics → error handling → composition → advanced patterns

1. **Header & Quick Links** (foundational)
2. **For Systems and Performance Projects** (context setting)
3. **Required Libraries** (establish dependencies early)
4. **Error Handling (Result/Option)** (fundamental pattern)
5. **Monadic Composition** (building on error handling)
6. **Algebraic Data Types** (enums + structs, before pattern matching)
7. **Pattern Matching** (builds on ADTs)
8. **Iterator Patterns (Foldable)** (core FP pattern)
9. **Collection Patterns (Traversable)** (builds on Iterator)
10. **Ownership & Borrowing** (FP context, after basic patterns)
11. **Immutable Data Structures** (builds on ownership)
12. **Function Composition** (combine patterns)
13. **Type-Driven Development** (guides architecture)
14. **Async Patterns** (tokio/futures)
15. **Parallel Patterns** (rayon)
16. **Real-World Examples** (synthesize all patterns)
17. **Testing Patterns** (verification)
18. **Data Structure Patterns** (cross-reference)
19. **Mandatory Rules Reference** (footer)

**Flow**: Basic → Error Handling → Composition → Advanced → Real-World → Testing

---

## Example Pattern Strategy

### Show Both Owned and Borrowed Versions

**Pattern**: For each major example, show both approaches with notes

```rust
// Owned version (consumes iterator)
let result: Result<Vec<i32>, Error> = numbers
    .into_iter()  // Consuming iterator
    .map(validate)
    .collect();

// Borrowed version (keeps original)
let result: Result<Vec<&i32>, Error> = numbers
    .iter()  // Borrowing iterator
    .map(|n| validate(*n).map(|_| n))
    .collect();
```

**Note**: Explain when to use each approach

---

### Use clone() with Performance Notes

**Pattern**: Use clone when ownership complicates examples, note implications

```rust
// Clone when needed for clarity
let result = items
    .iter()
    .map(|item| process(item.clone()))  // Clone for clarity
    .collect();

// Note: In production, consider:
// - Using references (&T)
// - Restructuring to avoid clones
// - Using Cow (Clone-on-Write) for conditional cloning
```

---

### Emphasize Zero-Cost Abstractions

**Pattern**: Show equivalent manual code, note zero overhead

```rust
// FP style (zero-cost)
let sum: i32 = numbers.iter().filter(|&&x| x > 0).sum();

// Equivalent manual code (same performance)
let mut sum = 0;
for &x in &numbers {
    if x > 0 {
        sum += x;
    }
}

// Note: Both compile to identical machine code!
```

---

### Show Compiler Errors and Fixes

**Pattern**: Show common compiler errors and how to fix them

```rust
// ❌ Won't compile: value used after move
let result1 = process(items);
let result2 = process(items);  // Error: value used after move

// ✅ Fix 1: Clone
let result1 = process(items.clone());
let result2 = process(items);

// ✅ Fix 2: Borrow
let result1 = process(&items);
let result2 = process(&items);
```

---

## Cross-Reference Strategy

### Internal Cross-References

**Pattern**: Link related sections within rust-fp-style-guide.md

```markdown
See [Iterator Patterns](#8-iterator-patterns-foldable) for fold operations.
```

**Where to use**:
- Link from Monadic Composition → Error Handling
- Link from Collection Patterns → Iterator Patterns
- Link from Real-World Examples → all pattern sections
- Link from Testing → all pattern sections

---

### External Cross-References

**Pattern**: Link to other guides and documentation

```markdown
- **Full Guide**: [guides/traversable-foldable-guide.md](guides/traversable-foldable-guide.md#rust-implementation)
- **Quick Reference**: [DATA_STRUCTURE_PATTERNS.md](DATA_STRUCTURE_PATTERNS.md#rust)
- **CURSOR.md Section 8**: [Data Structure Guidelines](CURSOR.md#8-data-structure-guidelines-recommended)
```

**Where to use**:
- Quick Links section
- Data Structure Patterns section
- Mandatory Rules Reference section

---

## Code Example Strategy

### Standard Example Format

```rust
// Pattern: Problem statement in comment
// Problem: Validate user input and transform to domain object

// ❌ Anti-pattern (if applicable)
fn bad_example() { /* ... */ }

// ✅ FP pattern
fn good_example() -> Result<User, ValidationError> {
    // Step-by-step with comments
    data.email
        .validate()? // Early return on error
        .and_then(|email| /* ... */)
        .map(|validated| User::new(validated))
}

// Usage example
match good_example() {
    Ok(user) => println!("User: {:?}", user),
    Err(e) => eprintln!("Error: {}", e),
}
```

### Real-World Example Format

**Pattern**: Complete, runnable examples with full context

```rust
// Full example with imports, types, and usage
use std::result::Result;

#[derive(Debug)]
struct User {
    name: String,
    email: String,
    age: u32,
}

#[derive(Debug)]
enum ValidationError {
    InvalidEmail,
    InvalidAge,
}

fn validate_email(email: &str) -> Result<String, ValidationError> {
    // Validation logic
}

fn validate_age(age: u32) -> Result<u32, ValidationError> {
    // Validation logic
}

fn create_user(
    name: String,
    email: &str,
    age: u32
) -> Result<User, ValidationError> {
    let validated_email = validate_email(email)?;
    let validated_age = validate_age(age)?;
    Ok(User {
        name,
        email: validated_email,
        age: validated_age,
    })
}

// Usage
fn main() {
    match create_user("Alice".to_string(), "alice@example.com", 30) {
        Ok(user) => println!("Created: {:?}", user),
        Err(e) => eprintln!("Error: {:?}", e),
    }
}
```

---

## Dependency Management Strategy

### Cargo.toml Format

**Pattern**: Show minimal dependencies first, then optional enhancements

```toml
[dependencies]
# Core async runtime
tokio = { version = "1.35", features = ["full"] }

# Parallel iterators
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

---

## Ownership Complexity Strategy

### When to Use clone()

**Use clone when**:
- Ownership complicates the example significantly
- The focus is on the FP pattern, not memory management
- Alternative approaches would obscure the main point

**Always note**:
- Performance implications
- Alternative approaches (references, restructuring)
- When cloning is acceptable in production

### When to Show Borrowing

**Show borrowing when**:
- Demonstrating iterator patterns (iter vs into_iter)
- Performance is part of the lesson
- Teaching ownership concepts

### When to Use Cow

**Use Cow when**:
- Demonstrating conditional cloning
- Performance optimization examples
- Advanced patterns

---

## Testing Strategy

### Example Test Format

```rust
#[cfg(test)]
mod tests {
    use super::*;

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

### Property-Based Testing

```rust
use proptest::prelude::*;

proptest! {
    #[test]
    fn test_fold_associative(nums in prop::collection::vec(any::<i32>(), 0..100)) {
        let result1 = nums.iter().fold(0, |acc, x| acc + x);
        let result2 = nums.iter().sum::<i32>();
        prop_assert_eq!(result1, result2);
    }
}
```

---

## Integration Strategy

### CURSOR.md Updates

**Section 8 (Data Structure Guidelines)**:
- Add Rust examples to 8.2 (Foldable)
- Add Rust examples to 8.3 (Traversable)
- Add Rust to 8.4 (Parallel)
- Add Rust to 8.5 (Common Patterns)

**Section 10 (Language-Specific Rules)**:
- Add Rust subsection after Swift
- Link to rust-fp-style-guide.md
- List key libraries (rayon, tokio)
- List key patterns (Result, Option, Iterator)

---

### DATA_STRUCTURE_PATTERNS.md Updates

**Add Rust to all tables**:

| Language | Foldable | Traversable | Parallel |
|----------|----------|-------------|----------|
| **Rust** | `iter().fold()` | `collect::<Result<Vec<_>, _>>()` | `par_iter()` (rayon) |

**Add Rust examples**:
- Quick syntax for fold
- Quick syntax for traverse
- Quick syntax for parallel operations

---

### Auto-Detection Logic

**Update CURSOR.md Section 9.2**:

```markdown
**Detection Logic**:
- Python: `requirements.txt`, `pyproject.toml`, `*.py` files
- TypeScript: `package.json`, `tsconfig.json`, `*.ts`, `*.tsx` files
- Kotlin: `build.gradle.kts`, `*.kt` files
- Swift: `Package.swift`, `*.swift` files
- Rust: `Cargo.toml`, `*.rs` files  # NEW

**Example**: If Cursor detects `Cargo.toml` in root:
```

---

## Quality Assurance Strategy

### Checklist Before Completion

**Code Examples**:
- [ ] All code examples compile
- [ ] All examples follow Rust idioms
- [ ] Ownership is handled correctly
- [ ] Performance notes are accurate
- [ ] Examples are complete and runnable

**Cross-References**:
- [ ] All internal links work
- [ ] All external links work
- [ ] Cross-references are accurate
- [ ] Section numbers are correct

**Consistency**:
- [ ] Formatting matches other guides
- [ ] Section structure consistent
- [ ] Example format consistent
- [ ] Terminology consistent

**Completeness**:
- [ ] All planned sections included
- [ ] All required libraries covered
- [ ] All patterns documented
- [ ] Real-world examples complete

---

## Risk Mitigation

### Risk 1: Ownership Complexity

**Mitigation**:
- Keep examples simple
- Use clone() with notes when needed
- Show both owned and borrowed versions
- Explain trade-offs clearly

### Risk 2: Over-Technical

**Mitigation**:
- Focus on FP patterns, not advanced Rust
- Avoid lifetimes unless necessary
- Skip unsafe code entirely
- Keep examples practical

### Risk 3: Length Creep

**Mitigation**:
- Stick to ~1,000-1,200 lines for guide
- Keep sections focused
- Move advanced topics to "Further Reading"
- Maintain parity with other language guides

---

## Success Metrics

**Quantitative**:
- [ ] rust-fp-style-guide.md: 1,000-1,200 lines
- [ ] Rust T/F section: 800-1,000 lines
- [ ] All 18 sections complete
- [ ] 40+ working code examples
- [ ] All cross-references functional

**Qualitative**:
- [ ] Examples are clear and idiomatic
- [ ] Ownership is explained well
- [ ] Zero-cost abstractions emphasized
- [ ] Consistent with other 4 guides
- [ ] Production-ready patterns

---

## Timeline Adherence

**Phase 1**: 4 hours for rust-fp-style-guide.md
- Average: 15-20 minutes per section
- Longer sections (Real-World, Testing): 30-40 minutes
- Buffer: 30 minutes for review

**Phase 2**: 3 hours for T/F guide Rust section
- Average: 15-20 minutes per subsection
- Longer patterns: 30 minutes
- Buffer: 30 minutes for review

**Phase 3**: 2 hours for integration
- CURSOR.md: 40 minutes
- DATA_STRUCTURE_PATTERNS.md: 30 minutes
- Other updates: 30 minutes
- Review: 20 minutes

**Phase 4**: 1.5 hours for examples
- Project structure: 20 minutes
- .cursorrules template: 30 minutes
- Example code: 20 minutes
- Tests: 15 minutes
- Summary: 15 minutes

---

**Strategy Complete!** Ready for implementation.

