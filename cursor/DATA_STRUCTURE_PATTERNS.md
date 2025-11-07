# Data Structure Patterns: Quick Reference Guide

**Purpose**: Quick lookup for Foldable/Traversable patterns across Haskell, Python, TypeScript, Kotlin, Swift, and Rust.

**Full Guide**: See [guides/traversable-foldable-guide.md](guides/traversable-foldable-guide.md) for comprehensive details (4,800+ lines covering all 6 languages, with Haskell as the reference implementation).

---

## When to Use This Guide

**Apply these patterns when:**
- ✅ Designing data structure access and transformations
- ✅ Implementing collection validation with early exit
- ✅ Building async/parallel operations on collections
- ✅ Creating data pipelines (ETL, processing)
- ✅ Validating forms across multiple fields
- ✅ Aggregating API responses

**Consider for:**
- Data flow through modules
- Collection update strategies
- Error handling in transformations
- Parallel operation design

---

## Decision Tree

```
Need to work with collections/data structures?
    ↓
    YES
    ↓
Need to aggregate/reduce? (sum, product, concat)
    ↓
    YES → Use FOLDABLE
    │     - Haskell: foldr/foldl, sum, product (REFERENCE IMPL!)
    │     - Python: reduce, foldr
    │     - TypeScript: Array.reduce, fp-ts
    │     - Kotlin: fold/reduce, Arrow
    │     - Swift: reduce, reduce(into:)
    │     - Rust: fold, sum (ZERO-COST!)
    │
Need to transform with effects? (validation, IO, async)
    ↓
    YES → Use TRAVERSABLE
    │     - Haskell: traverse, sequenceA (REFERENCE IMPL!)
    │     - Python: custom traverse with Result
    │     - TypeScript: fp-ts traverse, Effect
    │     - Kotlin: Arrow traverse with Either/IO
    │     - Swift: custom traverse with Result/async
    │     - Rust: collect() with Result/Option (NATIVE!)
    │
Need validation with early exit?
    ↓
    YES → Use TRAVERSE with Result/Either
    │     - Stops at first error
    │     - All-or-nothing semantics
    │
Need parallel async operations?
    ↓
    YES → Use PARALLEL TRAVERSE
          - Rust: tokio/rayon (FASTEST!)
          - Swift: TaskGroup (BEST ERGONOMICS!)
          - Kotlin: Arrow parTraverse
          - TypeScript: Effect parallel
          - Python: asyncio + custom
```

---

## Quick Syntax Reference

### Foldable (Reduce/Fold)

| Language | Syntax | Example |
|----------|--------|---------|
| **Haskell** 🎩 | `foldr f z xs`, `sum`, `product` | `sum [1,2,3]  -- 6` |
| **Python** | `reduce(f, iterable, initial)` | `reduce(lambda acc, x: acc + x, [1,2,3], 0)` |
| **TypeScript** | `array.reduce(f, initial)` | `[1,2,3].reduce((acc, x) => acc + x, 0)` |
| **Kotlin** | `list.fold(initial) { acc, x -> ... }` | `listOf(1,2,3).fold(0) { acc, x -> acc + x }` |
| **Swift** | `array.reduce(initial, f)` | `[1,2,3].reduce(0, +)` |
| **Rust** 🦀 | `iter.fold(initial, f)` | `vec![1,2,3].iter().fold(0, \|acc, x\| acc + x)` |

### Traversable (Transform with Effects)

| Language | Approach | Example |
|----------|----------|---------|
| **Haskell** 🎩 | **Native typeclass** | `traverse validatePositive [1,2,3]  -- Just [1,2,3]` |
| **Python** | Custom function | `traverse(items, validate)` |
| **TypeScript** | fp-ts/Effect | `A.traverse(E.Applicative)(validate)(items)` |
| **Kotlin** | Arrow | `items.traverse(Either.applicative()) { validate(it) }` |
| **Swift** | Extension | `items.traverse(validate)` |
| **Rust** 🦀 | **Native collect()** | `items.into_iter().map(validate).collect::<Result<Vec<_>, _>>()` |

### Parallel Operations

| Language | Syntax | Performance |
|----------|--------|-------------|
| **Rust** 🦀 | `items.par_iter().map(f).collect()` (rayon) | ⭐⭐⭐⭐⭐ **Fastest!** |
| **Swift** | `items.traverseParallel(f)` with TaskGroup | ⭐⭐⭐⭐⭐ **Best Ergonomics!** |
| **Haskell** 🎩 | `parTraverse strat f items` (Strategies) | ⭐⭐⭐⭐ Excellent |
| **Kotlin** | `items.parTraverse { f(it) }` | ⭐⭐⭐⭐ Excellent |
| **TypeScript** | `Effect.all(items.map(f), { concurrency: "unbounded" })` | ⭐⭐⭐⭐ Excellent |
| **Python** | `asyncio.gather(*[f(x) for x in items])` | ⭐⭐⭐ Good |

---

## Common Use Cases

### Use Case 1: Validate Collection

**Problem**: Validate all items, fail fast on first error

**Solution**: Traverse with Result/Either

```python
# Python
def validate(items):
    for item in items:
        result = validate_item(item)
        if isinstance(result, Failure):
            return result
    return Success(items)
```

```typescript
// TypeScript
const validated = pipe(
  items,
  A.traverse(E.Applicative)(validateItem)
)
```

```kotlin
// Kotlin
val validated = items.traverse(Either.applicative()) { validateItem(it) }
```

```swift
// Swift
let validated = items.traverse(validateItem)
```

```rust
// Rust (NATIVE!)
let validated: Result<Vec<_>, _> = items
    .into_iter()
    .map(|item| validate_item(item))
    .collect();
// collect() stops at first Err! Zero-cost!
```

---

### Use Case 2: Aggregate Collection

**Problem**: Sum, product, or combine collection elements

**Solution**: Foldable

```python
# Python
from functools import reduce
total = reduce(lambda acc, x: acc + x, numbers, 0)
```

```typescript
// TypeScript
const total = numbers.reduce((acc, x) => acc + x, 0)
```

```kotlin
// Kotlin
val total = numbers.fold(0) { acc, x -> acc + x }
```

```swift
// Swift
let total = numbers.reduce(0, +)
```

```rust
// Rust
let total = numbers.iter().fold(0, |acc, x| acc + x);
// Or more concise:
let total: i32 = numbers.iter().sum();
```

---

### Use Case 3: Parallel API Calls

**Problem**: Fetch multiple resources in parallel

**Solution**: Parallel Traverse

```python
# Python
import asyncio
results = await asyncio.gather(*[fetch(id) for id in ids])
```

```typescript
// TypeScript
const results = await Effect.all(
  ids.map(id => fetch(id)),
  { concurrency: "unbounded" }
)
```

```kotlin
// Kotlin
val results = ids.parTraverse { fetch(it) }
```

```swift
// Swift (BEST ERGONOMICS!)
let results = await ids.traverseParallel { await fetch($0) }
// Uses TaskGroup
```

```rust
// Rust (BEST PERFORMANCE!)
use futures::future::try_join_all;

let futures: Vec<_> = ids.iter().map(|&id| fetch(id)).collect();
let results = try_join_all(futures).await?;
// All parallel, zero-cost abstractions!
```

---

### Use Case 4: Form Validation

**Problem**: Validate multiple form fields, all must pass

**Solution**: Traverse with Result/Either

```python
# Python
def validate_form(name, email, age):
    results = [
        validate_name(name),
        validate_email(email),
        validate_age(age)
    ]
    # Check all succeeded
    for r in results:
        if isinstance(r, Failure):
            return r
    return Success(User(name, email, age))
```

```typescript
// TypeScript
const validateForm = (data) =>
  pipe(
    [validateName(data.name), validateEmail(data.email), validateAge(data.age)],
    A.sequence(E.Applicative),
    E.map(([name, email, age]) => ({ name, email, age }))
  )
```

```kotlin
// Kotlin
fun validateForm(data: FormData): Either<Error, User> =
    listOf(
        validateName(data.name),
        validateEmail(data.email),
        validateAge(data.age)
    ).sequence(Either.applicative())
     .map { (name, email, age) -> User(name, email, age) }
```

```swift
// Swift
func validateForm(name: String, email: String, age: Int) -> Result<User, Error> {
    validateName(name).flatMap { validName in
        validateEmail(email).flatMap { validEmail in
            validateAge(age).map { validAge in
                User(name: validName, email: validEmail, age: validAge)
            }
        }
    }
}
```

```rust
// Rust (? operator for railway-oriented programming!)
fn validate_form(name: &str, email: &str, age: u32) -> Result<User, ValidationError> {
    let valid_name = validate_name(name)?;    // Exit on error
    let valid_email = validate_email(email)?;  // Exit on error
    let valid_age = validate_age(age)?;        // Exit on error
    
    Ok(User {
        name: valid_name,
        email: valid_email,
        age: valid_age,
    })
}
```

---

## Language-Specific Recommendations

### Python
- **Foldable**: Use `reduce()` from functools
- **Traversable**: Implement custom with `returns` library
- **Parallel**: Use `asyncio.gather()`
- **Libraries**: `returns` for Result, `toolz` for utilities
- **Best for**: Simple reductions, basic FP patterns

### TypeScript
- **Foldable**: Use `Array.reduce()` or fp-ts
- **Traversable**: Use fp-ts or Effect
- **Parallel**: Effect with structured concurrency
- **Libraries**: `Effect` (modern) or `fp-ts` (Haskell-like)
- **Best for**: Full FP abstractions, type-safe patterns

### Kotlin
- **Foldable**: Use native `fold()`/`reduce()` or Arrow
- **Traversable**: Use Arrow (mandatory for traverse)
- **Parallel**: Use `parTraverse` from arrow-fx-coroutines
- **Libraries**: `Arrow` (standard FP library)
- **Best for**: JVM, Android, full typeclass support

### Swift
- **Foldable**: Use native `reduce()` ⭐
- **Traversable**: Custom extensions with Result
- **Parallel**: Use `TaskGroup` ⭐⭐⭐⭐⭐ **BEST ERGONOMICS!**
- **Libraries**: Native first, Bow for advanced FP
- **Best for**: iOS/macOS, SwiftUI, great async/await

### Rust 🦀
- **Foldable**: Use `Iterator` trait (fold, sum, reduce) ⭐⭐⭐⭐⭐
- **Traversable**: Use `collect()` with Result/Option ⭐⭐⭐⭐⭐ **NATIVE!**
- **Parallel**: Use `rayon` (par_iter) or `tokio` (async) ⭐⭐⭐⭐⭐ **FASTEST!**
- **Libraries**: std (zero dependencies!), rayon, tokio, futures
- **Best for**: Systems programming, performance-critical code, zero-cost FP

**Rust Strengths**:
- ⭐⭐⭐⭐⭐ Zero-cost abstractions (FP without overhead)
- ⭐⭐⭐⭐⭐ Best performance (fastest of all 5 languages)
- ⭐⭐⭐⭐⭐ Memory safety (ownership + borrow checker)
- ⭐⭐⭐⭐⭐ Native Traversable (collect! with Result/Option)

---

## Performance Guidelines

### When to Use Parallel

**Use parallel traverse when:**
- ✅ Operations are I/O-bound (API calls, file reads)
- ✅ Operations are independent (no shared state)
- ✅ Collection has 5+ items
- ✅ Individual operations take >10ms

**Avoid parallel when:**
- ❌ Operations are CPU-bound on single core
- ❌ Operations share mutable state
- ❌ Collection is small (<5 items)
- ❌ Individual operations are fast (<5ms)

### Performance Ranking

**Parallel Operations** (fastest to slowest):
1. 🥇 **Rust** - rayon/tokio (zero-cost, 6x speedup, no GC) ⭐⭐⭐⭐⭐
2. 🥈 **Swift** - TaskGroup (native, great ergonomics) ⭐⭐⭐⭐⭐
3. 🥉 **Kotlin** - parTraverse (coroutines, JVM overhead)
4. **TypeScript** - Effect (structured concurrency, V8 JIT)
5. **Python** - asyncio.gather (single-threaded, slowest)

**Sequential Operations**:
1. 🥇 **Rust** - Iterator (zero-cost, fastest)
2. 🥈 **Swift** - reduce (no GC, fast)
3. 🥉 **Kotlin** - fold (JVM overhead)
4. **TypeScript** - Array.reduce (V8 JIT)
5. **Python** - reduce (slowest)

---

## Anti-Patterns

### ❌ Don't: Nested Loops for Validation

```python
# BAD - manual nested loops
def validate_all(items):
    valid_items = []
    for item in items:
        if not is_valid(item):
            return Error("Invalid")
        valid_items.append(item)
    return Success(valid_items)
```

### ✅ Do: Use Traverse

```python
# GOOD - use traverse
def validate_all(items):
    return traverse(items, validate_item)
```

---

### ❌ Don't: Manual Parallel Coordination

```typescript
// BAD - manual promise management
const results = []
for (const id of ids) {
  results.push(fetch(id))
}
const resolved = await Promise.all(results)
```

### ✅ Do: Use Parallel Traverse

```typescript
// GOOD - structured parallel traverse
const results = await ids.traverseParallel(fetch)
```

---

### ❌ Don't: Accumulate with forEach

```kotlin
// BAD - imperative accumulation
var sum = 0
numbers.forEach { sum += it }
```

### ✅ Do: Use Fold

```kotlin
// GOOD - functional fold
val sum = numbers.fold(0) { acc, n -> acc + n }
```

---

## Testing Patterns

### Test Foldable Laws

```python
# Test: fold left and right should give same result for commutative ops
def test_fold_commutative():
    numbers = [1, 2, 3, 4, 5]
    assert foldl(add, numbers, 0) == foldr(add, numbers, 0)
```

### Test Traversable Laws

```kotlin
// Test: traverse with identity should return identity
fun testTraverseIdentity() {
    val list = listOf(1, 2, 3)
    val result = list.traverse(Id.applicative()) { it.toId() }
    assertEquals(list.toId(), result)
}
```

---

## Common Errors & Solutions

### Error: "Early Exit Not Happening"

**Problem**: Traverse continues after error

**Solution**: Ensure you're returning failure immediately

```swift
// Make sure to return .failure, not continue
guard condition else {
    return .failure(error)  // Stop here!
}
```

---

### Error: "Parallel Ops Not Actually Parallel"

**Problem**: Operations run sequentially despite "parallel" code

**Solution**: Check you're using the right parallel construct

```swift
// BAD - still sequential
for id in ids {
    let result = await fetch(id)  // Waits for each
}

// GOOD - truly parallel
let results = await ids.traverseParallel { await fetch($0) }
```

---

### Error: "Type Inference Failing"

**Problem**: Compiler can't infer types

**Solution**: Add explicit type annotations

```typescript
// Add explicit type to help inference
const result: Either<Error, number[]> = traverse(items, validate)
```

---

## Further Reading

**Comprehensive Guide**: [traversable-foldable-guide.md](../traversable-foldable-guide.md)

**Language-Specific Guides**:
- [Python FP Guide](python-fp-style-guide.md#data-structure-patterns)
- [TypeScript FP Guide](typescript-fp-style-guide.md#data-structure-patterns)
- [Kotlin FP Guide](kotlin-fp-style-guide.md#data-structure-patterns)
- [Swift FP Guide](swift-fp-style-guide.md#data-structure-patterns)

**Main Cursor Rules**: [CURSOR.md](CURSOR.md#data-structure-guidelines)

---

## Summary

**Foldable** = Reduce/aggregate collections
- Use for: sum, product, concat, building collections
- Available in all 4 languages natively

**Traversable** = Transform collections with effects
- Use for: validation, async ops, "all-or-nothing"
- Requires libraries or custom implementations

**Parallel Traverse** = Async operations on collections
- Swift has the BEST support (TaskGroup)
- Critical for performance with I/O-bound operations

**When in doubt**: Start with native reduce/fold, add traverse when you need validation or async operations.

---

*This guide provides practical patterns for data structure operations. For deep theoretical understanding, see the [full guide](../traversable-foldable-guide.md).*

