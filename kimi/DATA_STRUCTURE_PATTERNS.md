# Data Structure Patterns: Quick Reference Guide for Kimi

**Purpose**: Quick lookup for Foldable/Traversable patterns across Haskell, Python, TypeScript, Kotlin, Swift, and Rust with Kimi-specific validation patterns.

**Full Guide**: See [cursor/guides/traversable-foldable-guide.md](cursor/guides/traversable-foldable-guide.md) for comprehensive details (4,800+ lines covering all 6 languages, with Haskell as the reference implementation).

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

## Kimi-Specific Validation Patterns

### Parallel Validation of Foldable/Traversable Operations

Kimi can validate multiple collection operations simultaneously:

```haskell
-- Kimi excels at validating Haskell's native Foldable/Traversable
data ValidationResult = Valid | Invalid String

validateUsers :: [User] -> [ValidationResult]
validateUsers users = 
  -- Kimi can validate in parallel:
  -- ✓ foldMap for aggregation
  -- ✓ traverse for effectful validation
  -- ✓ Parallel composition correctness
  traverse validateUser users
  
  where
    validateUser :: User -> ValidationResult
    validateUser user = 
      validateEmail (userEmail user)  -- ✓ Validated
        <> validateName (userName user)  -- ✓ Validated (parallel)
```

### Subagent Pattern for Complex Transformations

Use Kimi's Task tool for complex data structure validations:

```typescript
// Complex pipeline that benefits from Kimi's subagent validation
const processUserData = (users: User[]): Either<Error, ProcessedUser[]> =>
  pipe(
    users,
    // Kimi can spawn subagent for: ✓ Type safety
    A.traverse(TE.taskEither)(validateUser),
    // Kimi can spawn subagent for: ✓ Transformation correctness
    TE.chain(A.traverse(TE.taskEither)(enrichUserData)),
    // Kimi can spawn subagent for: ✓ Final aggregation
    TE.map(A.filter(isActiveUser))
  )
```

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
    │     - Python: reduce, foldr (returns library)
    │     - TypeScript: Array.reduce, fp-ts fold
    │     - Kotlin: fold/reduce, Arrow fold
    │     - Swift: reduce, reduce(into:)
    │     - Rust: fold, sum (ZERO-COST!)
    │
Need to transform with effects? (validation, IO, async)
    ↓
    YES → Use TRAVERSABLE
    │     - Haskell: traverse, sequenceA (REFERENCE IMPL!)
    │     - Python: custom traverse with Result
    │     - TypeScript: fp-ts traverse, Effect traverse
    │     - Kotlin: Arrow traverse with Either/IO
    │     - Swift: custom traverse with Result
    │     - Rust: Iterator::collect with Result/Option (NATIVE!)
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
          - Swift: TaskGroup/async let (BEST ERGONOMICS!)
          - Kotlin: Arrow parTraverse/use async/await + Either
          - TypeScript: Effect Promise.all/parallel
          - Python: asyncio.gather + custom traverse
```

---

## Quick Syntax Reference

### Foldable (Reduce/Fold)

| Language | Syntax | Example | Kimi Validation |
|----------|--------|---------|-----------------|
| **Haskell** | `foldr f z xs`, `sum`, `product` | `sum [1,2,3]` → `6` | ✓ Type safety, ✓ Polymorphic |
| **Python** | `reduce(f, xs, init)`, `foldr` | `reduce(lambda a,x: a+x, [1,2,3], 0)` | ✓ Side-effect analysis |
| **TypeScript** | `xs.reduce(f, init)`, `A.fold` | `[1,2,3].reduce((a,x) => a+x, 0)` | ✓ Generic constraints |
| **Kotlin** | `xs.fold(init, f)`, `fold()` | `list.fold(0) { a, x -> a + x }` | ✓ Immutability check |
| **Swift** | `xs.reduce(into:init, f)` | `[1,2,3].reduce(0, +)` | ✓ Value semantics |
| **Rust** | `xs.iter().fold(init, f)`, `sum()` | `[1,2,3].iter().sum()` | ✓ Zero-cost guarantees |

### Traversable (Effectful Transform)

| Language | Syntax | Example | Kimi Validation |
|----------|--------|---------|-----------------|
| **Haskell** | `traverse f xs`, `sequenceA` | `traverse validate [x,y,z]` | ✓ Law compliance |
| **Python** | `[f(x) for x in xs]` (custom) | `returns.pipeline.traverse(f, xs)` | ✓ Result type safety |
| **TypeScript** | `A.traverse(O.option)(f, xs)` | `A.traverse(TE.taskEither)` | ✓ Effect typing |
| **Kotlin** | `xs.traverseEither(f)` | `list.traverseEither { validate(it) }` | ✓ Coroutine safety |
| **Swift** | Custom with `map` + `sequence` | `[user1, user2].traverse(fetch)` | ✓ Async correctness |
| **Rust** | `xs.iter().map(f).collect()` | `users.iter().map(validate).collect()` | ✓ Iterator soundness |

### Parallel Traversable

| Language | Syntax | Kimi Advantage |
|----------|--------|----------------|
| **Haskell** | `parTraverse` (Parallel Strategies) | Sequential fallback check |
| **Python** | `asyncio.gather(*[f(x) for x in xs])` | Concurrency safety |
| **TypeScript** | `T.traverse(TE.taskEither)(xs, f)` | Promise.all optimization |
| **Kotlin** | `parTraverse { async { ... } }` | Structured concurrency |
| **Swift** | `TaskGroup` + `addTask` | Actor isolation check |
| **Rust** | `rayon::iter::ParallelIterator` | Data race detection |

---

## Use Cases with Kimi Validation

### Use Case 1: Form Validation (Early Exit)

```typescript
// Kimi can parallel validate each field validation
const validateForm = (form: FormData): Either<ValidationError, ValidForm> =>
  pipe(
    E.Do,
    E.bind('name', () => validateName(form.name)),  // ✓ Validated
    E.bind('email', () => validateEmail(form.email)),  // ✓ Validated (parallel)
    E.bind('phone', () => validatePhone(form.phone)),  // ✓ Validated
    E.map(({ name, email, phone }) => ({ name, email, phone }))
  )

// Kimi spawns subagents to verify:
// - Each validation function is pure
// - Error types align
// - Railway pattern correct
```

### Use Case 2: API Response Aggregation

```kotlin
// Kimi validates parallel coroutine safety
suspend fun fetchUserWithPosts(userId: UserId): Either<Error, UserWithPosts> =
    either {
        coroutineScope {
            // Kimi validates: ✓ Parallel safety
            val user = async { getUser(userId).bind() }  // Subagent 1
            val posts = async { getPosts(userId).bind() }  // Subagent 2 (parallel)
            
            UserWithPosts(user.await(), posts.await())
        }
    }
```

### Use Case 3: Processing Pipeline (Streaming)

```rust
// Kimi validates iterator composition
fn process_file(path: &Path) -> Result<Vec<ProcessedRecord>, Error> {
    read_lines(path)?
        .par_iter()  // Kimi: ✓ Parallel safety
        .map(parse_line)  // Kimi: ✓ Type transformation
        .map(validate_record)  // Kimi: ✓ Validation (parallel)
        .map(enrich_record)  // Kimi: ✓ Enrichment
        .collect()  // Kimi: ✓ Final aggregation
}
```

---

## Language-Specific Examples

### Haskell (Reference Implementation)

```haskell
-- Kimi validates: ✓ Typeclass laws
validateUsers :: [User] -> Maybe ValidationError
validateUsers = foldMap validateUser
  where
    validateUser :: User -> Maybe ValidationError
    validateUser u = validateEmail (userEmail u) <> validateName (userName u)

-- Traversable for effectful validation
processUsers :: [User] -> IO [Either Error ProcessedUser]
processUsers users = traverse processUser users
  where
    processUser :: User -> IO (Either Error ProcessedUser)
    processUser user = runExceptT $ do
        validateUserIO user
        processData user
        
-- Parallel with strategies
processUsersParallel :: [User] -> IO [Either Error ProcessedUser]
processUsersParallel users = 
    parTraverse processUser users  -- Kimi: ✓ Parallel semantics
```

### TypeScript with fp-ts

```typescript
// Kimi validates: ✓ Effect typing
const validateAndProcessUsers = (
    users: User[]
): TE.TaskEither<ProcessingError, ProcessedUser[]> =>
    pipe(
        users,
        // Kimi subagent: ✓ Type safety
        A.traverse(TE.taskEither)(validateAndProcessUser)
        // Kimi subagent: ✓ Error handling
    )

// Parallel with Effect
import * as Effect from 'effect'

const processInParallel = (
    users: User[]
): Effect.Effect<ProcessedUser[], ProcessingError> =>
    Effect.forEach(users, processUser, { concurrency: 5 })
    // Kimi: ✓ Concurrency limits validated
```

### Python with returns

```python
# Kimi validates: ✓ Result type safety
from returns.pipeline import is_successful
from returns.result import Result, Success, Failure

def validate_all_users(users: List[User]) -> Result[List[User], ValidationError]:
    results = [validate_user(user) for user in users]
    
    # Kimi: ✓ Early exit on failure
    if not all(is_successful(r) for r in results):
        return Failure(ValidationError("Some users invalid"))
    
    return Success([r.unwrap() for r in results])

# Kimi can parallel-validate each user validation
```

### Rust with Iterator

```rust
// Kimi validates: ✓ Iterator soundness
fn validate_products(products: &[Product]) -> Result<Vec<ValidatedProduct>, Error> {
    products
        .par_iter()  // Kimi: ✓ Parallel iteration safety
        .map(validate_product)  // Kimi: ✓ Map correctness
        .collect()  // Kimi: ✓ Type aggregation
}

// Sequential fallback
fn validate_products_seq(products: &[Product]) -> Result<Vec<ValidatedProduct>, Error> {
    products
        .iter()  // Kimi: ✓ Sequential correctness
        .map(validate_product)
        .collect()
}
```

---

## Kimi CLI Integration

### Parallel Validation Scripts

```bash
#!/bin/bash
# Run all validations in parallel

# Kimi can execute multiple validation commands simultaneously
echo "Running parallel validations..."

# File validation (parallel)
kimi read-file src/module.ts &
kimi read-file src/pure/functions.ts &
wait

# Type checking (parallel)
python -m mypy src/ &
npx tsc --noEmit &
cargo check &
wait

# Test execution (parallel)
pytest tests/unit/ &
npm test &
cargo test &
wait

echo "All validations complete!"
```

### Subagent Pattern for Data Validation

```bash
# Complex data validation pipeline
# Use Kimi's Task tool to spawn subagents

# Subagent 1: Validate type signatures
echo "Spawning type validation subagent..."
kimi task "Type Safety Check" "coder" "
Verify all functions in src/ follow:
- Proper Either types for error handling
- Immutable data structures
- No mutation of inputs
" &

# Subagent 2: Validate collection operations
echo "Spawning collection validation subagent..."
kimi task "Collection Pattern Check" "coder" "
Verify all collection operations use:
- traverse for effectful transformations
- foldMap for aggregations
- Proper parallelization where needed
" &

wait
echo "All subagent validations complete!"
```

---

## Summary

This guide provides quick reference for Foldable/Traversable patterns across all supported languages with Kimi-specific validation strategies.

**Key Takeaways**:
- Use Foldable for aggregation/reduction
- Use Traversable for effectful transformations
- Kimi validates type safety and composition
- Use parallel traversal for independent operations
- Kimi can spawn subagents for complex validations

**Why This Matters**: These patterns are fundamental to FP data processing. Haskell provides the reference implementation; all other languages adapt these patterns.

**Next**: See [cursor/guides/traversable-foldable-guide.md](cursor/guides/traversable-foldable-guide.md) for comprehensive coverage with 4,800+ lines of examples across all 6 languages.

---

**Last Updated**: 2025-11-14  
**Maintained By**: Kimi CLI Global Rules System  
**Status**: Active
