---
title: TypeScript Functional Programming Libraries Guide
language: typescript
category: code_guidelines
type: libraries
applies_to: [cursor, kimi, claude, gemini]
version: 1.0.0
last_updated: 2025-11-19
---

# TypeScript Functional Programming Libraries Guide

This guide covers the essential libraries for functional programming in TypeScript, their use cases, and best practices.

---

## Core FP Libraries

### 1. `fp-ts` - Classic Haskell-Style FP

**Purpose**: Provides monads, functors, and type classes following Haskell conventions. Mature and comprehensive.

**Installation**:
```bash
npm install fp-ts
# or
yarn add fp-ts
```

**Key Modules**:
- `Either<E, A>` - Error handling (Left/Right)
- `Option<A>` - Nullable values (Some/None)
- `TaskEither<E, A>` - Async operations with errors
- `IO<A>` - Synchronous side effects
- `Reader<R, A>` - Dependency injection
- `Array<A>` - Functional array operations

**When to Use**:
- Existing codebases using fp-ts
- You prefer Haskell-style FP
- Need maximum compatibility and community resources
- Learning classic FP concepts

**Example**:
```typescript
import * as E from 'fp-ts/Either'
import * as TE from 'fp-ts/TaskEither'
import { pipe } from 'fp-ts/function'

type ApiError = { code: number; message: string }

const fetchUser = (id: string): TE.TaskEither<ApiError, User> =>
  pipe(
    TE.tryCatch(
      () => fetch(`/api/users/${id}`).then(r => r.json()),
      (reason): ApiError => ({ code: 500, message: String(reason) })
    ),
    TE.chain(user => 
      user ? TE.right(user) : TE.left({ code: 404, message: 'Not found' })
    )
  )

// Usage
fetchUser('123')().then(result => {
  if (E.isRight(result)) {
    console.log('Success:', result.right)
  } else {
    console.log('Error:', result.left)
  }
})
```

**Best Practices**:
- Use `pipe` for left-to-right composition
- Use `flow` for right-to-left composition
- Leverage `tryCatch` for exception handling
- Define error types as discriminated unions
- Use `TaskEither` for all async operations

---

### 2. `Effect` - Modern Effect System

**Purpose**: Next-generation effect system with better developer experience, performance, and integration.

**Installation**:
```bash
npm install effect
# or
yarn add effect
```

**Key Features**:
- First-class effect system
- Automatic dependency tracking
- Superior error messages
- Better performance
- Integrated ecosystem (Schema, Stream, etc.)

**When to Use**:
- New projects
- You want the best developer experience
- Need integrated ecosystem (Schema, Stream, etc.)
- Prefer modern FP patterns
- Want automatic dependency injection

**Example**:
```typescript
import { Effect } from 'effect'

class DatabaseError {
  readonly _tag = 'DatabaseError'
  constructor(readonly message: string) {}
}

class NotFoundError {
  readonly _tag = 'NotFoundError'
  constructor(readonly id: string) {}
}

type UserError = DatabaseError | NotFoundError

interface User {
  id: string
  name: string
}

// Define services
class Database {
  find(id: string): Effect.Effect<User | null, DatabaseError> {
    return Effect.tryPromise({
      try: () => this.queryUser(id),
      catch: (e) => new DatabaseError(String(e))
    })
  }
  
  private async queryUser(id: string): Promise<User | null> {
    // Implementation
    return { id, name: 'Alice' }
  }
}

// Service usage with automatic dependency tracking
const getUser = (id: string) =>
  Effect.gen(function* (_) {
    const db = yield* _(Effect.service<Database>())
    const user = yield* _(db.find(id))
    
    if (!user) {
      yield* _(Effect.fail(new NotFoundError(id)))
    }
    
    return user
  })

// Provide services at runtime
const program = getUser('123')

Effect.runPromise(program.pipe(
  Effect.provideService(Database, new Database())
)).then(
  user => console.log('Success:', user),
  error => console.log('Error:', error)
)
```

**Best Practices**:
- Use `Effect.gen` for generator-style composition
- Define errors as classes with `_tag` property
- Use `Effect.service` for dependency injection
- Leverage `Effect.provide*` for service configuration
- Use `Schema` for validation (integrated with Effect)

---

### 3. `io-ts` - Runtime Type Validation

**Purpose**: Runtime type checking and validation with static TypeScript type inference.

**Installation**:
```bash
npm install io-ts
# or
yarn add io-ts
```

**When to Use**:
- API request/response validation
- Parsing unknown data
- Form validation
- Configuration validation

**Example**:
```typescript
import * as t from 'io-ts'
import { pipe } from 'fp-ts/function'
import * as E from 'fp-ts/Either'

// Define runtime type
const UserCodec = t.type({
  id: t.string,
  name: t.string,
  email: t.string,
  age: t.number
})

type User = t.TypeOf<typeof UserCodec>
// TypeScript infers: { id: string; name: string; email: string; age: number }

// Validate at runtime
const validateUser = (input: unknown): E.Either<t.Errors, User> =>
  pipe(
    UserCodec.decode(input),
    E.mapLeft(errors => errors)
  )

// Usage
const result = validateUser({
  id: '123',
  name: 'Alice',
  email: 'alice@example.com',
  age: 30
})

if (E.isRight(result)) {
  console.log('Valid user:', result.right)
} else {
  console.log('Validation errors:', result.left)
}
```

**Best Practices**:
- Use codecs for all external data
- Compose codecs for complex types
- Use `t.partial` for optional fields
- Leverage branded types for refinement

---

## Utility Libraries

### 4. `remeda` - Functional Utilities

**Purpose**: Lodash-style utilities but immutable and TypeScript-first.

**Installation**:
```bash
npm install remeda
# or
yarn add remeda
```

**When to Use**:
- Data transformation pipelines
- Immutable array/object operations
- FP-friendly utility functions

**Example**:
```typescript
import { pipe, map, filter, sortBy } from 'remeda'

const users = [
  { id: 1, name: 'Alice', age: 30 },
  { id: 2, name: 'Bob', age: 25 },
  { id: 3, name: 'Charlie', age: 35 }
]

// Immutable pipeline
const result = pipe(
  users,
  filter(user => user.age > 25),
  map(user => ({ ...user, name: user.name.toUpperCase() })),
  sortBy(user => user.age)
)

// result is new array, original unchanged
```

**Best Practices**:
- Use `pipe` for left-to-right composition
- All operations return new collections
- Leverage TypeScript inference
- Use `purry` for auto-currying

---

### 5. `date-fns` - Functional Date Utilities

**Purpose**: Immutable date operations with functional API.

**Installation**:
```bash
npm install date-fns
# or
yarn add date-fns
```

**When to Use**:
- All date manipulations
- Date formatting
- Date arithmetic

**Example**:
```typescript
import { addDays, format, isAfter, parseISO } from 'date-fns'

// Pure functions - original date unchanged
const today = new Date()
const tomorrow = addDays(today, 1)
const nextWeek = addDays(today, 7)

// Formatting
const formatted = format(today, 'yyyy-MM-dd')

// Comparison
const isFuture = isAfter(tomorrow, today) // true
```

**Best Practices**:
- Never mutate Date objects directly
- Use date-fns for all date operations
- Leverage tree-shaking (import specific functions)
- Use `parseISO` for parsing dates

---

## Testing Libraries

### 6. `fast-check` - Property-Based Testing

**Purpose**: Test properties that should hold for all inputs.

**Installation**:
```bash
npm install fast-check
# or
yarn add fast-check
```

**When to Use**:
- Testing pure functions
- Verifying algebraic laws
- Finding edge cases

**Example**:
```typescript
import * as fc from 'fast-check'
import * as E from 'fp-ts/Either'

// Property: division is inverse of multiplication (for non-zero divisor)
fc.assert(
  fc.property(
    fc.integer(),
    fc.integer().filter(n => n !== 0),
    (a, b) => {
      const result = divide(a, b)
      if (E.isRight(result)) {
        return Math.abs(result.right * b - a) < 0.0001
      }
      return true // Skip if division failed
    }
  )
)
```

**Best Practices**:
- Test properties, not just examples
- Use arbitraries for custom types
- Shrink failures to minimal case
- Combine with unit tests

---

### 7. `vitest` - Modern Testing Framework

**Purpose**: Fast testing framework with native TypeScript support.

**Installation**:
```bash
npm install vitest
# or
yarn add vitest
```

**FP Testing Patterns**:
```typescript
import { describe, it, expect } from 'vitest'
import * as E from 'fp-ts/Either'
import * as O from 'fp-ts/Option'

describe('pure functions', () => {
  it('should be deterministic', () => {
    const add = (a: number, b: number): number => a + b
    
    // Same input → same output
    expect(add(2, 3)).toBe(5)
    expect(add(2, 3)).toBe(add(2, 3)) // Idempotent
  })
  
  it('should handle errors with Either', () => {
    const result = divide(10, 2)
    expect(result).toEqual(E.right(5))
    
    const errorResult = divide(10, 0)
    expect(E.isLeft(errorResult)).toBe(true)
  })
  
  it('should handle optional values', () => {
    const user = getUser('123')
    
    O.match(user, {
      onNone: () => expect.fail('User should exist'),
      onSome: (u) => expect(u.id).toBe('123')
    })
  })
})
```

**Best Practices**:
- Test pure functions for determinism
- Verify Either/Option handling
- Test error cases explicitly
- Use property-based testing for pure functions

---

## Library Integration Patterns

### Combining fp-ts and io-ts

```typescript
import * as t from 'io-ts'
import * as E from 'fp-ts/Either'
import { pipe } from 'fp-ts/function'

// Validation with Either
const validateAndProcess = (input: unknown) =>
  pipe(
    input,
    MyCodec.decode,
    E.mapLeft(errors => new ValidationError(errors)),
    E.chain(processData)
  )
```

### Using Effect with Schema

```typescript
import { Effect, Schema } from 'effect'

const UserSchema = Schema.struct({
  id: Schema.string,
  name: Schema.string,
  email: Schema.string
})

const validateUser = Schema.decode(UserSchema)

const processUser = (input: unknown) =>
  Effect.gen(function* (_) {
    const user = yield* _(validateUser(input))
    // user is typed as { id: string; name: string; email: string }
    return user
  })
```

### Remeda with fp-ts

```typescript
import { pipe } from 'fp-ts/function'
import * as A from 'fp-ts/Array'
import * as R from 'remeda'

// Combine both libraries
const processUsers = (users: User[]) =>
  pipe(
    users,
    R.filter(user => user.age > 18),
    R.map(user => ({ ...user, name: user.name.toUpperCase() })),
    A.sortBy([user => user.age])
  )
```

---

## Library Selection Guide

| Use Case | Primary | Alternative | Notes |
+|----------|---------|-------------|-------|
+| Error handling | `Effect` | `fp-ts` | Effect has better DX |
+| Async operations | `Effect` | `fp-ts/TaskEither` | Effect handles everything |
+| Validation | `io-ts` | `Effect.Schema` | Effect Schema is integrated |
+| Data transformation | `remeda` | `fp-ts/Array` | Remeda more ergonomic |
+| Date operations | `date-fns` | - | Immutable by design |
+| Testing | `vitest` | `jest` | Vitest faster, native TS |
+| Property testing | `fast-check` | - | Essential for FP |
+| Dependencies | `Effect` | `fp-ts/Reader` | Effect automatic |
+| Streaming | `Effect/Stream` | - | Effect built-in |

---

## Version Compatibility

All libraries support TypeScript 4.5+:

```json
{
  "dependencies": {
+    "effect": "^3.0.0",
+    "fp-ts": "^2.16.0",
+    "io-ts": "^2.2.0",
+    "remeda": "^1.29.0",
+    "date-fns": "^3.0.0"
+  },
+  "devDependencies": {
+    "vitest": "^1.0.0",
+    "fast-check": "^3.14.0",
+    "typescript": "^5.3.0"
+  }
+}
```

---

## Further Reading

- **Core FP Concepts**: `code_guidelines/principles/functional_programming.md`
+- **TypeScript Patterns**: `code_guidelines/languages/typescript/patterns.md`
+- **Code Examples**: `code_guidelines/languages/typescript/examples.md`

---

**Last Updated**: 2025-11-19  
+**Maintained By**: Global AI Rules System  
+**Status**: Active  
+**Applies To**: All AI assistants (Cursor, Kimi, Claude, Gemini)