---
title: TypeScript Functional Programming Patterns Reference
language: typescript
category: code_guidelines
type: patterns
applies_to: [cursor, kimi, claude, gemini]
version: 2.0.0
last_updated: 2025-11-19
---

# TypeScript Functional Programming Patterns Reference

Quick reference for common functional programming patterns in TypeScript. For detailed explanations, see `fp_style_guide.md`.

---

## Error Handling Patterns

### Either Type (Result)

Wrap success/failure in a type-safe discriminated union.

```typescript
import * as E from 'fp-ts/Either'
import { pipe } from 'fp-ts/function'

type Error = { readonly message: string }

function divide(a: number, b: number): E.Either<Error, number> {
  if (b === 0) {
    return E.left({ message: 'Cannot divide by zero' })
  }
  return E.right(a / b)
}

// Usage
const result1 = divide(10, 2)  // E.right(5)
const result2 = divide(10, 0)  // E.left({ message: '...' })

// Transform
pipe(
  result1,
  E.map(x => x * 2)      // E.right(10)
)

// Chain
pipe(
  result1,
  E.flatMap(x => divide(x, 2))  // E.right(2.5)
)
```

**When to use**: Synchronous operations that can fail. Replaces exceptions.

---

### TaskEither (Async Result)

Handle asynchronous operations that can fail.

```typescript
import * as TE from 'fp-ts/TaskEither'
import { pipe } from 'fp-ts/function'

type ApiError = { readonly _tag: 'ApiError'; readonly status: number }

const fetchUser = (id: string): TE.TaskEither<ApiError, User> =>
  TE.tryCatch(
    () => fetch(`/api/users/${id}`).then(r => r.json()),
    (error): ApiError => ({ _tag: 'ApiError', status: 500 })
  )

// Usage
pipe(
  fetchUser('123'),
  TE.map(user => user.email),
  TE.fold(
    error => console.error('Failed:', error),
    email => console.log('Email:', email)
  )
)()
```

**When to use**: Asynchronous operations (API calls, file I/O, database queries).

---

### Option/Maybe

Handle optional values without null checks.

```typescript
import * as O from 'fp-ts/Option'
import { pipe } from 'fp-ts/function'

const findUser = (id: string): O.Option<User> => {
  const user = database.get(id)
  return user ? O.some(user) : O.none
}

// Usage
pipe(
  findUser('123'),
  O.map(user => user.email),
  O.getOrElse(() => 'guest@example.com')
)

// Pattern matching
O.match(findUser('123'), {
  onNone: () => console.log('User not found'),
  onSome: (user) => console.log('Found:', user.name)
})
```

**When to use**: Nullable values, optional fields, safe property access.

---

## Composition Patterns

### Pipe (Left-to-Right)

Chain operations sequentially for readability.

```typescript
import { pipe } from 'fp-ts/function'
import * as O from 'fp-ts/Option'

const result = pipe(
  '  TEST@example.com  ',
  (s: string) => s.toLowerCase(),
  (s: string) => s.trim(),
  O.fromPredicate(email => email.includes('@')),
  O.map(email => ({ email, verified: false }))
)
```

**When to use**: Sequential transformations where each step depends on the previous.

---

### Flow (Function Composition)

Create reusable function pipelines.

```typescript
import { flow } from 'fp-ts/function'
import * as O from 'fp-ts/Option'

// Create reusable transformation
const normalizeEmail = flow(
  (s: string) => s.toLowerCase(),
  (s: string) => s.trim(),
  O.fromPredicate(email => email.includes('@'))
)

// Usage
const email1 = normalizeEmail('  TEST@example.com  ') // O.some('test@example.com')
const email2 = normalizeEmail('invalid') // O.none
```

**When to use**: Creating reusable transformation pipelines, higher-order functions.

---

### Railway-Oriented Programming

Chain operations that can fail with automatic error handling.

```typescript
import * as TE from 'fp-ts/TaskEither'
import { pipe } from 'fp-ts/function'

type ValidationError = { readonly _tag: 'ValidationError' }
type DatabaseError = { readonly _tag: 'DatabaseError' }

const validateInput = (input: unknown): TE.TaskEither<ValidationError, Input> =>
  // Implementation

const saveToDatabase = (data: Input): TE.TaskEither<DatabaseError, Result> =>
  // Implementation

const sendEmail = (result: Result): TE.TaskEither<EmailError, void> =>
  // Implementation

// Railway: errors short-circuit automatically
const pipeline = (input: unknown) =>
  pipe(
    input,
    validateInput,
    TE.flatMap(saveToDatabase),
    TE.flatMap(sendEmail)
  )

// Visualizing the railway:
// Input → validate → save → email → Success
//   ↓        ↓         ↓      ↓
// Error ←────┴─────────┴──────┴───────┘
//   ↑
// Short-circuit on first error
```

**When to use**: Multi-step workflows where any step can fail (validation → processing → storage → notifications).

---

### Reader Pattern (Dependency Injection)

Inject dependencies in a pure, type-safe way.

```typescript
import * as R from 'fp-ts/Reader'
import { pipe } from 'fp-ts/function'

// Dependencies interface
interface Dependencies {
  readonly database: Database
  readonly logger: Logger
  readonly config: Config
}

// Function that requires dependencies
const getUser = (id: string): R.Reader<Dependencies, O.Option<User>> =>
  pipe(
    R.ask<Dependencies>(),
    R.map(({ database }) => database.find(id))
  )

// Compose dependency-requiring functions
const createUser = (user: User): R.Reader<Dependencies, User> =>
  pipe(
    R.ask<Dependencies>(),
    R.map(({ database, logger }) => {
      logger.info(`Creating user ${user.id}`)
      return database.insert(user)
    })
  )

// Provide dependencies at the edge
const dependencies: Dependencies = {
  database: new PostgresDatabase(),
  logger: new WinstonLogger(),
  config: loadConfig()
}

// Run with dependencies
const user = createUser(newUser)(dependencies)
```

**When to use**: Dependency injection, configuration management, testing with mocks.

---

## Immutability Patterns

### Readonly Types

Make all properties readonly by default.

```typescript
interface User {
  readonly id: string
  readonly name: string
  readonly email: string
}

// Readonly arrays
const users: ReadonlyArray<User> = [user1, user2]
// users.push(newUser) // Compile error!
```

**When to use**: All data structures, domain models, API responses.

---

### as const Assertions

Create immutable constants with literal types.

```typescript
// Mutable array with string type
const API_ENDPOINTS = ['https://api1.com', 'https://api2.com']
API_ENDPOINTS.push('https://api3.com') // Allowed!

// Immutable tuple with literal types
const API_ENDPOINTS = ['https://api1.com', 'https://api2.com'] as const
// API_ENDPOINTS.push(...) // Compile error!
// Type is readonly ['https://api1.com', 'https://api2.com']
```

**When to use**: Constants, configuration values, discriminated union tags.

---

### Immutable Updates

Update objects immutably using spread syntax.

```typescript
interface State {
  readonly user: User
  readonly settings: Settings
  readonly count: number
}

// Immutable update returns new state
const increment = (state: State): State => ({
  ...state,
  count: state.count + 1
})

// Nested updates
const updateUserEmail = (state: State, newEmail: string): State => ({
  ...state,
  user: {
    ...state.user,
    email: newEmail
  }
})
```

**When to use**: State management, Redux reducers, functional updates.

---

## Effect Patterns (Modern FP)

### Effect as Alternative to TaskEither

Effect combines errors, dependencies, and async in one type.

```typescript
import { Effect } from 'effect'

type FetchError = { readonly _tag: 'FetchError' }
type ParseError = { readonly _tag: 'ParseError' }

// Effect<SuccessType, ErrorType, Dependencies>
const fetchUser = (id: string): Effect.Effect<User, FetchError | ParseError> =>
  Effect.gen(function* (_) {
    const response = yield* _(
      Effect.tryPromise({
        try: () => fetch(`/api/users/${id}`),
        catch: (): FetchError => ({ _tag: 'FetchError' })
      })
    )
    
    if (!response.ok) {
      yield* _(Effect.fail({ _tag: 'FetchError' } as const))
    }
    
    return yield* _(
      Effect.tryPromise({
        try: () => response.json(),
        catch: (): ParseError => ({ _tag: 'ParseError' })
      })
    )
  })

// Usage
Effect.runPromise(fetchUser('123')).then(
  user => console.log('Success:', user),
  error => console.log('Error:', error)
)
```

**When to use**: New projects, complex dependency management, prefer simpler DX.

---

### Effect Services

Define dependencies as services with automatic tracking.

```typescript
import { Effect, Context } from 'effect'

// Define service interface
class Database extends Context.Tag('Database')<
  Database,
  { readonly find: (id: string) => Effect.Effect<User | null, DatabaseError> }
>() {}

class Logger extends Context.Tag('Logger')<
  Logger,
  { readonly info: (message: string) => Effect.Effect<void, never> }
>() {}

// Use services automatically
const getUser = (id: string): Effect.Effect<User, DatabaseError, Database> =>
  Effect.gen(function* (_) {
    const user = yield* _(Database.find(id))
    if (user === null) {
      yield* _(Effect.fail(new DatabaseError('User not found')))
    }
    return user
  })

// Compose with automatic dependency tracking
const createUser = (user: User): Effect.Effect<User, DatabaseError, Database | Logger> =>
  Effect.gen(function* (_) {
    yield* _(Logger.info(`Creating user ${user.id}`))
    return yield* _(Database.insert(user))
  })

// Provide services at runtime
const program = createUser(newUser)

Effect.runPromise(program.pipe(
  Effect.provideService(Database, new PostgresDatabase()),
  Effect.provideService(Logger, new ConsoleLogger())
))
```

**When to use**: Complex applications with many services, prefer compile-time dependency tracking.

---

## Testing Patterns

### Testing Pure Functions

Pure functions are trivial to test: same input → same output.

```typescript
// Pure function
const add = (a: number, b: number): number => a + b

// Test: Idempotent and deterministic
test('add is pure', () => {
  expect(add(2, 3)).toBe(5)
  expect(add(2, 3)).toBe(add(2, 3)) // Same result every time
})

// Test Result types
import * as E from 'fp-ts/Either'

const divide = (a: number, b: number): E.Either<string, number> =>
  b === 0 ? E.left('Division by zero') : E.right(a / b)

test('divide handles errors', () => {
  expect(divide(10, 2)).toEqual(E.right(5))
  expect(divide(10, 0)).toEqual(E.left('Division by zero'))
})
```

**When to use**: All business logic, utilities, domain functions.

---

### Testing Effect

Mock dependencies and test effects.

```typescript
import { Effect } from 'effect'

test('getUser success', async () => {
  const mockDatabase = {
    find: jest.fn(() => Effect.succeed({ id: '123', name: 'Alice' }))
  }
  
  const result = await Effect.runPromise(
    getUser('123').pipe(
      Effect.provideService(Database, mockDatabase)
    )
  )
  
  expect(result).toEqual({ id: '123', name: 'Alice' })
  expect(mockDatabase.find).toHaveBeenCalledWith('123')
})
```

**When to use**: Testing functions with dependencies, integration tests.

---

## Anti-Patterns

### ❌ Mutable State

```typescript
// BAD: Mutable state
let globalUser: User | null = null

function setUser(user: User): void {
  globalUser = user // Side effect!
}

// GOOD: Immutable state
interface State {
  readonly user: User | null
}

const setUser = (state: State, user: User): State => ({
  ...state,
  user
})
```

---

### ❌ Mixed IO and Logic

```typescript
// BAD: Database access mixed with computation
async function calculateAndSave(value: number): Promise<number> {
  const result = value * 2 // Logic
  await db.save(result)    // IO side effect!
  return result
}

// GOOD: Separate IO and logic
const calculate = (value: number): number => value * 2 // Pure

const saveResult = (result: number): TE.TaskEither<DbError, void> =>
  TE.tryCatch(
    () => db.save(result),
    (error): DbError => ({ message: String(error) })
  )

// Compose at the edge
const calculateAndSave = (value: number): TE.TaskEither<DbError, number> =>
  pipe(
    TE.right(calculate(value)),
    TE.flatMap(result =>
      pipe(
        saveResult(result),
        TE.map(() => result)
      )
    )
  )
```

---

### ❌ Using any Type

```typescript
// BAD: Type safety lost
function process(data: any): any {
  return data.map(x => x.value) // No type checking!
}

// GOOD: Proper generic types
function process<T extends { value: V }, V>(data: ReadonlyArray<T>): V[] {
  return data.map(x => x.value)
}
```

---

## Pattern Selection Guide

| Problem | Pattern | Example |
|---------|---------|---------|
+| Async operation can fail | TaskEither | `fetchUser(): TE.TaskEither<Error, User>` |
+| Synchronous operation can fail | Either | `parseInt(): E.Either<Error, number>` |
+| Value might be absent | Option | `findUser(): O.Option<User>` |
+| Nullable value | Option.fromNullable | `O.fromNullable(maybeValue)` |
+| Multiple async steps | Pipe + TE.flatMap | `pipe(validate, TE.flatMap(save), TE.flatMap(send))` |
+| Reusable transformations | Flow | `const transform = flow(validate, normalize, create)` |
+| Dependency injection | Reader | `R.Reader<Deps, Result>` |
+| Complex dependencies | Effect | `Effect.Effect<T, E, Services>` |
+| Configuration | Reader or Effect | `R.Reader<Config, T>` |
+| Immutable updates | Spread syntax | `{ ...state, count: state.count + 1 }` |
+| Safe property access | Option | `O.fromNullable(obj?.property)` |

---

**Last Updated**: 2025-11-19  
**Maintained By**: Global AI Rules System  
**Status**: Active  
**Applies To**: All AI assistants (Cursor, Kimi, Claude, Gemini)