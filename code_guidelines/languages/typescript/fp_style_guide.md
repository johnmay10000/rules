---
title: TypeScript Functional Programming Style Guide
language: typescript
category: code_guidelines
type: language
applies_to: [cursor, kimi, claude, gemini]
version: 2.0.0
last_updated: 2025-11-19
---

# TypeScript Functional Programming Style Guide

**Target**: TypeScript projects (Next.js, Supabase, Inngest, backend, web applications)

> **📖 Universal Rules**: This document provides TypeScript-specific functional programming guidance. For mandatory universal rules (Git, testing, documentation, project structure), see the `universal_rules/` directory.

---

## Quick Links

- **FP Principles Deep Dive**: See `code_guidelines/principles/functional_programming.md`
- **Pattern Reference**: See `code_guidelines/languages/typescript/patterns.md`
- **Code Examples**: See `code_guidelines/languages/typescript/examples.md`
- **Library Guide**: See `code_guidelines/languages/typescript/libraries.md`

---

## Core Principles

1. **Immutability**: Use `readonly`, `as const`, never mutate data
2. **Pure Functions**: Functions have no side effects unless explicitly marked
3. **Type Safety**: Leverage TypeScript's type system to the fullest
4. **Composability**: Build complex operations from small, reusable functions
5. **Explicit Error Handling**: Use `Either`/`Result` types instead of exceptions
6. **Railway-Oriented Programming**: Chain operations with monadic composition

---

## Required Libraries

Choose **one** FP library ecosystem:

### Option A: fp-ts (Haskell-Style, Mature)
```typescript
// Core FP library - like Haskell's Prelude for TypeScript
import * as E from 'fp-ts/Either'
import * as O from 'fp-ts/Option'
import * as TE from 'fp-ts/TaskEither'
import * as T from 'fp-ts/Task'
import * as A from 'fp-ts/Array'
import { pipe, flow } from 'fp-ts/function'
import * as IO from 'fp-ts/IO'
import * as IOE from 'fp-ts/IOEither'
import * as R from 'fp-ts/Reader'
```

### Option B: Effect (Modern, Better DX)
```typescript
// Alternative: Effect (more powerful, better developer experience)
import { Effect, Exit, pipe as Epipe, Either, Option, ReadonlyArray } from 'effect'
import * as Effect from 'effect/Effect'
```

**Recommendation**: Use `Effect` for new projects, `fp-ts` for existing codebases or if you prefer Haskell-style FP.

---

## 1. Error Handling with Either/Result

### ❌ Avoid: Try/Catch Exceptions

```typescript
// BAD: Exceptions are not part of the type signature
async function fetchUser(id: string): Promise<User> {
  try {
    const response = await fetch(`/api/users/${id}`)
    if (!response.ok) throw new Error('Failed to fetch')
    return await response.json()
  } catch (error) {
    throw error  // Who knows what might be thrown?
  }
}
```

### ✅ Prefer: TaskEither (fp-ts)

```typescript
import * as TE from 'fp-ts/TaskEither'
import * as E from 'fp-ts/Either'
import { pipe } from 'fp-ts/function'

// Define error types explicitly
type FetchError = 
  | { _tag: 'NetworkError'; message: string }
  | { _tag: 'ParseError'; message: string }
  | { _tag: 'NotFound' }

// Function returns TaskEither - errors are part of the type!
const fetchUser = (id: string): TE.TaskEither<FetchError, User> =>
  pipe(
    TE.tryCatch(
      () => fetch(`/api/users/${id}`),
      (error): FetchError => ({
        _tag: 'NetworkError',
        message: String(error)
      })
    ),
    TE.filterOrElse(
      (response) => response.ok,
      (response): FetchError =>
        response.status === 404
          ? { _tag: 'NotFound' }
          : { _tag: 'NetworkError', message: `Status ${response.status}` }
    ),
    TE.flatMap((response) =>
      TE.tryCatch(
        () => response.json(),
        (error): FetchError => ({
          _tag: 'ParseError',
          message: String(error)
        })
      )
    )
  )

// Usage
const userTask = fetchUser('123')

// Run the TaskEither
userTask().then(result => {
  if (E.isRight(result)) {
    console.log('Success:', result.right)  // User object
  } else {
    console.log('Error:', result.left)     // FetchError
  }
})
```

### ✅ Prefer: Effect (Modern Alternative)

```typescript
import { Effect } from 'effect'

type FetchError = 
  | { readonly _tag: 'NetworkError'; readonly message: string }
  | { readonly _tag: 'ParseError'; readonly message: string }
  | { readonly _tag: 'NotFound' }

const fetchUser = (id: string): Effect.Effect<User, FetchError> =>
  Effect.tryPromise({
    try: () => fetch(`/api/users/${id}`),
    catch: (error): FetchError => ({
      _tag: 'NetworkError',
      message: String(error)
    })
  }).pipe(
    Effect.flatMap(response =>
      response.ok
        ? Effect.succeed(response)
        : Effect.fail(
            response.status === 404
              ? { _tag: 'NotFound' } as const
              : { _tag: 'NetworkError', message: `Status ${response.status}` }
          )
    ),
    Effect.flatMap(response =>
      Effect.tryPromise({
        try: () => response.json(),
        catch: (error): FetchError => ({
          _tag: 'ParseError',
          message: String(error)
        })
      })
    )
  )

// Usage
Effect.runPromise(fetchUser('123')).then(
  user => console.log('Success:', user),
  error => console.log('Error:', error)
)
```

**Key Benefits**:
- Errors are part of the type signature (type-safe)
- Forces callers to handle errors explicitly
- No hidden exception flows
- Composable with other error-handling functions
- Exhaustive error type checking

---

## 2. Option/Maybe for Nullable Values

### ❌ Avoid: Null Checks Everywhere

```typescript
// BAD: Manual null checking, easy to forget
function getUser(id: string): User | null {
  return database.find(id) || null
}

function sendEmail(user: User | null): void {
  if (user !== null) {
    emailService.send(user.email)
  }
}

// Easy to forget the null check!
const user = getUser('123')
sendEmail(user) // Runtime error if user is null
```

### ✅ Prefer: Option (fp-ts)

```typescript
import * as O from 'fp-ts/Option'
import { pipe } from 'fp-ts/function'

// Function returns Option - must be handled explicitly
const getUser = (id: string): O.Option<User> => {
  const user = database.find(id)
  return user ? O.some(user) : O.none
}

const sendEmail = (user: User): TE.TaskEither<EmailError, void> => {
  // Implementation
}

// Explicit handling required
pipe(
  getUser('123'),
  O.fold(
    () => console.log('User not found'),  // Handle None case
    (user) => sendEmail(user)             // Handle Some case
  )
)
```

### ✅ Prefer: Option (Effect)

```typescript
import { Option } from 'effect'

const getUser = (id: string): Option.Option<User> =>
  Option.fromNullable(database.find(id))

const sendEmail = (user: User): Effect.Effect<void, EmailError> =>
  // Implementation

// Explicit handling
Option.match(getUser('123'), {
  onNone: () => console.log('User not found'),
  onSome: (user) => sendEmail(user)
})
```

**Benefits**:
- Nullable values are explicit in the type system
- Forces handling of missing values
- Rich API for transformations and defaults
- No null pointer exceptions

---

## 3. Immutability Patterns

### ✅ Use Readonly Types

```typescript
// All properties are readonly
interface User {
  readonly id: string
  readonly name: string
  readonly email: string
}

// Readonly arrays
const users: ReadonlyArray<User> = [user1, user2]
// users.push(newUser) // Compile error!

// Or using built-in utility type
type Immutable<T> = {
  readonly [P in keyof T]: T[P]
}

interface MutableUser {
  id: string
  name: string
}

type ImmutableUser = Immutable<MutableUser>
// All properties become readonly
```

### ✅ Use as const for Constants

```typescript
// BAD: Mutable array
const API_ENDPOINTS = [
  'https://api1.example.com',
  'https://api2.example.com'
]
API_ENDPOINTS.push('https://api3.example.com') // Allowed!

// GOOD: Immutable constant
const API_ENDPOINTS = [
  'https://api1.example.com',
  'https://api2.example.com'
] as const
// API_ENDPOINTS.push(...) // Compile error!
// Type is readonly ['https://api1.example.com', 'https://api2.example.com']
```

### ✅ Immutable Updates with Spread

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

---

## 4. Function Composition

### ✅ Compose Small Functions

```typescript
import { pipe } from 'fp-ts/function'

// Small, focused functions
const validateEmail = (email: string): O.Option<string> =>
  email.includes('@') ? O.some(email) : O.none

const normalizeEmail = (email: string): string =>
  email.toLowerCase().trim()

const createUser = (email: string): User => ({
  id: generateId(),
  email,
  createdAt: new Date()
})

// Compose into pipeline
const registerUser = (rawEmail: string): O.Option<User> =>
  pipe(
    rawEmail,
    validateEmail,
    O.map(normalizeEmail),
    O.map(createUser)
  )
```

### ✅ Railway-Oriented Programming

```typescript
import * as TE from 'fp-ts/TaskEither'
import { pipe } from 'fp-ts/function'

// Each step returns TaskEither - errors short-circuit
const createUserPipeline = (input: unknown) =>
  pipe(
    TE.fromEither(validateInput(input)),
    TE.flatMap(parseData),
    TE.flatMap(checkConstraints),
    TE.flatMap(saveToDatabase),
    TE.map(createWelcomeEmail),
    TE.flatMap(sendEmail)
  )

// Visualizing the railway:
// Input → validate → parse → check → save → email → send → Result
//   ↓        ↓        ↓       ↓      ↓      ↓      ↓       ↓
// Error ←────┴────────┴───────┴──────┴──────┴──────┴───────┘
//   ↑
// Short-circuit on first error
```

---

## 5. Reader Pattern for Dependency Injection

### ✅ Pure Dependency Injection

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

// Another function using dependencies
const createUser = (user: User): R.Reader<Dependencies, User> =>
  pipe(
    R.ask<Dependencies>(),
    R.map(({ database, logger }) => {
      logger.info(`Creating user ${user.id}`)
      return database.insert(user)
    })
  )

// Compose functions that need dependencies
const registerUser = (userData: UserData): R.Reader<Dependencies, O.Option<User>> =>
  pipe(
    validateUserData(userData),
    O.fold(
      () => R.of(O.none),  // Validation failed
      (validated) => pipe(
        createUser(validated),
        R.map(O.some)
      )
    )
  )

// Provide dependencies at the edge
const dependencies: Dependencies = {
  database: new PostgresDatabase(),
  logger: new WinstonLogger(),
  config: loadConfig()
}

// Run with dependencies
const user = registerUser(userData)(dependencies)
```

---

## 6. Effect Pattern (Modern Alternative)

### ✅ Using Effect for Everything

```typescript
import { Effect } from 'effect'

// Dependencies as a service
class Database {
  find(id: string): Effect.Effect<User | null, DatabaseError> {
    // Implementation
  }
}

class Logger {
  info(message: string): Effect.Effect<void, never> {
    // Implementation
  }
}

// Dependencies interface
interface Services {
  readonly database: Database
  readonly logger: Logger
}

// Function using dependencies
const getUser = (id: string): Effect.Effect<User, DatabaseError, Services> =>
  Effect.gen(function* (_) {
    const { database } = yield* _(Effect.context<Services>())
    const user = yield* _(database.find(id))
    if (user === null) {
      yield* _(Effect.fail(new DatabaseError('User not found')))
    }
    return user
  })

// Compose with automatic dependency tracking
const createUser = (user: User): Effect.Effect<User, DatabaseError, Services> =>
  Effect.gen(function* (_) {
    const { database, logger } = yield* _(Effect.context<Services>())
    yield* _(logger.info(`Creating user ${user.id}`))
    return yield* _(database.insert(user))
  })

// Effect handles dependencies, errors, and async automatically
const registerUser = (userData: UserData) =>
  Effect.gen(function* (_) {
    const validated = yield* _(validateUserData(userData))
    const user = yield* _(createUser(validated))
    yield* _(sendWelcomeEmail(user))
    return user
  })

// Provide dependencies at runtime
const program = registerUser(userData)

Effect.runPromise(program.pipe(
  Effect.provideService(Logger, new WinstonLogger()),
  Effect.provideService(Database, new PostgresDatabase())
)).then(
  user => console.log('Success:', user),
  error => console.log('Error:', error)
)
```

---

## 7. Testing FP Code

### ✅ Testing Pure Functions

```typescript
// Pure function - easy to test
const add = (a: number, b: number): number => a + b

// Test: same input → same output
test('add is pure', () => {
  expect(add(2, 3)).toBe(5)
  expect(add(2, 3)).toBe(add(2, 3)) // Idempotent
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

### ✅ Testing Effect

```typescript
import { Effect } from 'effect'

// Test with mocked dependencies
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
})
```

---

## Tool-Specific Notes

### For Kimi Users

Kimi provides enhanced FP validation:
- **Parallel Verification**: Verify multiple pure functions simultaneously
- **Task Tool**: Spawn subagents for complex type checking
- **SetTodoList Integration**: Track FP refactoring tasks
- **Pipeline Validation**: Validate railway-oriented compositions

Example Kimi pattern:
```typescript
// Kimi can verify in parallel:
// - Type signature correctness
// - Error handling completeness
// - Function purity
// - Composition validity
const complexPipeline = pipe(
  validateInput,
  TE.flatMap(parseData),
  TE.flatMap(checkConstraints),
  TE.flatMap(saveToDatabase)
)
```

### For Cursor Users

Cursor provides IDE-integrated FP support:
- **Inline Suggestions**: Real-time FP pattern recommendations
- **Refactoring**: Automated conversion to FP patterns
- **Type Checking**: Integrated TypeScript and library type checking
- **VS Code Integration**: Native IDE features

### For Claude Users

Claude excels at FP code generation:
- **Pattern Generation**: Create complex pipelines from descriptions
- **Error Handling**: Automatic Either/Result wrapping
- **Composition**: Suggest optimal function compositions
- **Best Practices**: Detailed FP guidance

### For Gemini Users

Gemini provides comprehensive FP education:
- **Concept Explanations**: Detailed FP theory and practice
- **Code Examples**: Extensive example libraries
- **Pattern Recognition**: Identify FP opportunities in existing code
- **Multi-Library**: Support for fp-ts, Effect, and other FP libraries

---

## Quick Reference Card

### Either/TaskEither (fp-ts)

```typescript
// Creating
E.right(value)          // Success
E.left(error)           // Failure
TE.right(value)         // Async success
TE.left(error)          // Async failure
TE.tryCatch(promise, onError)

// Transforming
E.map(f)                // Transform success
E.mapLeft(f)            // Transform error
E.flatMap(f)            // Chain E-returning functions
TE.map(f)               // Transform async success
TE.flatMap(f)           // Chain TE-returning functions

// Chaining
pipe(value, f1, E.flatMap(f2), E.flatMap(f3))
```

### Option (fp-ts)

```typescript
// Creating
O.some(value)           // Present value
O.none                  // Absent value
O.fromNullable(value)

// Transforming
O.map(f)                // Transform if present
O.flatMap(f)            // Chain O-returning functions
O.getOrElse(default)    // Provide default

// Usage
O.match(option, { onNone: () => ..., onSome: (value) => ... })
```

### Effect (Modern)

```typescript
// Creating
Effect.succeed(value)
Effect.fail(error)
Effect.tryPromise({ try: () => ..., catch: () => ... })

// Transforming
Effect.map(f)
Effect.flatMap(f)
Effect.catchAll(f)

// Running
Effect.runPromise(effect)
Effect.runSync(effect)
```

---

## Further Reading

- **Core Principles**: `code_guidelines/principles/functional_programming.md`
- **TypeScript Patterns**: `code_guidelines/languages/typescript/patterns.md`
- **Code Examples**: `code_guidelines/languages/typescript/examples.md`
- **Library Guide**: `code_guidelines/languages/typescript/libraries.md`

---

**Last Updated**: 2025-11-19  
**Maintained By**: Global AI Rules System  
**Status**: Active  
**Applies To**: All AI assistants (Cursor, Kimi, Claude, Gemini)