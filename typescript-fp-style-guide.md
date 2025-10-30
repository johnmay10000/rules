# TypeScript Functional Programming Style Guide
## For Next.js, Supabase & Inngest Projects

### Core Principles

1. **Immutability**: Use `readonly`, `as const`, never mutate
2. **Pure Functions**: Explicit side effect management
3. **Type Safety**: Leverage TypeScript's type system to the fullest
4. **Composability**: Build from small, reusable functions
5. **Railway-Oriented Programming**: Chain operations with Either/Result

---

## Required Libraries

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

// Alternative: Effect (more powerful, better DX)
import { Effect, Exit, pipe as Epipe } from 'effect'
```

**Choose one**: `fp-ts` (closer to Haskell) or `Effect` (more ergonomic). Examples show both.

---

## 1. Error Handling with Either/Result

### ❌ Avoid: Try/Catch
```typescript
async function fetchUser(id: string): Promise<User> {
  try {
    const response = await fetch(`/api/users/${id}`)
    if (!response.ok) throw new Error('Failed to fetch')
    return await response.json()
  } catch (error) {
    throw error
  }
}
```

### ✅ Prefer: TaskEither (fp-ts)
```typescript
import * as TE from 'fp-ts/TaskEither'
import * as E from 'fp-ts/Either'
import { pipe } from 'fp-ts/function'

type FetchError = 
  | { _tag: 'NetworkError'; message: string }
  | { _tag: 'ParseError'; message: string }
  | { _tag: 'NotFound' }

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
```

### ✅ Prefer: Effect (cleaner syntax)
```typescript
import { Effect, pipe } from 'effect'

type FetchError = 
  | { readonly _tag: 'NetworkError'; readonly message: string }
  | { readonly _tag: 'ParseError'; readonly message: string }
  | { readonly _tag: 'NotFound' }

const fetchUser = (id: string): Effect.Effect<User, FetchError> =>
  Effect.tryPromise({
    try: async () => {
      const response = await fetch(`/api/users/${id}`)
      if (!response.ok) {
        if (response.status === 404) {
          throw { _tag: 'NotFound' as const }
        }
        throw {
          _tag: 'NetworkError' as const,
          message: `Status ${response.status}`
        }
      }
      return response.json()
    },
    catch: (error): FetchError => {
      if (typeof error === 'object' && error !== null && '_tag' in error) {
        return error as FetchError
      }
      return {
        _tag: 'ParseError',
        message: String(error)
      }
    }
  })
```

---

## 2. Monadic Composition (Do Notation Style)

### ✅ Chain Operations with bind/flatMap (fp-ts)
```typescript
import * as TE from 'fp-ts/TaskEither'
import { pipe } from 'fp-ts/function'

type AppError = string

const validatePositive = (x: number): E.Either<AppError, number> =>
  x > 0 ? E.right(x) : E.left('Must be positive')

const safeSqrt = (x: number): E.Either<AppError, number> =>
  E.right(Math.sqrt(x))

const safeReciprocal = (x: number): E.Either<AppError, number> =>
  x === 0 ? E.left('Cannot divide by zero') : E.right(1 / x)

// Monadic composition - like Haskell's do notation
const processValue = (x: number): E.Either<AppError, number> =>
  pipe(
    validatePositive(x),
    E.flatMap(safeSqrt),
    E.flatMap(safeReciprocal),
    E.map(v => v * 100)
  )

// With TaskEither for async
const processValueAsync = (x: number): TE.TaskEither<AppError, number> =>
  pipe(
    TE.right(x),
    TE.flatMap(v => TE.fromEither(validatePositive(v))),
    TE.flatMap(v => TE.fromEither(safeSqrt(v))),
    TE.flatMap(v => TE.fromEither(safeReciprocal(v))),
    TE.map(v => v * 100)
  )
```

### ✅ Effect Do Notation (Even Closer to Haskell)
```typescript
import { Effect, pipe } from 'effect'

const processValue = (x: number): Effect.Effect<number, AppError> =>
  Effect.gen(function* (_) {
    // This looks like Haskell's do notation!
    const validated = yield* _(validatePositive(x))
    const sqrt = yield* _(safeSqrt(validated))
    const reciprocal = yield* _(safeReciprocal(sqrt))
    return reciprocal * 100
  })

// Or using pipe for point-free style
const processValuePointFree = (x: number): Effect.Effect<number, AppError> =>
  pipe(
    validatePositive(x),
    Effect.flatMap(safeSqrt),
    Effect.flatMap(safeReciprocal),
    Effect.map(v => v * 100)
  )
```

---

## 3. Currying and Partial Application

### ✅ Curry Functions for Composition
```typescript
// Manual currying
const curry = <A, B, C>(f: (a: A, b: B) => C) =>
  (a: A) => (b: B) => f(a, b)

const curry3 = <A, B, C, D>(f: (a: A, b: B, c: C) => D) =>
  (a: A) => (b: B) => (c: C) => f(a, b, c)

// Example: Database query builder
const queryBuilder = curry3(
  (table: string, select: string[], where: Record<string, unknown>) =>
    supabase
      .from(table)
      .select(select.join(','))
      .match(where)
)

// Partially apply to create specialized queries
const usersQuery = queryBuilder('users')
const userFields = usersQuery(['id', 'name', 'email'])
const activeUsers = userFields({ active: true })

// Or chain them
const getUserById = (id: string) =>
  queryBuilder('users')(['*'])({ id })
```

---

## 4. Function Composition

### ✅ Build Complex Pipelines (fp-ts)
```typescript
import { pipe, flow } from 'fp-ts/function'
import * as A from 'fp-ts/Array'
import * as O from 'fp-ts/Option'

// Example: Data transformation pipeline
interface RawData {
  value: string
  timestamp: string
}

interface ProcessedData {
  value: number
  date: Date
}

// Small, pure functions
const parseValue = (s: string): O.Option<number> => {
  const n = Number(s)
  return isNaN(n) ? O.none : O.some(n)
}

const parseDate = (s: string): O.Option<Date> => {
  const d = new Date(s)
  return isNaN(d.getTime()) ? O.none : O.some(d)
}

const processItem = (raw: RawData): O.Option<ProcessedData> =>
  pipe(
    O.Do,
    O.apS('value', parseValue(raw.value)),
    O.apS('date', parseDate(raw.timestamp)),
    O.map(({ value, date }) => ({ value, date }))
  )

// Compose into pipeline using flow (left-to-right)
const processData = flow(
  A.map(processItem),
  A.compact  // Remove None values
)

// Or using pipe for more flexibility
const processDataWithPipe = (data: RawData[]) =>
  pipe(
    data,
    A.map(processItem),
    A.compact,
    A.filter(d => d.value > 0),
    A.sortBy([({ date }) => date.getTime()])
  )
```

---

## 5. Immutable Data Structures

### ✅ Use Readonly and Type-Safe Updates
```typescript
// Use readonly everywhere
interface User {
  readonly id: string
  readonly name: string
  readonly email: string
  readonly settings: {
    readonly theme: 'light' | 'dark'
    readonly notifications: boolean
  }
}

// Helper for immutable updates
const updateUser = <K extends keyof User>(
  user: User,
  key: K,
  value: User[K]
): User => ({
  ...user,
  [key]: value
})

// Deep updates with lenses (optional, but powerful)
import { Lens } from 'monocle-ts'

const themeLens = Lens.fromPath<User>()(['settings', 'theme'])

const setTheme = (theme: 'light' | 'dark') => (user: User): User =>
  themeLens.set(theme)(user)

// Or using immer for complex updates (pragmatic choice)
import { produce } from 'immer'

const updateUserSettings = (
  user: User,
  updater: (draft: User['settings']) => void
): User =>
  produce(user, draft => {
    updater(draft.settings)
  })
```

---

## 6. Railway-Oriented Programming

### ✅ Supabase Query with Error Handling
```typescript
import * as TE from 'fp-ts/TaskEither'
import { pipe } from 'fp-ts/function'
import { createClient } from '@supabase/supabase-js'

type DbError = 
  | { _tag: 'QueryError'; message: string }
  | { _tag: 'NotFound' }
  | { _tag: 'ValidationError'; errors: string[] }

const supabase = createClient(url, key)

const validateUser = (user: User): E.Either<DbError, User> =>
  user.email.includes('@')
    ? E.right(user)
    : E.left({
        _tag: 'ValidationError',
        errors: ['Invalid email']
      })

const fetchUser = (id: string): TE.TaskEither<DbError, User> =>
  pipe(
    TE.tryCatch(
      () => supabase.from('users').select('*').eq('id', id).single(),
      (error): DbError => ({
        _tag: 'QueryError',
        message: String(error)
      })
    ),
    TE.filterOrElse(
      (result) => result.data !== null,
      (): DbError => ({ _tag: 'NotFound' })
    ),
    TE.map(result => result.data as User),
    TE.flatMap(user => TE.fromEither(validateUser(user)))
  )

const updateUser = (
  id: string,
  updates: Partial<User>
): TE.TaskEither<DbError, User> =>
  pipe(
    TE.tryCatch(
      () =>
        supabase
          .from('users')
          .update(updates)
          .eq('id', id)
          .select()
          .single(),
      (error): DbError => ({
        _tag: 'QueryError',
        message: String(error)
      })
    ),
    TE.filterOrElse(
      (result) => result.data !== null,
      (): DbError => ({ _tag: 'NotFound' })
    ),
    TE.map(result => result.data as User)
  )

// Compose operations
const updateAndFetch = (
  id: string,
  updates: Partial<User>
): TE.TaskEither<DbError, User> =>
  pipe(
    updateUser(id, updates),
    TE.flatMap(() => fetchUser(id))
  )
```

---

## 7. Next.js Server Actions with Effect

### ✅ Type-Safe Server Actions
```typescript
'use server'

import { Effect, pipe } from 'effect'
import { revalidatePath } from 'next/cache'

type ActionError = 
  | { readonly _tag: 'ValidationError'; readonly errors: string[] }
  | { readonly _tag: 'Unauthorized' }
  | { readonly _tag: 'DatabaseError'; readonly message: string }

type ActionResult<A> =
  | { success: true; data: A }
  | { success: false; error: ActionError }

// Convert Effect to ActionResult
const runAction = <A>(
  effect: Effect.Effect<A, ActionError>
): Promise<ActionResult<A>> =>
  Effect.runPromise(
    pipe(
      effect,
      Effect.map(data => ({ success: true as const, data })),
      Effect.catchAll(error =>
        Effect.succeed({ success: false as const, error })
      )
    )
  )

// Validate input
const validateCreateUser = (
  input: unknown
): Effect.Effect<{ name: string; email: string }, ActionError> =>
  Effect.gen(function* (_) {
    if (
      typeof input !== 'object' ||
      input === null ||
      !('name' in input) ||
      !('email' in input)
    ) {
      return yield* _(
        Effect.fail({
          _tag: 'ValidationError' as const,
          errors: ['Invalid input shape']
        })
      )
    }

    const { name, email } = input as { name: string; email: string }

    const errors: string[] = []
    if (typeof name !== 'string' || name.length < 2) {
      errors.push('Name must be at least 2 characters')
    }
    if (typeof email !== 'string' || !email.includes('@')) {
      errors.push('Invalid email')
    }

    if (errors.length > 0) {
      return yield* _(
        Effect.fail({ _tag: 'ValidationError' as const, errors })
      )
    }

    return { name, email }
  })

// Database operation
const createUserInDb = (
  data: { name: string; email: string }
): Effect.Effect<User, ActionError> =>
  Effect.tryPromise({
    try: async () => {
      const { data: user, error } = await supabase
        .from('users')
        .insert(data)
        .select()
        .single()

      if (error) throw error
      return user as User
    },
    catch: (error): ActionError => ({
      _tag: 'DatabaseError',
      message: String(error)
    })
  })

// Composed action
export async function createUserAction(
  input: unknown
): Promise<ActionResult<User>> {
  const effect = pipe(
    validateCreateUser(input),
    Effect.flatMap(createUserInDb),
    Effect.tap(() =>
      Effect.sync(() => revalidatePath('/users'))
    )
  )

  return runAction(effect)
}
```

---

## 8. Inngest Functions with Railway Pattern

### ✅ Type-Safe Inngest Functions
```typescript
import { inngest } from './client'
import * as TE from 'fp-ts/TaskEither'
import { pipe } from 'fp-ts/function'

type ProcessError = 
  | { _tag: 'FetchError'; message: string }
  | { _tag: 'ProcessingError'; message: string }
  | { _tag: 'SaveError'; message: string }

interface ProcessData {
  readonly id: string
  readonly rawData: unknown
}

interface ProcessedResult {
  readonly id: string
  readonly processed: boolean
  readonly timestamp: Date
}

const fetchData = (id: string): TE.TaskEither<ProcessError, unknown> =>
  TE.tryCatch(
    async () => {
      const response = await fetch(`/api/data/${id}`)
      return response.json()
    },
    (error): ProcessError => ({
      _tag: 'FetchError',
      message: String(error)
    })
  )

const processData = (
  data: unknown
): TE.TaskEither<ProcessError, ProcessedData> =>
  TE.tryCatch(
    async () => {
      // Processing logic
      return processed
    },
    (error): ProcessError => ({
      _tag: 'ProcessingError',
      message: String(error)
    })
  )

const saveResult = (
  data: ProcessedData
): TE.TaskEither<ProcessError, ProcessedResult> =>
  TE.tryCatch(
    async () => {
      const { error } = await supabase
        .from('processed')
        .insert(data)

      if (error) throw error

      return {
        id: data.id,
        processed: true,
        timestamp: new Date()
      }
    },
    (error): ProcessError => ({
      _tag: 'SaveError',
      message: String(error)
    })
  )

// Composed pipeline
const processPipeline = (
  id: string
): TE.TaskEither<ProcessError, ProcessedResult> =>
  pipe(
    fetchData(id),
    TE.flatMap(processData),
    TE.flatMap(saveResult)
  )

// Inngest function
export const processDataFunction = inngest.createFunction(
  { id: 'process-data' },
  { event: 'data.process' },
  async ({ event, step }) => {
    const result = await processPipeline(event.data.id)()

    return pipe(
      result,
      E.match(
        (error) => {
          // Log error, maybe retry
          console.error('Processing failed:', error)
          throw new Error(error.message)
        },
        (success) => {
          console.log('Processing succeeded:', success)
          return success
        }
      )
    )
  }
)
```

---

## 9. Higher-Kinded Types Simulation

### ✅ Generic Patterns with HKT
```typescript
// Simulate Higher-Kinded Types
interface HKT<URI, A> {
  readonly _URI: URI
  readonly _A: A
}

interface Functor<F> {
  readonly URI: F
  readonly map: <A, B>(fa: HKT<F, A>, f: (a: A) => B) => HKT<F, B>
}

interface Monad<M> extends Functor<M> {
  readonly of: <A>(a: A) => HKT<M, A>
  readonly flatMap: <A, B>(
    ma: HKT<M, A>,
    f: (a: A) => HKT<M, B>
  ) => HKT<M, B>
}

// Implement for Option
type OptionURI = 'Option'

type OptionHKT<A> = HKT<OptionURI, A>

const optionMonad: Monad<OptionURI> = {
  URI: 'Option',
  map: (fa, f) => O.map(f)(fa as O.Option<never>),
  of: O.some,
  flatMap: (ma, f) =>
    O.flatMap((a: never) => f(a) as O.Option<never>)(ma as O.Option<never>)
}

// Generic traverse function
const traverse = <F, A, B>(
  M: Monad<F>,
  items: A[],
  f: (a: A) => HKT<F, B>
): HKT<F, B[]> => {
  return items.reduce(
    (acc, item) =>
      M.flatMap(acc, (bs) =>
        M.map(f(item), (b) => [...bs, b])
      ),
    M.of([]) as HKT<F, B[]>
  )
}
```

---

## 10. Pattern Matching for ADTs

### ✅ Discriminated Unions with Exhaustiveness
```typescript
// Define ADTs with discriminated unions
type RemoteData<E, A> =
  | { readonly _tag: 'NotAsked' }
  | { readonly _tag: 'Loading' }
  | { readonly _tag: 'Failure'; readonly error: E }
  | { readonly _tag: 'Success'; readonly data: A }

// Pattern matching with exhaustiveness checking
const matchRemoteData = <E, A, B>(
  rd: RemoteData<E, A>,
  patterns: {
    NotAsked: () => B
    Loading: () => B
    Failure: (error: E) => B
    Success: (data: A) => B
  }
): B => {
  switch (rd._tag) {
    case 'NotAsked':
      return patterns.NotAsked()
    case 'Loading':
      return patterns.Loading()
    case 'Failure':
      return patterns.Failure(rd.error)
    case 'Success':
      return patterns.Success(rd.data)
  }
}

// Or use fp-ts's match
import { match } from 'fp-ts/Either'

const renderUserState = (state: RemoteData<string, User>) =>
  matchRemoteData(state, {
    NotAsked: () => <div>Click to load</div>,
    Loading: () => <div>Loading...</div>,
    Failure: (error) => <div>Error: {error}</div>,
    Success: (user) => <div>Hello, {user.name}!</div>
  })
```

---

## Complete Example: Next.js API Route

```typescript
// app/api/users/[id]/route.ts
import { NextRequest, NextResponse } from 'next/server'
import * as TE from 'fp-ts/TaskEither'
import * as E from 'fp-ts/Either'
import { pipe } from 'fp-ts/function'

type ApiError = 
  | { _tag: 'ValidationError'; message: string }
  | { _tag: 'NotFound' }
  | { _tag: 'DatabaseError'; message: string }
  | { _tag: 'UnexpectedError'; message: string }

// Validation
const validateUserId = (id: string): E.Either<ApiError, string> =>
  id.length > 0
    ? E.right(id)
    : E.left({
        _tag: 'ValidationError',
        message: 'Invalid user ID'
      })

// Database operation
const getUserFromDb = (id: string): TE.TaskEither<ApiError, User> =>
  TE.tryCatch(
    async () => {
      const { data, error } = await supabase
        .from('users')
        .select('*')
        .eq('id', id)
        .single()

      if (error) throw error
      if (!data) {
        throw { _tag: 'NotFound' }
      }

      return data as User
    },
    (error): ApiError => {
      if (
        typeof error === 'object' &&
        error !== null &&
        '_tag' in error &&
        error._tag === 'NotFound'
      ) {
        return { _tag: 'NotFound' }
      }
      return {
        _tag: 'DatabaseError',
        message: String(error)
      }
    }
  )

// Transform to API response
const toApiResponse = (user: User) => ({
  id: user.id,
  name: user.name,
  email: user.email,
  createdAt: user.created_at
})

// Complete pipeline
const getUser = (
  id: string
): TE.TaskEither<ApiError, ReturnType<typeof toApiResponse>> =>
  pipe(
    TE.fromEither(validateUserId(id)),
    TE.flatMap(getUserFromDb),
    TE.map(toApiResponse)
  )

// Handle errors consistently
const handleError = (error: ApiError): NextResponse => {
  switch (error._tag) {
    case 'ValidationError':
      return NextResponse.json(
        { error: error.message },
        { status: 400 }
      )
    case 'NotFound':
      return NextResponse.json(
        { error: 'User not found' },
        { status: 404 }
      )
    case 'DatabaseError':
      return NextResponse.json(
        { error: 'Database error' },
        { status: 500 }
      )
    case 'UnexpectedError':
      return NextResponse.json(
        { error: 'Unexpected error' },
        { status: 500 }
      )
  }
}

// Route handler
export async function GET(
  request: NextRequest,
  { params }: { params: { id: string } }
) {
  const result = await getUser(params.id)()

  return pipe(
    result,
    E.match(
      handleError,
      (user) => NextResponse.json(user)
    )
  )
}
```

---

## Style Rules Summary

1. **No try/catch**: Use `TaskEither`, `Effect`, or `tryCatch` wrappers
2. **No mutations**: Use `readonly`, `as const`, immutable updates
3. **Explicit effects**: Wrap side effects in `Task`, `IO`, or `Effect`
4. **Compose, don't nest**: Use `pipe`, `flow`, monadic composition
5. **Curry when useful**: Enable partial application
6. **Type everything**: Use strict TypeScript, discriminated unions
7. **Pattern match**: Use exhaustive switch on `_tag`
8. **Small functions**: Compose for complexity
9. **Railway-oriented**: Chain Either/TaskEither, handle at boundaries
10. **Point-free when clear**: Balance readability with elegance

---

## File Organization

```
src/
├── types/              # ADTs, type definitions
│   ├── errors.ts
│   ├── domain.ts
│   └── api.ts
├── lib/
│   ├── fp/            # FP utilities
│   │   ├── result.ts
│   │   ├── validation.ts
│   │   └── compose.ts
│   ├── db/            # Database operations (TaskEither)
│   │   ├── users.ts
│   │   └── posts.ts
│   └── api/           # API clients (TaskEither)
├── app/
│   └── api/           # Next.js routes
└── inngest/           # Inngest functions
    └── functions/
```

---

## Tooling

```json
// tsconfig.json
{
  "compilerOptions": {
    "strict": true,
    "strictNullChecks": true,
    "noUncheckedIndexedAccess": true,
    "noImplicitAny": true,
    "exactOptionalPropertyTypes": true
  }
}
```

```json
// .eslintrc.json
{
  "rules": {
    "@typescript-eslint/no-explicit-any": "error",
    "@typescript-eslint/explicit-function-return-type": "warn",
    "functional/immutable-data": "error",
    "functional/no-let": "error",
    "functional/no-loop-statements": "error"
  }
}
```

---

## Additional Resources

- **fp-ts**: https://gcanti.github.io/fp-ts/
- **Effect**: https://effect.website/
- **monocle-ts** (Lenses): https://github.com/gcanti/monocle-ts
- **io-ts** (Runtime validation): https://github.com/gcanti/io-ts

This guide brings Haskell's functional purity to TypeScript while embracing the ecosystem's strengths.
