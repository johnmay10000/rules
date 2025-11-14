# TypeScript Functional Programming Style Guide for Kimi CLI

**Version**: 2.0.0  
**Last Updated**: 2025-11-14  
**Part of**: [KIMI.md](KIMI.md) Global Rule Set  
**Target**: TypeScript projects (Next.js, Supabase, Inngest, backend)

> **📖 Global Rules**: This document extends [KIMI.md](KIMI.md) with TypeScript-specific guidance. For mandatory universal rules (Git, documentation, testing, file size), see [KIMI.md](KIMI.md).

---

## Quick Links

- **Mandatory Rules**: See [KIMI.md](KIMI.md) sections 1-4  
- **FP Principles Deep Dive**: See [KIMI_FP_PRINCIPLES.md](KIMI_FP_PRINCIPLES.md)  
- **Workflow Guide**: See [KIMI_WORKFLOW_GUIDE.md](KIMI_WORKFLOW_GUIDE.md)  
- **Integration**: See [KIMI.md Integration](#kimi-integration) below  

---

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

## Kimi-Specific Patterns

### Parallel Verification of FP Pipelines

Kimi can verify multiple operations in your FP pipeline simultaneously:

```typescript
// Kimi excels at validating FP pipelines with parallel tool calls
const processUserPipeline = (userId: string) =>
  pipe(
    TE.Do,
    TE.bind('user', () => fetchUser(userId)),
    TE.bind('posts', ({ user }) => fetchPosts(user.id)),
    TE.bind('comments', ({ posts }) => fetchComments(posts)),
    TE.map(({ user, posts, comments }) => ({
      userProfile: buildProfile(user, posts),
      engagement: calculateEngagement(comments)
    }))
    // Kimi can verify all these bindings in parallel
    // Each function can be validated independently by subagents
  )
```

### Subagent Pattern for Complex Validations

Use Kimi's Task tool to spawn subagents for complex type checking:

```typescript
// Complex function that benefits from Kimi's subagent validation
const validateAndTransformData = (
  input: unknown
): TE.TaskEither<ValidationError, TransformedData> => 
  pipe(
    TE.fromEither(validateInput(input)),
    TE.chain(parseData),
    TE.chain(checkConstraints),
    TE.map(transform)
  )

// Kimi can spawn a subagent to verify:
// - Type signature correctness
// - Error handling completeness
// - Pipe composition validity
```

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

## 2. Monadic Composition with Kimi Validation

### ✅ Chain Operations with bind/flatMap (fp-ts)
```typescript
import * as TE from 'fp-ts/TaskEither'
import { pipe } from 'fp-ts/function'

type User = { id: string; name: string; email: string }
type Post = { id: string; userId: string; title: string }
type Comment = { id: string; postId: string; content: string }

// Kimi can validate each function's composition independently
declare function fetchUser(id: string): TE.TaskEither<FetchError, User>
declare function fetchPosts(userId: string): TE.TaskEither<FetchError, Post[]>
declare function fetchComments(postIds: string[]): TE.TaskEither<FetchError, Comment[]>

const getUserWithEngagement = (userId: string) =>
  pipe(
    TE.Do,
    TE.bind('user', () => fetchUser(userId)),
    TE.bind('posts', ({ user }) => fetchPosts(user.id)),
    TE.bind('comments', ({ posts }) => 
      fetchComments(posts.map(p => p.id))
    ),
    TE.map(({ user, posts, comments }) => ({
      user,
      postCount: posts.length,
      commentCount: comments.length,
      engagementRate: comments.length / Math.max(posts.length, 1)
    }))
    // Kimi can spawn subagents to verify:
    // - Type safety throughout the pipeline
    // - Error handling coverage
    // - Function composability
  )
```

### ✅ Do-Notation Style (Effect)
```typescript
import { Effect } from 'effect'

const getUserWithEngagementEffect = (userId: string) =>
  Effect.gen(function* () {
    // Kimi's parallel tool calls can verify each step
    const user = yield* fetchUserEffect(userId)  // ✓ Validated
    const posts = yield* fetchPostsEffect(user.id)  // ✓ Validated (in parallel with above verification)
    const comments = yield* fetchCommentsEffect(posts.map(p => p.id))  // ✓ Validated
    
    return {
      user,
      postCount: posts.length,
      commentCount: comments.length,
      engagementRate: comments.length / Math.max(posts.length, 1)
    }
  })
```

---

## 3. Immutable Data Structures

### ✅ Use Readonly and as const
```typescript
// Type-level immutability
type User = {
  readonly id: string
  readonly name: string
  readonly email: string
}

// Value-level immutability
const config = {
  apiUrl: 'https://api.example.com',
  timeout: 5000
} as const  // Makes all properties readonly

// Immutable updates with spread operator
const updateUser = (user: User, newEmail: string): User => ({
  ...user,
  email: newEmail
})

// Functional lens pattern (advanced)
import { Optional } from 'monocle-ts'

interface Company {
  readonly address: {
    readonly street: {
      readonly name: string
    }
  }
}

const streetName = Optional.fromPath<Company>()(['address', 'street', 'name'])
```

---

## 4. Railway-Oriented Programming with Kimi

### ✅ Chain Operations (fp-ts)
```typescript
import * as TE from 'fp-ts/TaskEither'
import { pipe } from 'fp-ts/function'

type ValidationError = { _tag: 'ValidationError'; message: string }
type ProcessingError = { _tag: 'ProcessingError'; message: string }
type SaveError = { _tag: 'SaveError'; message: string }

// Kimi can verify each step in isolation, then the whole pipeline
declare function validate(input: unknown): TE.TaskEither<ValidationError, ValidInput>
declare function process(data: ValidInput): TE.TaskEither<ProcessingError, ProcessedData>
declare function save(data: ProcessedData): TE.TaskEither<SaveError, Result>

type AllErrors = ValidationError | ProcessingError | SaveError

const processPipeline = (input: unknown): TE.TaskEither<AllErrors, Result> =>
  pipe(
    validate(input),
    TE.flatMap(process),
    TE.flatMap(save)
    // Kimi validates: ✓ Error type unions work correctly
    //                  ✓ Each function's type signature
    //                  ✓ Railway pattern implementation
  )
```

### ✅ Parallel Composition (fp-ts)
```typescript
import * as TE from 'fp-ts/TaskEither'
import * as A from 'fp-ts/Array'

// Kimi's parallel tool calls shine here
declare function validateUser(data: UserData): TE.TaskEither<Error, User>
declare function validatePost(data: PostData): TE.TaskEither<Error, Post>
declare function validateComment(data: CommentData): TE.TaskEither<Error, Comment>

const validateAll = (data: AllData) =>
  pipe(
    A.array.traverse(TE.taskEither)([
      validateUser(data.user),
      validatePost(data.post),
      validateComment(data.comment)
    ], x => x),
    TE.map(([user, post, comment]) => ({ user, post, comment }))
    // Kimi can spawn 3 subagents to validate each function in parallel
  )
```

---

## 5. Partial Application and Currying

```typescript
import { pipe } from 'fp-ts/function'

// Base function with full parameters
const createApiCall = 
  (baseUrl: string) => 
  (endpoint: string) => 
  (params: Record<string, string>) =>
  (): Promise<Response> =>
    fetch(`${baseUrl}${endpoint}?${new URLSearchParams(params)}`)

// Kimi can verify each partial application step
const api = createApiCall('https://api.example.com')
const usersApi = api('/users')  // ✓ Validated by Kimi
const getUser = usersApi({ id: '123' })  // ✓ Validated

// Utils with explicit typing
export const constant = <A>(a: A) => (): A => a
export const constVoid = constant(undefined)
```

---

## 6. Option/Maybe Type

```typescript
import * as O from 'fp-ts/Option'
import { pipe } from 'fp-ts/function'

type User = { id: string; name: string; email?: string }

// Safe property access
defaultUser = (user: User): User => ({
  ...user,
  email: pipe(
    O.fromNullable(user.email),
    O.getOrElse(() => 'default@example.com')
  )
})

// Working with arrays
const findUser = (users: User[], id: string): O.Option<User> =>
  pipe(
    users,
    A.findFirst(user => user.id === id)
  )

// Kimi validates option chaining
const getUserEmail = (users: User[], id: string): string =>
  pipe(
    findUser(users, id),
    O.chain(user => O.fromNullable(user.email)),
    O.getOrElse(() => 'no-email@example.com')
  )
```

---

## 7. Array/Collection Operations

```typescript
import * as A from 'fp-ts/Array'
import * as O from 'fp-ts/Option'
import { pipe } from 'fp-ts/function'

type Product = { name: string; price: number; inStock: boolean }

// Declarative array processing (Kimi validates each transformation)
const processProducts = (products: Product[]) =>
  pipe(
    products,
    A.filter(p => p.inStock),  // ✓ Validated
    A.map(p => ({ ...p, price: p.price * 1.1 })),  // ✓ Validated (parallel with above)
    A.sort((a, b) => b.price - a.price),  // ✓ Validated
    A.take(10)  // ✓ Validated
    // Kimi's parallel validation ensures each step is type-safe
  )

// Reduce and fold
const calculateTotal = (products: Product[]): number =>
  pipe(
    products,
    A.filter(p => p.inStock),
    A.map(p => p.price),
    A.reduce(0, (acc, price) => acc + price)
  )

// Partition
const partitionByStock = (products: Product[]) =>
  pipe(
    products,
    A.partition(p => p.inStock)
  )
```

---

## 8. Inngest + FP Patterns

```typescript
import { Inngest } from 'inngest'
import * as TE from 'fp-ts/TaskEither'
import { pipe } from 'fp-ts/function'

// FP-style Inngest functions
type UserOnboardingEvent = { name: 'user/created'; data: UserData }

const inngest = new Inngest({ name: 'My App' })

// Kimi validates: ✓ Type safety, ✓ Error handling, ✓ Pipeline composition
export const handleUserOnboarding = inngest.createFunction(
  { name: 'Onboard New User' },
  { event: 'user/created' },
  async ({ event, step }) =>
    pipe(
      TE.Do,
      TE.bind('user', () => createUserRecord(event.data)),
      TE.bind('welcomeEmail', ({ user }) => sendWelcomeEmail(user)),
      TE.bind('trialSetup', ({ user }) => setupTrial(user)),
      TE.map(({ user }) => ({ success: true, userId: user.id }))
    )()
)

// Helper functions with explicit types
declare function createUserRecord(data: UserData): TE.TaskEither<Error, User>
declare function sendWelcomeEmail(user: User): TE.TaskEither<Error, EmailReceipt>
declare function setupTrial(user: User): TE.TaskEither<Error, Trial>
```

---

## 9. Supabase + TaskEither

```typescript
import { supabase } from './supabase'
import * as TE from 'fp-ts/TaskEither'
import { pipe } from 'fp-ts/function'

// FP wrapper around Supabase
type DbError = { _tag: 'DbError'; message: string; details: unknown }

const queryToTaskEither = <T>(
  query: Promise<{ data: T | null; error: unknown }>
): TE.TaskEither<DbError, T> =>
  TE.tryCatch(
    async () => {
      const { data, error } = await query
      if (error) throw error
      if (data === null) throw new Error('No data returned')
      return data
    },
    (error): DbError => ({
      _tag: 'DbError',
      message: String(error),
      details: error
    })
  )

// Type-safe queries that Kimi can validate
interface User { id: string; email: string; profile: Profile }
interface Profile { name: string; avatar: string }

const getUserWithProfile = (userId: string) =>
  pipe(
    queryToTaskEither(
      supabase.from('users').select('*, profile(*)').eq('id', userId).single()
    ),
    TE.map(user => ({
      id: user.id,
      email: user.email,
      name: user.profile.name,
      avatar: user.profile.avatar
    }))
    // Kimi validates: ✓ Type transformations
    //                  ✓ Error handling
    //                  ✓ Query safety
  )
```

---

## 10. Testing FP Code with Kimi

```typescript
import * as E from 'fp-ts/Either'
import { pipe } from 'fp-ts/function'

// Test that Kimi can help verify
interface UserData { name: string; email: string; age: number }

const validateUser = (data: unknown): E.Either<ValidationError, UserData> =>
  pipe(
    E.Do,
    E.bind('name', () => 
      typeof (data as any)?.name === 'string' && (data as any).name.length > 0
        ? E.right((data as any).name)
        : E.left({ _tag: 'ValidationError', message: 'Invalid name' })
    ),
    E.bind('email', () =>
      /\S+@\S+\.\S+/.test((data as any)?.email || '')
        ? E.right((data as any).email)
        : E.left({ _tag: 'ValidationError', message: 'Invalid email' })
    ),
    E.bind('age', () =>
      typeof (data as any)?.age === 'number' && (data as any).age >= 0
        ? E.right((data as any).age)
        : E.left({ _tag: 'ValidationError', message: 'Invalid age' })
    ),
    E.map(({ name, email, age }) => ({ name, email, age }))
  )

// Kimi can spawn a subagent to:
// 1. Verify type signatures match
// 2. Check all validation rules are correct
// 3. Ensure error messages are descriptive
// 4. Validate the E.map transformation
```

---

## 11. Kimi CLI Integration

### Project Setup

1. **Environment Variable**:
   ```bash
   export KIMI_RULES_PATH="$HOME/projects/rules"
   export KIMI_TYPESCRIPT_RULES="$KIMI_RULES_PATH/kimi/typescript-fp-style-guide.md"
   ```

2. **Project-Specific Rules** (`.kimirules`):
   ```markdown
   # .kimirules for TypeScript Project
   
   ## TypeScript FP Rules
   - Use fp-ts or Effect for all operations (no try/catch)
   - Prefer TaskEither over Promise for async operations
   - Use Option for nullable values (no null/undefined checks)
   - Immutable data with readonly and as const
   - Railway-oriented programming for error handling
   
   ## Kimi-Specific
   - Use parallel tool calls to verify multiple pure functions
   - Spawn subagents for complex pipeline validation
   - Validate Effect/fp-ts imports and usage patterns
   - Check for proper type safety throughout code
   - Verify no mutations of readonly properties
   ```

3. **Daily Work**: Kimi follows 3-tier structure automatically:
   - Tier 1: ARCHITECTURE_PLAN.md (strategic)
   - Tier 2: docs/plans/ (tactical)
   - Tier 3: docs/YYYY_MM_DD/ (execution)

4. **Verification**: Kimi can validate:
   - ✓ Type safety in FP pipelines
   - ✓ Proper Either/Option usage
   - ✓ Railway-oriented patterns
   - ✓ Immutability with readonly
   - ✓ Function composition correctness

---

## 12. Next.js API Routes (FP-Style)

```typescript
// app/api/users/[id]/route.ts
import { NextRequest, NextResponse } from 'next/server'
import * as TE from 'fp-ts/TaskEither'
import { pipe } from 'fp-ts/function'

type RouteError = 
  | { _tag: 'InvalidId' }
  | { _tag: 'UserNotFound' }
  | { _tag: 'ServerError'; message: string }

// Type-safe API route that Kimi can validate
export async function GET(
  request: NextRequest,
  { params }: { params: { id: string } }
) {
  return pipe(
    TE.Do,
    TE.bind('userId', () => 
      TE.fromEither(validateId(params.id))
    ),
    TE.bind('user', ({ userId }) => 
      fetchUserFromDb(userId)
    ),
    TE.match(
      (error) => NextResponse.json(
        { error }, 
        { status: mapErrorToStatus(error) }
      ),
      (user) => NextResponse.json(user)
    )
  )()
}

// Kimi validates: ✓ Error type safety
//                  ✓ Proper TaskEither usage
//                  ✓ Response handling
//                  ✓ Can spawn subagent for error mapping validation
```

---

## Summary

This guide provides TypeScript-specific FP patterns optimized for Kimi CLI usage with Next.js, Supabase, and Inngest ecosystems. Kimi excels at validating FP pipelines through parallel tool calls and subagent delegation.

**Key Takeaways**:
- Use fp-ts or Effect for railway-oriented programming
- Prefer TaskEither over Promise for composable async operations
- Immutable data with readonly and as const
- Option for nullable values (no null checks)
- Leverage Kimi's parallel validation for complex pipelines
- Spawn subagents for type safety verification

**Next**: See [KIMI_FP_PRINCIPLES.md](KIMI_FP_PRINCIPLES.md) for deeper FP concepts.

---

**Last Updated**: 2025-11-14  
**Maintained By**: Kimi CLI Global Rules System  
**Status**: Active
