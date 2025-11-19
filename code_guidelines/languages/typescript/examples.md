---
title: TypeScript Functional Programming Examples
language: typescript
category: code_guidelines
type: examples
applies_to: [cursor, kimi, claude, gemini]
version: 2.0.0
last_updated: 2025-11-19
---

# TypeScript Functional Programming Examples

Comprehensive code examples demonstrating functional programming patterns in TypeScript using fp-ts and Effect.

---

## Example 1: Error Handling with Either/TaskEither

### Before (Imperative with Try/Catch)

```typescript
// BAD: Exceptions are not part of the type signature
async function fetchUser(id: string): Promise<User> {
  try {
    const response = await fetch(`/api/users/${id}`)
    if (!response.ok) {
      throw new Error(`HTTP error! status: ${response.status}`)
    }
    return await response.json()
  } catch (error) {
    console.error('Failed to fetch user:', error)
    throw error // Who knows what might be thrown?
  }
}

// Usage - easy to forget error handling
async function getUserProfile(id: string) {
  const user = await fetchUser(id) // Might throw!
  return renderProfile(user)
}

// Runtime error if fetch fails
getUserProfile('123') // Unhandled promise rejection if network fails
```

### After (Functional with TaskEither)

```typescript
import * as TE from 'fp-ts/TaskEither'
import * as E from 'fp-ts/Either'
import { pipe } from 'fp-ts/function'

// Define all possible errors explicitly
type FetchError = 
  | { readonly _tag: 'NetworkError'; readonly message: string }
  | { readonly _tag: 'NotFound' }
  | { readonly _tag: 'Unauthorized' }
  | { readonly _tag: 'ParseError'; readonly message: string }

interface User {
  readonly id: string
  readonly name: string
  readonly email: string
}

// Function returns TaskEither - errors are part of the type!
const fetchUser = (id: string): TE.TaskEither<FetchError, User> =>
  pipe(
    TE.tryCatch(
      () => fetch(`/api/users/${id}`),
      (error): FetchError => ({
        _tag: 'NetworkError',
        message: error instanceof Error ? error.message : 'Unknown network error'
      })
    ),
    TE.flatMap(response => {
      if (response.ok) {
        return TE.right(response)
      }
      
      // Map HTTP status to specific error types
      const error: FetchError = 
        response.status === 404 ? { _tag: 'NotFound' } :
        response.status === 401 ? { _tag: 'Unauthorized' } :
        { _tag: 'NetworkError', message: `HTTP ${response.status}` }
      
      return TE.left(error)
    }),
    TE.flatMap(response =>
      TE.tryCatch(
        () => response.json() as Promise<User>,
        (error): FetchError => ({
          _tag: 'ParseError',
          message: error instanceof Error ? error.message : 'Failed to parse JSON'
        })
      )
    )
  )

// Usage - explicit error handling required
const getUserProfile = (id: string): TE.TaskEither<FetchError, string> =>
  pipe(
    fetchUser(id),
    TE.map(user => `Profile: ${user.name} <${user.email}>`)
  )

// Run the TaskEither
async function main() {
  const result = await getUserProfile('123')()
  
  // Must handle both success and error cases
  if (E.isRight(result)) {
    console.log('Success:', result.right)
  } else {
    // Type-safe error handling - all cases known at compile time
    switch (result.left._tag) {
      case 'NetworkError':
        console.error('Network failed:', result.left.message)
        break
      case 'NotFound':
        console.error('User not found')
        break
      case 'Unauthorized':
        console.error('Access denied')
        break
      case 'ParseError':
        console.error('Invalid response:', result.left.message)
        break
    }
  }
}

main()
```

### Alternative: Using Effect (Modern Approach)

```typescript
import { Effect } from 'effect'

type FetchError = 
  | { readonly _tag: 'NetworkError'; readonly message: string }
  | { readonly _tag: 'NotFound' }
  | { readonly _tag: 'Unauthorized' }
  | { readonly _tag: 'ParseError'; readonly message: string }

interface User {
  readonly id: string
  readonly name: string
  readonly email: string
}

const fetchUser = (id: string): Effect.Effect<User, FetchError> =>
  Effect.tryPromise({
    try: () => fetch(`/api/users/${id}`),
    catch: (error): FetchError => ({
      _tag: 'NetworkError',
      message: error instanceof Error ? error.message : 'Unknown error'
    })
  }).pipe(
    Effect.flatMap(response =>
      response.ok
        ? Effect.succeed(response)
        : Effect.fail(
            response.status === 404 ? { _tag: 'NotFound' } as const :
            response.status === 401 ? { _tag: 'Unauthorized' } as const :
            { _tag: 'NetworkError', message: `HTTP ${response.status}` }
          )
    ),
    Effect.flatMap(response =>
      Effect.tryPromise({
        try: () => response.json() as Promise<User>,
        catch: (error): FetchError => ({
          _tag: 'ParseError',
          message: error instanceof Error ? error.message : 'Parse failed'
        })
      })
    )
  )

const getUserProfile = (id: string): Effect.Effect<string, FetchError> =>
  Effect.map(fetchUser(id), user => `Profile: ${user.name} <${user.email}>`)

// Usage
async function main() {
  const result = await Effect.runPromise(getUserProfile('123'))
  console.log(result)
}

// Or with error handling
Effect.runPromiseExit(getUserProfile('123')).then(exit => {
  if (exit._tag === 'Success') {
    console.log('Success:', exit.value)
  } else {
    console.log('Error:', exit.cause)
  }
})
```

---

## Example 2: Monadic Composition for Form Validation

### Scenario: Complex Form Validation Pipeline

```typescript
import * as E from 'fp-ts/Either'
import * as TE from 'fp-ts/TaskEither'
import { pipe } from 'fp-ts/function'

// Domain types
interface Email {
  readonly _tag: 'Email'
  readonly value: string
}

interface Password {
  readonly _tag: 'Password'
  readonly value: string
}

interface UserRegistration {
  readonly email: Email
  readonly password: Password
  readonly age: number
}

// Error types
type ValidationError = 
  | { readonly _tag: 'InvalidEmail'; readonly reason: string }
  | { readonly _tag: 'InvalidPassword'; readonly reason: string }
  | { readonly _tag: 'TooYoung'; readonly requiredAge: number }

// Validation functions (each returns Either)
const validateEmail = (raw: string): E.Either<ValidationError, Email> =>
  raw.includes('@') && raw.length > 3
    ? E.right({ _tag: 'Email', value: raw })
    : E.left({ _tag: 'InvalidEmail', reason: 'Email must contain @ and be at least 4 characters' })

const validatePassword = (raw: string): E.Either<ValidationError, Password> =>
  raw.length >= 8 && /[A-Z]/.test(raw) && /[0-9]/.test(raw)
    ? E.right({ _tag: 'Password', value: raw })
    : E.left({ 
        _tag: 'InvalidPassword', 
        reason: 'Password must be at least 8 characters with uppercase and number' 
      })

const validateAge = (age: number): E.Either<ValidationError, number> =>
  age >= 18
    ? E.right(age)
    : E.left({ _tag: 'TooYoung', requiredAge: 18 })

// Simulate async email uniqueness check
const checkEmailUnique = (email: Email): TE.TaskEither<ValidationError, Email> =>
  TE.tryCatch(
    async () => {
      // Simulate API call
      const takenEmails = ['test@example.com', 'admin@example.com']
      if (takenEmails.includes(email.value)) {
        throw new Error('Email already taken')
      }
      return email
    },
    () => ({ _tag: 'InvalidEmail', reason: 'Email already registered' })
  )

// Railway-oriented validation pipeline
const validateRegistration = (
  rawEmail: string,
  rawPassword: string,
  rawAge: number
): TE.TaskEither<ValidationError, UserRegistration> =>
  pipe(
    // Start with synchronous validations
    E.Do,
    E.bind('email', () => validateEmail(rawEmail)),
    E.bind('password', () => validatePassword(rawPassword)),
    E.bind('age', () => validateAge(rawAge)),
    // Convert to TaskEither for async validation
    TE.fromEither,
    // Check email uniqueness (async)
    TE.bind('uniqueEmail', ({ email }) => checkEmailUnique(email)),
    // Build final result
    TE.map(({ email, password, age }) => ({
      email,
      password,
      age
    }))
  )

// Usage
async function registerUser(data: {
  email: string
  password: string
  age: number
}) {
  const result = await validateRegistration(data.email, data.password, data.age)()
  
  if (E.isRight(result)) {
    console.log('Registration valid:', result.right)
    // Proceed with registration...
  } else {
    const error = result.left
    switch (error._tag) {
      case 'InvalidEmail':
        console.error('Email error:', error.reason)
        break
      case 'InvalidPassword':
        console.error('Password error:', error.reason)
        break
      case 'TooYoung':
        console.error(`Must be at least ${error.requiredAge} years old`)
        break
    }
  }
}

// Test cases
registerUser({
  email: 'test@example.com',
  password: 'weak',
  age: 16
})
// Email error: Email already registered

registerUser({
  email: 'new@example.com',
  password: 'StrongPass123',
  age: 16
})
// Must be at least 18 years old

registerUser({
  email: 'new@example.com',
  password: 'StrongPass123',
  age: 25
})
// Registration valid: { email: {...}, password: {...}, age: 25 }
```

---

## Example 3: Reader Pattern for Dependency Injection

### Scenario: Service Layer with Configurable Dependencies

```typescript
import * as R from 'fp-ts/Reader'
import { pipe } from 'fp-ts/function'

// Dependencies interface
interface Services {
  readonly database: Database
  readonly logger: Logger
  readonly emailService: EmailService
  readonly config: AppConfig
}

interface Database {
  readonly find: <T>(id: string) => Promise<T | null>
  readonly insert: <T>(record: T) => Promise<T>
}

interface Logger {
  readonly info: (message: string) => void
  readonly error: (message: string) => void
}

interface EmailService {
  readonly send: (to: string, subject: string, body: string) => Promise<void>
}

interface AppConfig {
  readonly emailEnabled: boolean
  readonly logLevel: 'debug' | 'info' | 'warn' | 'error'
}

// Domain types
interface User {
  readonly id: string
  readonly email: string
  readonly name: string
}

// Service functions using Reader
const getUser = (id: string): R.Reader<Services, Promise<User | null>> =>
  pipe(
    R.ask<Services>(),
    R.map(({ database }) => database.find<User>(id))
  )

const createUser = (userData: Omit<User, 'id'>): R.Reader<Services, Promise<User>> =>
  pipe(
    R.ask<Services>(),
    R.map(async ({ database, logger }) => {
      const user = { ...userData, id: generateId() }
      logger.info(`Creating user ${user.id}`)
      return database.insert(user)
    })
  )

const sendWelcomeEmail = (user: User): R.Reader<Services, Promise<void>> =>
  pipe(
    R.ask<Services>(),
    R.map(async ({ emailService, config, logger }) => {
      if (!config.emailEnabled) {
        logger.info('Email disabled, skipping welcome email')
        return
      }
      
      logger.info(`Sending welcome email to ${user.email}`)
      await emailService.send(
        user.email,
        'Welcome!',
        `Hello ${user.name}, welcome to our platform!`
      )
    })
  )

// Compose services
const registerUser = (userData: Omit<User, 'id'>): R.Reader<Services, Promise<void>> =>
  pipe(
    R.ask<Services>(),
    R.map(async (services) => {
      const user = await createUser(userData)(services)
      await sendWelcomeEmail(user)(services)
    })
  )

// Mock implementations for testing
const mockDatabase: Database = {
  find: async (id) => ({ id, email: 'test@example.com', name: 'Test' }),
  insert: async (record) => record
}

const mockLogger: Logger = {
  info: (message) => console.log('[INFO]', message),
  error: (message) => console.error('[ERROR]', message)
}

const mockEmailService: EmailService = {
  send: async (to, subject, body) => {
    console.log(`Email to ${to}: ${subject}`)
  }
}

// Production dependencies
const productionServices: Services = {
  database: new PostgresDatabase(),
  logger: new WinstonLogger(),
  emailService: new SendGridService(),
  config: {
    emailEnabled: true,
    logLevel: 'info'
  }
}

// Test dependencies
const testServices: Services = {
  database: mockDatabase,
  logger: mockLogger,
  emailService: mockEmailService,
  config: {
    emailEnabled: false, // Disable emails in tests
    logLevel: 'debug'
  }
}

// Usage in production
async function main() {
  const userData = {
    email: 'newuser@example.com',
    name: 'Alice'
  }
  
  await registerUser(userData)(productionServices)
}

// Usage in tests
async function testRegistration() {
  const userData = {
    email: 'test@example.com',
    name: 'Test User'
  }
  
  // Use test services
  await registerUser(userData)(testServices)
  
  // Verify behavior...
}

// Benefits:
// 1. Pure functions - easy to test
// 2. Explicit dependencies - no hidden globals
// 3. Swappable implementations - test/production
// 4. Type-safe - compiler verifies dependencies
// 5. Composable - easy to build complex services
```

---

## Example 4: Immutable State Management

### Scenario: Redux-Style State Management with fp-ts

```typescript
import * as O from 'fp-ts/Option'
import { pipe } from 'fp-ts/function'

// State type (all readonly)
interface AppState {
  readonly user: O.Option<User>
  readonly posts: ReadonlyArray<Post>
  readonly loading: boolean
  readonly error: O.Option<string>
}

interface User {
  readonly id: string
  readonly name: string
  readonly email: string
}

interface Post {
  readonly id: string
  readonly title: string
  readonly content: string
  readonly authorId: string
}

// Initial state
const initialState: AppState = {
  user: O.none,
  posts: [],
  loading: false,
  error: O.none
}

// Action types
type Action =
  | { readonly _tag: 'LOGIN'; readonly user: User }
  | { readonly _tag: 'LOGOUT' }
  | { readonly _tag: 'LOAD_POSTS' }
  | { readonly _tag: 'LOAD_POSTS_SUCCESS'; readonly posts: ReadonlyArray<Post> }
  | { readonly _tag: 'LOAD_POSTS_ERROR'; readonly error: string }
  | { readonly _tag: 'ADD_POST'; readonly post: Post }

// Reducer (pure function: state + action → new state)
const reducer = (state: AppState, action: Action): AppState => {
  switch (action._tag) {
    case 'LOGIN':
      return {
        ...state,
        user: O.some(action.user)
      }

    case 'LOGOUT':
      return {
        ...state,
        user: O.none,
        posts: []
      }

    case 'LOAD_POSTS':
      return {
        ...state,
        loading: true,
        error: O.none
      }

    case 'LOAD_POSTS_SUCCESS':
      return {
        ...state,
        posts: action.posts,
        loading: false
      }

    case 'LOAD_POSTS_ERROR':
      return {
        ...state,
        error: O.some(action.error),
        loading: false
      }

    case 'ADD_POST':
      return {
        ...state,
        posts: [...state.posts, action.post]
      }

    default:
      return state
  }
}

// Usage
let state = initialState

// Dispatch actions
state = reducer(state, {
  _tag: 'LOGIN',
  user: { id: '123', name: 'Alice', email: 'alice@example.com' }
})

state = reducer(state, {
  _tag: 'LOAD_POSTS_SUCCESS',
  posts: [
    { id: '1', title: 'First Post', content: 'Hello world', authorId: '123' }
  ]
})

// State is immutable
console.log(state.posts.length) // 1

// Each action creates a new state
const newState = reducer(state, {
  _tag: 'ADD_POST',
  post: { id: '2', title: 'Second Post', content: 'More content', authorId: '123' }
})

console.log(state.posts.length)      // 1 (original unchanged)
console.log(newState.posts.length)   // 2 (new state)

// Selectors (pure functions)
const getUserName = (state: AppState): O.Option<string> =>
  pipe(state.user, O.map(user => user.name))

const getPostCount = (state: AppState): number =>
  state.posts.length

const getPostsByAuthor = (state: AppState, authorId: string): ReadonlyArray<Post> =>
  state.posts.filter(post => post.authorId === authorId)

// Usage with React
import { useReducer } from 'react'

const useAppState = () => {
  const [state, dispatch] = useReducer(reducer, initialState)
  
  return {
    state,
    login: (user: User) => dispatch({ _tag: 'LOGIN', user }),
    logout: () => dispatch({ _tag: 'LOGOUT' }),
    loadPostsSuccess: (posts: ReadonlyArray<Post>) => 
      dispatch({ _tag: 'LOAD_POSTS_SUCCESS', posts })
  }
}

// Benefits:
// 1. Immutable state - predictable, no surprises
// 2. Pure reducer - easy to test, no side effects
// 3. Type-safe actions - compiler catches errors
// 4. Time-travel debugging - can replay actions
// 5. Easy to persist/rehydrate - state is just data
```

---

## Example 5: Higher-Order Functions and Generic Patterns

### Scenario: Validation Framework with Higher-Kinded Types

```typescript
import * as E from 'fp-ts/Either'
import { pipe } from 'fp-ts/function'

// Generic validator type
type Validator<E, A> = (value: A) => E.Either<E, A>

// Combine validators with AND logic (all must pass)
const and = <E, A>(...validators: Array<Validator<E, A>>): Validator<E, A> =>
  (value: A) => {
    for (const validator of validators) {
      const result = validator(value)
      if (E.isLeft(result)) {
        return result
      }
    }
    return E.right(value)
  }

// Combine validators with OR logic (at least one must pass)
const or = <E, A>(...validators: Array<Validator<E, A>>): Validator<E, A> =>
  (value: A) => {
    const errors: Array<E> = []
    
    for (const validator of validators) {
      const result = validator(value)
      if (E.isRight(result)) {
        return result // Return first success
      }
      errors.push(result.left)
    }
    
    return E.left(errors[0]) // Return first error
  }

// Specific validators
const minLength = (min: number): Validator<string, string> =>
  value => value.length >= min
    ? E.right(value)
    : E.left({ type: 'MinLengthError', min, actual: value.length })

const maxLength = (max: number): Validator<string, string> =>
  value => value.length <= max
    ? E.right(value)
    : E.left({ type: 'MaxLengthError', max, actual: value.length })

const matchesRegex = (regex: RegExp, description: string): Validator<string, string> =>
  value => regex.test(value)
    ? E.right(value)
    : E.left({ type: 'RegexError', description })

// Compose complex validators
const validateUsername = and(
  minLength(3),
  maxLength(20),
  matchesRegex(/^[a-zA-Z0-9_]+$/, 'only letters, numbers, and underscores')
)

const validateEmail = or(
  matchesRegex(/^[^\\s@]+@[^\\s@]+\\.[^\\s@]+$/, 'basic email format'),
  matchesRegex(/^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$/, 'standard email format')
)

// Usage
const username1 = validateUsername('alice')      // Right('alice')
const username2 = validateUsername('a')          // Left({ type: 'MinLengthError', ... })
const username3 = validateUsername('alice@123')  // Left({ type: 'RegexError', ... })

const email1 = validateEmail('test@example.com') // Right('test@example.com')
const email2 = validateEmail('invalid-email')    // Left({ type: 'RegexError', ... })

// Generic traverse function
const traverse = <E, A, B>(
  items: ReadonlyArray<A>,
  fn: (item: A) => E.Either<E, B>
): E.Either<E, ReadonlyArray<B>> => {
  const results: Array<B> = []
  
  for (const item of items) {
    const result = fn(item)
    if (E.isLeft(result)) {
      return result // Short-circuit on first error
    }
    results.push(result.right)
  }
  
  return E.right(results)
}

// Validate all items in a collection
const emails = ['test@example.com', 'invalid', 'user@domain.org']
const validatedEmails = traverse(emails, validateEmail)
// Left({ type: 'RegexError', ... }) - fails on second email

const goodEmails = ['test@example.com', 'user@domain.org']
const validatedGoodEmails = traverse(goodEmails, validateEmail)
// Right(['test@example.com', 'user@domain.org'])

// Benefits:
// 1. Reusable validators - compose complex rules from simple ones
// 2. Type-safe - errors are part of the type
// 3. Short-circuiting - stops at first error
// 4. Generic - works with any validation logic
// 5. Composable - and/or combinators for complex rules
```

---

## Example 6: Real-World API Endpoint with Effect

### Scenario: Complete REST API Endpoint with Error Handling, Database, and External Services

```typescript
import { Effect } from 'effect'
import { Schema } from '@effect/schema'

// Domain types
class User extends Schema.Class<User>('User')({
  id: Schema.string,
  email: Schema.string,
  name: Schema.string,
  createdAt: Schema.date
}) {}

class DatabaseError extends Schema.TaggedError<DatabaseError>()('DatabaseError', {
  message: Schema.string
}) {}

class ValidationError extends Schema.TaggedError<ValidationError>()('ValidationError', {
  message: Schema.string,
  field: Schema.string
}) {}

class EmailError extends Schema.TaggedError<EmailError>()('EmailError', {
  message: Schema.string
}) {}

// Services
interface Database {
  readonly findUser: (id: string) => Effect.Effect<User | null, DatabaseError>
  readonly insertUser: (user: Omit<User, 'id' | 'createdAt'>) => 
    Effect.Effect<User, DatabaseError>
  readonly isEmailTaken: (email: string) => Effect.Effect<boolean, DatabaseError>
}

interface EmailService {
  readonly sendWelcome: (user: User) => Effect.Effect<void, EmailError>
}

interface Logger {
  readonly info: (message: string) => Effect.Effect<void, never>
  readonly error: (message: string) => Effect.Effect<void, never>
}

// Dependencies
interface Services {
  readonly database: Database
  readonly emailService: EmailService
  readonly logger: Logger
}

// Validation
const validateEmail = (email: string): Effect.Effect<string, ValidationError> =>
  email.includes('@') && email.length > 3
    ? Effect.succeed(email)
    : Effect.fail(new ValidationError({ 
        message: 'Invalid email format', 
        field: 'email' 
      }))

const validatePassword = (password: string): Effect.Effect<string, ValidationError> =>
  password.length >= 8 && /[A-Z]/.test(password) && /[0-9]/.test(password)
    ? Effect.succeed(password)
    : Effect.fail(new ValidationError({ 
        message: 'Password must be 8+ chars with uppercase and number', 
        field: 'password' 
      }))

// API endpoint handler
const registerUserHandler = (
  request: { readonly email: string; readonly password: string; readonly name: string }
): Effect.Effect<User, DatabaseError | ValidationError | EmailError, Services> =>
  Effect.gen(function* (_) {
    const { database, emailService, logger } = yield* _(Effect.context<Services>())
    
    // Validate input
    const email = yield* _(validateEmail(request.email))
    const password = yield* _(validatePassword(request.password))
    
    // Check if email is taken
    const isTaken = yield* _(database.isEmailTaken(email))
    if (isTaken) {
      yield* _(logger.error(`Email already taken: ${email}`))
      yield* _(Effect.fail(new ValidationError({ 
        message: 'Email already registered', 
        field: 'email' 
      })))
    }
    
    // Create user
    const user = yield* _(database.insertUser({
      email,
      password, // In real app, hash the password!
      name: request.name
    }))
    
    // Send welcome email
    yield* _(logger.info(`User created: ${user.id}`))
    yield* _(emailService.sendWelcome(user))
    
    return user
  })

// Mock implementations for testing
const mockDatabase: Database = {
  findUser: (id) => Effect.succeed({ 
    id, 
    email: 'test@example.com', 
    name: 'Test', 
    createdAt: new Date() 
  }),
  insertUser: (userData) => Effect.succeed({
    ...userData,
    id: 'user-123',
    createdAt: new Date()
  }),
  isEmailTaken: (email) => Effect.succeed(email === 'test@example.com')
}

const mockEmailService: EmailService = {
  sendWelcome: (user) => Effect.succeed(undefined).pipe(
    Effect.tap(() => Effect.log(`Welcome email sent to ${user.email}`))
  )
}

const mockLogger: Logger = {
  info: (message) => Effect.succeed(undefined).pipe(
    Effect.tap(() => console.log('[INFO]', message))
  ),
  error: (message) => Effect.succeed(undefined).pipe(
    Effect.tap(() => console.error('[ERROR]', message))
  )
}

// Test the handler
async function testRegistration() {
  const services = { database: mockDatabase, emailService: mockEmailService, logger: mockLogger }
  
  // Test successful registration
  const result1 = await Effect.runPromise(
    registerUserHandler({
      email: 'new@example.com',
      password: 'StrongPass123',
      name: 'Alice'
    }).pipe(
      Effect.provideService(services)
    )
  )
  console.log('Success:', result1)
  
  // Test validation error
  const result2 = await Effect.runPromiseExit(
    registerUserHandler({
      email: 'invalid-email',
      password: 'weak',
      name: 'Bob'
    }).pipe(
      Effect.provideService(services)
    )
  )
  console.log('Validation error:', result2)
}

// Production usage
const program = registerUserHandler({
  email: 'user@example.com',
  password: 'SecurePass123!',
  name: 'Charlie'
})

Effect.runPromise(program.pipe(
  Effect.provideService(mockDatabase),
  Effect.provideService(mockEmailService),
  Effect.provideService(mockLogger)
)).then(
  user => console.log('User registered:', user),
  error => console.error('Registration failed:', error)
)

// Benefits:
// 1. Type-safe errors - all error cases known at compile time
// 2. Dependency injection - explicit, testable
// 3. Async/await syntax - readable and familiar
// 4. Automatic error handling - no try/catch needed
// 5. Composable - easy to build complex workflows
// 6. Testable - easy to mock dependencies
// 7. Logging built-in - structured logging
// 8. Schema validation - runtime type safety
```

---

## Further Reading

- **Core Principles**: `code_guidelines/principles/functional_programming.md`
- **TypeScript Patterns**: `code_guidelines/languages/typescript/patterns.md`
- **Library Guide**: `code_guidelines/languages/typescript/libraries.md`

---

**Last Updated**: 2025-11-19  
**Maintained By**: Global AI Rules System  
**Status**: Active  
**Next**: See `patterns.md` for advanced patterns and `libraries.md` for library-specific guidance