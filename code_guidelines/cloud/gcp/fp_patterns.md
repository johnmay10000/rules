# GCP Functional Programming Patterns

**Version**: 1.0.0
**Part of**: [code_guidelines/cloud/index.json](../index.json)

> **📖 Universal Rules**: This document outlines Functional Programming patterns for Google Cloud Platform services (Cloud Functions, Cloud Run, Firestore, Pub/Sub).

---

## 1. Cloud Functions with Railway Pattern

### ❌ Avoid: Global State and Exceptions

```typescript
// BAD: Global state, exception-based errors
global.docRef = null

export const processUpload = functions.storage.object().onFinalize(async (object) => {
  try {
    if (!global.docRef) {
      global.docRef = admin.firestore().collection('files').doc()
    }
    await global.docRef.set({ name: object.name })
    return { success: true }
  } catch (error) {
    throw new Error('Processing failed') // ❌ Opaque error
  }
})
```

### ✅ Prefer: Pure Handlers with Either

```typescript
import * as functions from 'firebase-functions'
import * as TE from 'fp-ts/TaskEither'
import { pipe } from 'fp-ts/function'

const processUpload = functions.storage.object().onFinalize(async (event) =>
  pipe(
    TE.Do,
    TE.bind('validatedEvent', () => validateStorageEvent(event)),
    TE.bind('metadata', ({ validatedEvent }) => extractMetadata(validatedEvent)),
    TE.chainW(({ metadata }) => storeInFirestore(metadata)),
    TE.mapLeft(mapToFunctionsError) // Railway error channel
  )()
)
```

### Generic Handler Factory

```typescript
// Reusable pure function factory for Cloud Functions
export const createEventFunction = <TInput, TOutput, TError>({
  validator,
  processor,
  errorMapper,
}: {
  validator: (event: unknown) => TE.TaskEither<TError, TInput>
  processor: (input: TInput) => TE.TaskEither<TError, TOutput>
  errorMapper: (error: TError) => functions.https.FunctionsError
}) => {
  return async (event: unknown) =>
    pipe(
      TE.Do,
      TE.bind('input', () => validator(event)),
      TE.bind('result', ({ input }) => processor(input)),
      TE.getOrElse((error) => { throw errorMapper(error) })
    )()
}
```

---

## 2. Cloud Run with Railway-Oriented API

### Type-Safe Express Handler

```typescript
const createAuthedHandler = <TInput, TSuccess>(
  handler: (req: AuthenticatedRequest, input: TInput) => TE.TaskEither<ApiError, TSuccess>
) => {
  return (req: AuthenticatedRequest, res: express.Response) =>
    pipe(
      TE.Do,
      TE.bind('user', () => authenticateRequest(req)),
      TE.bind('input', () => validateRequestBody(req.body)),
      TE.bind('result', ({ input }) => handler(req, input)),
      TE.match(
        (error) => res.status(mapErrorToStatus(error)).json(error),
        (result) => res.status(200).json(result)
      )
    )()
}
```

---

## 3. Firestore with Event Sourcing

### Immutable Document Versions

```typescript
// Treat each document write as an immutable version
type UserProfileEvent =
  | { type: 'ProfileCreated'; data: UserData; timestamp: Timestamp }
  | { type: 'EmailUpdated'; data: { newEmail: string }; timestamp: Timestamp }

const appendEvent = (userId: string, event: UserProfileEvent) =>
  pipe(
    TE.tryCatch(
      () => admin.firestore().runTransaction(async (transaction) => {
        // Read current, append event, increment version
      }),
      (err): FirestoreError => ({ _tag: 'FirestoreError', details: err })
    )
  )
```

---

## 4. Pub/Sub with Railway Pattern

### Type-Safe Event Publishing

```typescript
const publishEvent = (pubsub: PubSub, topicName: string, event: DomainEvent) =>
  pipe(
    TE.Do,
    TE.bind('validatedEvent', () => TE.fromEither(validateEvent(event))),
    TE.chainW(({ validatedEvent }) =>
      TE.tryCatch(
        async () => pubsub.topic(topicName).publish(Buffer.from(JSON.stringify(validatedEvent))),
        (err): PublishError => ({ _tag: 'PublisherError', details: err })
      )
    )
  )
```

### Parallel Event Processing

```typescript
// Process events in parallel with Railway
const processBatch = (pubsub: PubSub, subscriptionName: string, batchSize: number) =>
  pipe(
    pullMessages(subscription, batchSize),
    TE.chainW((batch) =>
      pipe(
        batch,
        A.traverse(TE.taskEither)((message) =>
          pipe(
            handleEvent(message),
            TE.chain(() => TE.tryCatch(() => message.ack(), toAckError))
          )
        )
      )
    )
  )
```

---

## 5. Eventarc with Railway Flow Control

### Event Router with Pattern Matching

```typescript
const eventRouter = (event: EventarcEvent) =>
  pipe(
    TE.fromEither(validateEventarcEvent(event)),
    TE.chain((validatedEvent) => {
      switch (validatedEvent.type) {
        case 'google.cloud.storage.object.v1.finalized':
          return TE.right(handleStorageEvent(validatedEvent))
        default:
          return TE.left({ _tag: 'UnknownEvent', event })
      }
    })
  )
```

---

## 6. Testing GCP Patterns

### Firebase Emulators

```typescript
// Test against local Firebase emulators
describe('Firestore event sourcing', () => {
  beforeAll(async () => {
    await firebase.initializeAdminApp({ projectId: 'test-project' })
  })

  it('should maintain event ordering', async () => {
    // Append events and verify projection
  })
})
```

### Property-Based Testing

```typescript
import * as fc from 'fast-check'

describe('Pub/Sub event processing', () => {
  it('should maintain event order and exactly-once semantics', () =>
    fc.assert(
      fc.asyncProperty(fc.array(fc.string()), async (messages) => {
        // Verify exactly-once processing
      })
    )
  )
})
```
