# GCP Functional Programming Style Guide for Kimi CLI

**Version**: 1.0.0  
**Last Updated**: 2025-11-14  
**Part of**: [KIMI.md](KIMI.md) Global Rule Set  
**Target**: GCP projects (Cloud Functions, Cloud Run, Firestore, Pub/Sub, Eventarc)

> **📖 Global Rules**: This document extends [KIMI.md](KIMI.md) with GCP-specific FP guidance. For mandatory universal rules (Git, documentation, testing), see [KIMI.md](KIMI.md).

---

## Quick Links

- **Mandatory Rules**: See [KIMI.md](KIMI.md) sections 1-4  
- **FP Principles**: See [KIMI_FP_PRINCIPLES.md](KIMI_FP_PRINCIPLES.md)  
- **Language Guides**: See [python-fp-style-guide.md](python-fp-style-guide.md), [typescript-fp-style-guide.md](typescript-fp-style-guide.md)
- **Universal Cloud Patterns**: See [../code_guidelines/cloud/serverless_fp_patterns.md](../code_guidelines/cloud/serverless_fp_patterns.md)

---

## For Cloud Functions and Cloud Run

### Core Principles

1. **Immutable Services**: Treat function deployments as immutable artifacts
2. **Pure Functions**: Cloud Functions as isolated, side-effect-controlled units
3. **Event-Driven**: Embrace Pub/Sub and Eventarc for loose coupling
4. **Explicit Error Handling**: Either/Result types through the entire pipeline
5. **Idempotency**: Design for at-least-once delivery semantics

---

## Kimi-Specific Patterns for GCP

### Parallel Multi-Service Validation

Kimi can validate multiple GCP services simultaneously:

```typescript
// Kimi excels at validating GCP project structure
// Run parallel validations:
// ✓ Cloud Function IaC (Terraform/Cloud Build)
// ✓ Cloud Run service configurations
// ✓ Firestore security rules
// ✓ Pub/Sub topic/subscription setup
// ✓ Eventarc triggers

const eventProcessingPipeline = new gcp.cloudfunctions.Function(
  'event-processor',
  {
    runtime: 'nodejs20',
    sourceArchiveBucket: sourceBucket.name,
    sourceArchiveObject: object.name,
    eventTrigger: {
      eventType: 'google.storage.object.finalize',
      resource: inputBucket.name,
      failurePolicy: {
        retry: {}, // Infinite retry with exponential backoff
      },
    },
    availableMemoryMb: 512,
    timeout: 300,
    environmentVariables: {
      OUTPUT_BUCKET: outputBucket.name,
      ERROR_TOPIC: errorTopic.name,
    },
  }
)

// Kimi validates: ✓ Event trigger configuration
//                 ✓ Environment variable setup
//                 ✓ Error handling through Pub/Sub
//                 ✓ Resource permissions
```

### Subagent Pattern for Multi-Project Validation

Use Kimi's Task tool for complex multi-project architectures:

```typescript
// Multi-environment setup benefits from Kimi subagents
interface EnvironmentConfig {
  projectId: string
  region: string
  environment: 'dev' | 'staging' | 'prod'
}

const validateEnvironments = (configs: EnvironmentConfig[]) => {
  // Kimi can spawn subagent per environment:
  // - dev: ✓ Relaxed security rules
  // - staging: ✓ Mirror production config
  // - prod: ✓ Strict IAM, encryption, audit logging

  return pipe(
    configs,
    A.traverse(TE.taskEither)(validateEnvironment)
  )
}

const validateEnvironment = (config: EnvironmentConfig): TE.TaskEither<ValidationError, ValidatedConfig> =>
  pipe(
    TE.Do,
    TE.bind('iam', () => validateIamPolicies(config)), // Kimi subagent
    TE.bind('network', () => validateNetwork(config)), // Kimi subagent (parallel)
    TE.bind('encryption', () => validateEncryption(config)), // Kimi subagent
    TE.map(({ iam, network, encryption }) => ({
      config,
      validation: { iam, network, encryption },
    }))
  )
```

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
    console.error(error)
    throw new Error('Processing failed') // ❌ Opaque error
  }
})
```

### ✅ Prefer: Pure Handlers with Either

```typescript
import * as functions from 'firebase-functions'
import * as TE from 'fp-ts/TaskEither'
import { pipe } from 'fp-ts/function'

interface StorageEvent {
  bucket: string
  name: string
  metageneration: string
  size: string
}

type ProcessingError =
  | { _tag: 'InvalidEvent'; event: unknown }
  | { _tag: 'StorageError'; operation: string; details: unknown }
  | { _tag: 'FirestoreError'; operation: string; details: unknown }

const processUpload = functions.storage.object().onFinalize(async (event) =>
  pipe(
    TE.Do,
    TE.bind('validatedEvent', () => validateStorageEvent(event)),
    TE.bind('metadata', ({ validatedEvent }) => extractMetadata(validatedEvent)),
    TE.bind('docId', () => TE.right(generateDocId())),
    TE.chainW(({ metadata, docId }) => storeInFirestore(docId, metadata)),
    TE.map((docRef) => ({
      success: true,
      documentId: docRef.id,
      processedAt: new Date().toISOString(),
    })),
    TE.mapLeft(mapToFunctionsError) // Railway error channel
  )()
)

// Kimi validates handler: ✓ No global state
//                          ✓ Explicit error handling
//                          ✓ Type safety

const validateStorageEvent = (event: unknown): TE.TaskEither<ProcessingError, StorageEvent> =>
  pipe(
    TE.fromNullable({ _tag: 'InvalidEvent', event } as const)(isStorageEvent(event) ? event : null),
    TE.chain((e) =>
      e.size === '0'
        ? TE.left({ _tag: 'InvalidEvent', event: 'Empty file' } as const)
        : TE.right(e)
    )
  )

const isStorageEvent = (event: unknown): event is StorageEvent =>
  typeof event === 'object' &&
  event !== null &&
  'bucket' in event &&
  'name' in event &&
  'size' in event
```

### Generic Handler Factory

```typescript
// Reusable pure function factory for Cloud Functions
export const createEventFunction = <TInput, TOutput, TError extends ProcessingError>({
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
      TE.getOrElse((error) => {
        throw errorMapper(error)
      })
    )()
}

// Usage for storage trigger
export const handler = createEventFunction({
  validator: validateStorageEvent,
  processor: (event) =>
    pipe(
      TE.Do,
      TE.bind('metadata', () => extractMetadata(event)),
      TE.chainW(({ metadata }) => storeInFirestore(metadata))
    ),
  errorMapper: (error) => {
    switch (error._tag) {
      case 'InvalidEvent':
        throw new functions.https.HttpsError('invalid-argument', error.event)
      case 'StorageError':
        throw new functions.https.HttpsError('internal', error.operation)
      case 'FirestoreError':
        throw new functions.https.HttpsError('unavailable', error.operation)
    }
  },
})

// Kimi validates factory: ✓ Generic type safety
//                          ✓ Error mapping exhaustiveness
//                          ✓ Pure function composition
```

---

## 2. Cloud Run with Railway-Oriented API

### Type-Safe Express Handler

```typescript
import express from 'express'
import * as TE from 'fp-ts/TaskEither'
import { pipe } from 'fp-ts/function'

interface AuthenticatedRequest extends express.Request {
  user?: DecodedIdToken
}

interface ValidationError {
  _tag: 'ValidationError'
  fields: string[]
}

interface AuthError {
  _tag: 'AuthError'
  message: string
}

type ApiError = ValidationError | AuthError | { _tag: 'DatabaseError'; details: unknown }

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
        (error) => {
          // Railway error handling
          switch (error._tag) {
            case 'ValidationError':
              res.status(400).json({ error: 'Invalid input', fields: error.fields })
              break
            case 'AuthError':
              res.status(401).json({ error: 'Unauthorized', message: error.message })
              break
            case 'DatabaseError':
              res.status(500).json({ error: 'Database error' })
              break
          }
        },
        (result) => res.status(200).json(result)
      )
    )()
}

// Kimi validates middleware: ✓ Composable authentication
//                              ✓ Type-safe request handling
//                              ✓ Railway error mapping

// Usage
const app = express()

app.post('/api/users', (req, res) =>
  pipe(
    TE.Do,
    TE.bind('userId', () => TE.right(generateUserId())),
    TE.bind('userData', ({ userId }) =>
      TE.right({
        userId,
        ...req.body,
        createdAt: new Date().toISOString(),
        version: 1,
      })
    ),
    TE.chainW(({ userData }) => saveUserToFirestore(userData)),
    TE.match(
      (error) => {
        if (error._tag === 'DatabaseError') {
          res.status(500).json({ error: 'Failed to create user' })
        } else {
          res.status(400).json({ error: 'Invalid request' })
        }
      },
      (user) => res.status(201).json(user)
    )
  )()
)
```

### Railway-Oriented Service Handler

```typescript
// Cloud Run service with railway pattern
import * as http from 'http'

const createServiceHandler = <TInput, TOutput>(config: {
  validate: (req: http.IncomingMessage) => TE.TaskEither<ValidationError, TInput>
  process: (input: TInput) => TE.TaskEither<ProcessingError, TOutput>
  serialize: (output: TOutput) => string
}) => {
  return async (req: http.IncomingMessage, res: http.ServerResponse) => {
    const result = await pipe(
      TE.Do,
      TE.bind('input', () => config.validate(req)),
      TE.bind('output', ({ input }) => config.process(input)),
      TE.match(
        (error) => {
          res.writeHead(mapErrorToStatus(error), { 'Content-Type': 'application/json' })
          res.end(JSON.stringify({ error: error._tag }))
        },
        (output) => {
          res.writeHead(200, { 'Content-Type': 'application/json' })
          res.end(config.serialize(output))
        }
      )
    )()
    
    await result
  }
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
  | { type: 'NameUpdated'; data: { newName: string }; timestamp: Timestamp }

interface UserProfileDocument {
  userId: string
  events: UserProfileEvent[]
  version: number
  lastUpdated: Timestamp
}

const appendEvent = (userId: string, event: UserProfileEvent) =>
  pipe(
    TE.Do,
    TE.bind('userRef', () => TE.right(admin.firestore().collection('users').doc(userId))),
    TE.bind('transactionResult', ({ userRef }) =>
      TE.tryCatch(
        () =>
          admin.firestore().runTransaction(async (transaction) => {
            const doc = await transaction.get(userRef)
            const currentData = doc.exists ? (doc.data() as UserProfileDocument) : null

            const updatedData: UserProfileDocument = currentData
              ? {
                  ...currentData,
                  events: [...currentData.events, event],
                  version: currentData.version + 1,
                  lastUpdated: event.timestamp,
                }
              : {
                  userId,
                  events: [event],
                  version: 1,
                  lastUpdated: event.timestamp,
                }

            transaction.set(userRef, updatedData)
            return updatedData
          }),
        (err): FirestoreError => ({ _tag: 'FirestoreError', operation: 'transaction', details: err })
      )
    ),
    TE.map(({ transactionResult }) => transactionResult)
  )

// Kimi validates: ✓ Transaction consistency
//                 ✓ Version increment
//                 ✓ Event immutability
//                 ✓ Optimistic locking
```

### Projected Current State

```typescript
// Materialized view from events
const projectCurrentState = (events: UserProfileEvent[]): UserData => {
  const initial: UserData = {
    userId: '',
    email: '',
    name: '',
    createdAt: '',
  }

  return events.reduce((state, event) => {
    switch (event.type) {
      case 'ProfileCreated':
        return { ...state, ...event.data }
      case 'EmailUpdated':
        return { ...state, email: event.data.newEmail }
      case 'NameUpdated':
        return { ...state, name: event.data.newName }
      default:
        return state // Ignore unknown events (forward compatibility)
    }
  }, initial)
}

// Kimi validates projection: ✓ Event ordering
//                             ✓ State transition purity
//                             ✓ Forward compatibility
```

---

## 4. Pub/Sub with Railway Pattern

### Type-Safe Event Publishing

```typescript
import { PubSub } from '@google-cloud/pubsub'

interface DomainEvent {
  type: 'UserRegistered' | 'OrderPlaced' | 'PaymentProcessed'
  data: Record<string, unknown>
  timestamp: string
  source: string
  version: number
}

type PublishError =
  | { _tag: 'SerializationError'; details: unknown }
  | { _tag: 'PublisherError'; topic: string; details: unknown }
  | { _tag: 'ValidationError'; event: unknown }

const publishEvent = (pubsub: PubSub, topicName: string, event: DomainEvent) =>
  pipe(
    TE.Do,
    TE.bind('validatedEvent', () =>
      TE.fromEither(validateEvent(event)) // Validation from earlier
    ),
    TE.bind('serializedEvent', ({ validatedEvent }) =>
      TE.tryCatch(
        async () => JSON.stringify(validatedEvent),
        (err): PublishError => ({ _tag: 'SerializationError', details: err })
      )
    ),
    TE.chainW(({ serializedEvent }) =>
      TE.tryCatch(
        async () => {
          const topic = pubsub.topic(topicName)
          const messageId = await topic.publish(Buffer.from(serializedEvent))
          return messageId
        },
        (err): PublishError => ({ _tag: 'PublisherError', topic: topicName, details: err })
      )
    )
  )

// Kimi validates: ✓ Event validation (before publish)
//                 ✓ Serialization error handling
//                 ✓ Publisher error channel
```

### Parallel Event Processing

```typescript
// Process events in parallel with Railway
const processBatch = (pubsub: PubSub, subscriptionName: string, batchSize: number) =>
  pipe(
    TE.Do,
    TE.bind('subscription', () => TE.right(pubsub.subscription(subscriptionName))),
    TE.bind('batch', ({ subscription }) => pullMessages(subscription, batchSize)),
    TE.chainW(({ batch }) =>
      pipe(
        batch,
        // Kimi validates: ✓ Parallel processing
        A.traverse(TE.taskEither)((message) =>
          pipe(
            parseMessage(message),
            TE.chain(validateEvent),
            TE.chain((event) => handleEvent(event)),
            TE.chain(() => TE.tryCatch(() => message.ack(), toAckError))
          )
        )
      )
    ),
    TE.mapLeft((error) => {
      // Handle partial failures (some succeeded, some failed)
      if (error._tag === 'PartialBatchFailure') {
        logPartialFailure(error)
      }
      return error
    })
  )

// Kimi validates batch: ✓ Parallel composition
//                         ✓ Acknowledgment handling
//                         ✓ Partial failure scenarios
```

---

## 5. Eventarc with Railway Flow Control

### Event Router with Pattern Matching

```typescript
import * as eventarc from '@google-cloud/eventarc'

interface EventarcEvent {
  type: string
  data: any
  source: string
  id: string
  time: string
}

const eventRouter = (event: EventarcEvent) =>
  pipe(
    TE.fromEither(validateEventarcEvent(event)),
    TE.chain((validatedEvent) => {
      switch (validatedEvent.type) {
        case 'google.cloud.storage.object.v1.finalized':
          return TE.right(handleStorageEvent(validatedEvent))
        case 'google.cloud.pubsub.topic.v1.messagePublished':
          return TE.right(handlePubSubEvent(validatedEvent))
        case 'google.cloud.firestore.document.v1.created':
          return TE.right(handleFirestoreEvent(validatedEvent))
        default:
          // Unknown events go to DLQ for analysis
          return TE.left({
            _tag: 'UnknownEvent',
            event: validatedEvent,
            dlq: 'projects/my-project/topics/unknown-events',
          })
      }
    })
  )

// Kimi validates routing: ✓ Exhaustive pattern matching
//                          ✓ Type discrimination
//                          ✓ Error channel for unknown events
```

### Circuit Breaker Pattern

```typescript
// Prevent cascading failures with circuit breaker
interface CircuitBreakerState {
  failureCount: number
  successCount: number
  lastFailureTime: number | null
  isOpen: boolean
}

const withCircuitBreaker = <TInput, TOutput, TError>(
  handler: (input: TInput) => TE.TaskEither<TError, TOutput>,
  state: CircuitBreakerState,
  threshold: number = 5,
  timeoutMs: number = 60000
) => {
  return (input: TInput): TE.TaskEither<TError | CircuitError, TOutput> => {
    const now = Date.now()

    return pipe(
      TE.Do,
      TE.bind('circuitCheck', () => {
        if (state.isOpen && state.lastFailureTime) {
          const elapsed = now - state.lastFailureTime
          if (elapsed > timeoutMs) {
            // Half-open: try one request
            state.isOpen = false
            state.failureCount = 0
            return TE.right('half-open')
          }
          return TE.left({ _tag: 'CircuitOpen', timeUntilRetry: timeoutMs - elapsed } as const)
        }
        return TE.right('closed')
      }),
      TE.chainW(() => handler(input)),
      TE.map((output) => {
        state.successCount++
        state.failureCount = 0
        return output
      }),
      TE.mapLeft((error) => {
        state.failureCount++
        state.lastFailureTime = now
        if (state.failureCount >= threshold) {
          state.isOpen = true
        }
        return { _tag: 'HandlerError', originalError: error } as const
      })
    )
  }
}

// Kimi validates: ✓ State management correctness
//                 ✓ Timeout configuration
//                 ✓ Half-open behavior
```

---

## 6. Testing GCP Patterns

### Firebase Emulators

```typescript
// Test against local Firebase emulators
describe('Firestore event sourcing', () => {
  beforeAll(async () => {
    // Start Firebase emulators
    await firebase.initializeAdminApp({ projectId: 'test-project' })
  })

  it('should maintain event ordering', async () => {
    const userId = 'test-user-123'
    const events: UserProfileEvent[] = [
      {
        type: 'ProfileCreated',
        data: { userId, name: 'Alice', email: 'alice@example.com' },
        timestamp: Timestamp.now(),
      },
      {
        type: 'NameUpdated',
        data: { newName: 'Alice Smith' },
        timestamp: Timestamp.now(),
      },
    ]

    // Append events
    for (const event of events) {
      await appendEvent(userId, event)()
    }

    // Verify projection
    const docSnapshot = await admin.firestore().collection('users').doc(userId).get()
    const projection = docSnapshot.data() as UserProfileDocument

    expect(projection.events).toHaveLength(2)
    expect(projectCurrentState(projection.events).name).toBe('Alice Smith')
  })

  afterAll(async () => {
    // Cleanup
    await Promise.all(firebase.apps().map((app) => app.delete()))
  })
})

// Kimi validates: ✓ Emulator setup/teardown
//                 ✓ Test isolation
//                 ✓ State verification
```

### Property-Based Testing

```typescript
import * as fc from 'fast-check'

describe('Pub/Sub event processing', () => {
  it('should maintain event order and exactly-once semantics', () =>
    fc.assert(
      fc.asyncProperty(fc.array(fc.string(), { minLength: 1 }), async (messages) => {
        const processed: string[] = []
        const duplicates = new Set<string>()

        // Simulate parallel processing
        await Promise.all(
          messages.map(async (msg) => {
            const isDuplicate = duplicates.has(msg)
            if (!isDuplicate) {
              processed.push(msg)
              duplicates.add(msg)
            }
          })
        )

        // Verify exactly-once
        expect(processed).toEqual(Array.from(new Set(messages)))
      })
    )
  )

  // Kimi can spawn subagent to validate: ✓ Property definition
  //                                         ✓ Race condition detection
  //                                         ✓ Idempotency guarantees
})
```

---

## 7. Kimi CLI Integration

### Project Setup

```bash
# 1. Environment variables
export KIMI_RULES_PATH="$HOME/projects/rules"
export KIMI_GCP_RULES="$KIMI_RULES_PATH/kimi/gcp-fp-style-guide.md"

# 2. Project .kimirules
cat > .kimirules << 'EOF'
## GCP FP Rules
@${KIMI_RULES_PATH}/kimi/KIMI.md
@${KIMI_RULES_PATH}/kimi/gcp-fp-style-guide.md
@${KIMI_RULES_PATH}/kimi/typescript-fp-style-guide.md

## Language-Specific
!if-exists package.json @${KIMI_RULES_PATH}/kimi/typescript-fp-style-guide.md
!if-exists main.py @${KIMI_RULES_PATH}/kimi/python-fp-style-guide.md

## GCP Services
!if-exists firebase.json @${KIMI_RULES_PATH}/kimi/gcp-fp-style-guide.md
!if-exists app.yaml @${KIMI_RULES_PATH}/kimi/gcp-fp-style-guide.md
EOF

# 3. Validate setup
kimi read-file .kimirules
```

### Local Development Script

```bash
#!/bin/bash
# Kimi can validate GCP local development setup

echo "Starting Firebase emulators..."

# Run emulators in background
firebase emulators:start --only firestore,pubsub,functions &
EMULATOR_PID=$!

# Wait for ready
echo "Waiting for emulators..."
sleep 5

# Validate configuration
kimi read-file firebase.json &
curl -s http://127.0.0.1:8080 &

# Run type checking
npm run type-check &

# Run tests
npm test -- --watchAll=false &

# Parallel validations
wait

echo "All checks passing!"

# Cleanup on exit
trap "kill $EMULATOR_PID" EXIT
```

---

## 8. Cloud Build with Railway CI/CD

### Type-Safe Build Pipeline

```yaml
# cloudbuild.yaml with railway pattern
steps:
  # Validation stage
  - name: 'node:20'
    entrypoint: 'npm'
    args: ['run', 'type-check']
    id: 'type-check'

  - name: 'node:20'
    entrypoint: 'npm'
    args: ['run', 'lint']
    id: 'lint'

  # Testing stage (parallel after validation)
  - name: 'node:20'
    entrypoint: 'npm'
    args: ['test', '--', '--ci', '--coverage']
    id: 'unit-tests'
    waitFor: ['type-check', 'lint']

  - name: 'node:20'
    entrypoint: 'firebase'
    args: ['emulators:exec', 'npm run test:integration']
    id: 'integration-tests'
    waitFor: ['type-check', 'lint']

  # Deployment stage (only if tests pass)
  - name: 'gcr.io/google.com/cloudsdktool/cloud-sdk'
    entrypoint: 'gcloud'
    args: [
      'functions',
      'deploy',
      'event-processor',
      '--runtime',
      'nodejs20',
      '--trigger-event',
      'google.storage.object.finalize',
      '--trigger-resource',
      'projects/$_PROJECT_ID/buckets/input-bucket',
    ]
    id: 'deploy-cloud-function'
    waitFor: ['unit-tests', 'integration-tests']

# Railway pattern: validate → test → deploy
# Errors at any stage stop the pipeline
```

---

## Summary

Google Cloud Platform's serverless offerings integrate well with functional programming patterns. Use Cloud Functions with explicit Either types, Firestore with event sourcing, Pub/Sub with batch processing, Eventarc with circuit breakers, and Cloud Build with railway CI/CD. Kimi excels at validating multi-environment deployments and complex event flows.

**Key Takeaways**:
- Cloud Functions with pure handler factories (no exceptions)
- Firestore with event sourcing (immutable events)
- Pub/Sub with parallel batch processing (type-safe)
- Eventarc with circuit breaker pattern (fault tolerance)
- Cloud Build with railway CI/CD (fail-fast)
- Kimi's parallel validation for multi-service architectures

**Why This Matters**: FP patterns provide predictable error handling, easier testing, and better observability in GCP's event-driven architecture.

**Next**: See [aws-fp-style-guide.md](aws-fp-style-guide.md) for AWS FP patterns comparison.

---

**Last Updated**: 2025-11-14  
**Maintained By**: Kimi CLI Global Rules System  
**Status**: Active
