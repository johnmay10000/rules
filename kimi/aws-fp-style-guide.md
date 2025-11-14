# AWS Functional Programming Style Guide for Kimi CLI

**Version**: 1.0.0  
**Last Updated**: 2025-11-14  
**Part of**: [KIMI.md](KIMI.md) Global Rule Set  
**Target**: AWS projects (Lambda, Step Functions, DynamoDB, EventBridge, S3)

> **📖 Global Rules**: This document extends [KIMI.md](KIMI.md) with AWS-specific FP guidance. For mandatory universal rules (Git, documentation, testing), see [KIMI.md](KIMI.md).

---

## Quick Links

- **Mandatory Rules**: See [KIMI.md](KIMI.md) sections 1-4  
- **FP Principles**: See [KIMI_FP_PRINCIPLES.md](KIMI_FP_PRINCIPLES.md)  
- **Language Guides**: See [python-fp-style-guide.md](python-fp-style-guide.md), [typescript-fp-style-guide.md](typescript-fp-style-guide.md)

---

## For Serverless and Event-Driven Architectures

### Core Principles

1. **Immutable Infrastructure**: Infrastructure as Code (IaC) with immutable updates
2. **Pure Functions**: Lambda handlers as pure functions with explicit inputs/outputs
3. **Event Sourcing**: Treat events as immutable facts
4. **Explicit Error Handling**: Result/Either types instead of exceptions
5. **Composability**: Build workflows from small, reusable Lambda functions

---

## Kimi-Specific Patterns for AWS

### Parallel Deployment Validation

Kimi can validate multiple AWS resources simultaneously before deployment:

```typescript
// Kimi excels at validating AWS SAM/CloudFormation templates
// Run parallel validations:
// ✓ Lambda handler type safety
// ✓ IAM policy correctness
// ✓ Event source mappings
// ✓ Resource naming conventions

const userProcessingStateMachine = new sfn.StateMachine(
  this,
  'UserProcessingStateMachine',
  {
    definitionBody: sfn.DefinitionBody.fromChainable(
      sfn.Chain.start(
        new tasks.LambdaInvoke(this, 'ValidateUser', {
          lambdaFunction: validateUserLambda,
          // Kimi validates: ✓ Lambda handler exists
          //                 ✓ Input payload matches schema
          //                 ✓ ResultPath configuration
        })
      )
        .next(
          new tasks.LambdaInvoke(this, 'EnrichUserData', {
            lambdaFunction: enrichUserLambda,
            // Kimi validates: ✓ Parallel composition
            //                 ✓ Error handling strategy
          })
        )
        .next(
          new tasks.LambdaInvoke(this, 'SaveToDynamoDB', {
            lambdaFunction: saveUserLambda,
            // Kimi validates: ✓ Result type from previous step
            //                 ✓ DynamoDB table permissions
          })
        )
    ),
  }
)
```

### Subagent Pattern for Multi-Region Validation

Use Kimi's Task tool to spawn subagents for validating multi-region deployments:

```typescript
// Complex multi-region deployment benefits from Kimi subagents
// Spawn subagent per region:
// - us-east-1: ✓ Validate Lambda versions
// - eu-west-1: ✓ Validate DynamoDB Global Tables
// - ap-southeast-2: ✓ Validate S3 cross-region replication

interface RegionalDeployment {
  region: string
  lambdaVersion: string
  tableName: string
  bucketName: string
}

const deployToRegions = (regions: RegionalDeployment[]) => {
  // Kimi can spawn subagent for each region in parallel
  regions.forEach((region) => {
    // Subagent validates: ✓ Service availability
    //                      ✓ IAM permissions
    //                      ✓ Resource quotas
    //                      ✓ Cost limits
  })
}
```

---

## 1. Lambda Handler Patterns

### ❌ Avoid: Exception-Based Handlers

```typescript
// BAD: Exception-based error handling
default async function lambdaHandler(event: APIGatewayEvent) {
  try {
    const user = await getUser(event.pathParameters.userId)
    if (!user) {
      throw new Error('User not found') // ❌ Opaque error
    }
    return { statusCode: 200, body: JSON.stringify(user) }
  } catch (error) {
    console.error(error)
    return { statusCode: 500, body: 'Internal server error' }
  }
}
```

### ✅ Prefer: Railway-Oriented Handlers

```typescript
// GOOD: Explicit error handling with Result
import { APIGatewayEvent } from 'aws-lambda'
import * as TE from 'fp-ts/TaskEither'
import { pipe } from 'fp-ts/function'

type LambdaError =
  | { _tag: 'ValidationError'; message: string }
  | { _tag: 'NotFoundError'; resource: string }
  | { _tag: 'DatabaseError'; operation: string; details: unknown }
  | { _tag: 'UnexpectedError'; error: unknown }

const lambdaHandler = (event: APIGatewayEvent) =>
  pipe(
    TE.Do,
    // Kimi validates: ✓ Type safety throughout
    TE.bind('userId', () => parseAndValidateUserId(event.pathParameters?.userId)),
    TE.bind('user', ({ userId }) => fetchUserFromDynamoDB(userId)),
    TE.chainW(({ user }) => validateUserPermissions(user)),
    TE.map((user) => ({
      statusCode: 200,
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(user),
    })),
    TE.mapLeft(mapErrorToStatus) // Railway pattern
  )

// Kimi can spawn subagent to validate: ✓ Error mapping
//                                         ✓ Status code correctness
//                                         ✓ Response shape
```

### Type-Safe Handler Factory

```typescript
// Reusable pure handler factory
export const createApiHandler = <TInput, TSuccess, TError extends LambdaError>({
  validator,
  processor,
  errorMapper,
}: {
  validator: (event: APIGatewayEvent) => TE.TaskEither<TError, TInput>
  processor: (input: TInput) => TE.TaskEither<TError, TSuccess>
  errorMapper: (error: TError) => { statusCode: number; body: string }
}) => {
  return (event: APIGatewayEvent) =>
    pipe(
      TE.Do,
      TE.bind('input', () => validator(event)),
      TE.bind('result', ({ input }) => processor(input)),
      TE.map((result) => ({
        statusCode: 200,
        body: JSON.stringify(result),
      })),
      TE.mapLeft(errorMapper)
    )
}

// Usage
export const handler = createApiHandler({
  validator: validateUserRequest,
  processor: processUserUpdate,
  errorMapper: mapUserErrorToStatus,
})

// Kimi validates factory: ✓ Generic constraints
//                          ✓ Type inference
//                          ✓ Error handling uniformity
```

---

## 2. Step Functions with Railway Pattern

### State Machine as Railway

```typescript
import * as sfn from 'aws-cdk-lib/aws-stepfunctions'
import * as tasks from 'aws-cdk-lib/aws-stepfunctions-tasks'

// Railway-oriented state machine
const userOnboardingMachine = new sfn.StateMachine(
  this,
  'UserOnboardingMachine',
  {
    definitionBody: sfn.DefinitionBody.fromChainable(
      sfn.Chain.start(
        new tasks.LambdaInvoke(this, 'ValidateInput', {
          lambdaFunction: validateLambda,
          // Success: go to ProcessUser
          // Failure: go to HandleValidationError
        }).addCatch(
          new sfn.Fail(this, 'ValidationFailed', {
            error: 'Input validation failed',
          }),
          { errors: ['ValidationError'] }
        )
      )
        .next(
          new tasks.LambdaInvoke(this, 'ProcessUser', {
            lambdaFunction: processLambda,
            resultPath: '$.user',
          }).addRetry({
            errors: ['DatabaseError'],
            interval: Duration.seconds(1),
            maxAttempts: 3,
            backoffRate: 2,
          })
        )
        .next(
          new tasks.LambdaInvoke(this, 'SendWelcomeEmail', {
            lambdaFunction: emailLambda,
          }).addCatch(
            new sfn.Pass(this, 'EmailFailedButUserCreated', {
              result: sfn.Result.fromObject({
                warning:
                  'User created but welcome email failed (non-critical)',
              }),
            }),
            { errors: ['EmailError'] }
          )
        )
        .next(
          new sfn.Succeed(this, 'OnboardingComplete', {
            outputPath: '$.user',
          })
        )
    ),
    timeouts: sfn.Timeouts.standard(Duration.minutes(5)),
  }
)

// Kimi validates state machine: ✓ Railway pattern adherence
//                                  ✓ Error handling coverage
//                                  ✓ Retry logic correctness
//                                  ✓ Timeout configuration
```

### Parallel Processing with Map State

```typescript
// Parallel processing with automatic result aggregation
const processOrdersInParallel = new sfn.Map(this, 'ProcessOrdersMap', {
  inputPath: '$.orders',
  maxConcurrency: 10, // Limit parallel executions
  itemSelector: {
    'order.$': '$$.Map.Item.Value',
    'config.$': '$.config',
  },
}).iterator(
  new tasks.LambdaInvoke(this, 'ProcessSingleOrder', {
    lambdaFunction: processOrderLambda,
  }).addRetry({
    errors: ['DatabaseError'],
    maxAttempts: 3,
  })
)

// Kimi validates: ✓ Max concurrency limits
//                 ✓ Item selector correctness
//                 ✓ Result aggregation
//                 ✓ Error isolation per item
```

---

## 3. DynamoDB with Immutable Data

### Event-Sourced DynamoDB Pattern

```typescript
import { AttributeValue } from '@aws-sdk/client-dynamodb'
import { unmarshall } from '@aws-sdk/util-dynamodb'

// Immutable event store
type UserEvent =
  | { type: 'UserCreated'; payload: UserData; timestamp: string }
  | { type: 'UserUpdated'; payload: Partial<UserData>; timestamp: string }
  | { type: 'EmailVerified'; payload: { email: string }; timestamp: string }

interface UserEventRecord {
  PK: string // USER#<userId>
  SK: string // EVENT#<timestamp>#<eventId>
  event: UserEvent
  ttl: number // Optional: auto-expire old events
}

const appendEvent = (userId: string, event: UserEvent) =>
  pipe(
    TE.Do,
    TE.bind('eventRecord', () =>
      TE.right({
        PK: `USER#${userId}`,
        SK: `EVENT#${Date.now()}#${ulid()}`,
        event,
        ttl: Math.floor(Date.now() / 1000) + 60 * 60 * 24 * 365, // 1 year
      })
    ),
    TE.chainW(({ eventRecord }) =>
      TE.tryCatch(
        () => dynamoClient.put({ TableName: EVENTS_TABLE, Item: marshall(eventRecord) }),
        (err): DynamoError => ({ _tag: 'DynamoError', operation: 'put', details: err })
      )
    ),
    TE.map(() => event)
  )

// Kimi validates: ✓ Event immutability
//                 ✓ PK/SK design for time-series queries
//                 ✓ TTL configuration
//                 ✓ Error handling for idempotency
```

### Projected Current State

```typescript
// Materialized view built from events
interface UserProjection {
  userId: string
  currentState: User
  version: number // Event sequence number for optimistic locking
  lastEventTimestamp: string
}

const projectCurrentState = (events: UserEvent[]): UserProjection => {
  const sortedEvents = [...events].sort((a, b) => a.timestamp.localeCompare(b.timestamp))

  const initialState: User = {
    userId: '',
    name: '',
    email: '',
    isVerified: false,
    createdAt: '',
  }

  const projected = sortedEvents.reduce(
    (state, event) => {
      switch (event.type) {
        case 'UserCreated':
          return { ...state, ...event.payload, createdAt: event.timestamp }
        case 'UserUpdated':
          return { ...state, ...event.payload }
        case 'EmailVerified':
          return { ...state, isVerified: true }
        default:
          return state
      }
    },
    initialState
  )

  return {
    userId: projected.userId,
    currentState: projected,
    version: sortedEvents.length,
    lastEventTimestamp: sortedEvents[sortedEvents.length - 1]?.timestamp || '',
  }
}

// Kimi validates projection: ✓ Event ordering
//                             ✓ State transition purity
//                             ✓ Version tracking
//                             ✓ Default case handling
```

---

## 4. EventBridge with Railway Pattern

### Event Router with Either

```typescript
import { EventBridge } from 'aws-cdk-lib/aws-events'
import { LambdaFunction } from 'aws-cdk-lib/aws-events-targets'

type DomainEvent =
  | { type: 'UserRegistered'; userId: string; email: string }
  | { type: 'OrderPlaced'; orderId: string; userId: string; amount: number }
  | { type: 'PaymentProcessed'; paymentId: string; orderId: string; status: 'success' | 'failed' }

// Event router with railway pattern
const eventRouter = (event: DomainEvent) =>
  pipe(
    validateEvent(event),
    TE.fromEither,
    TE.chain((validatedEvent) => {
      switch (validatedEvent.type) {
        case 'UserRegistered':
          // Kimi validates: ✓ Event routing
          //                 ✓ Handler signature match
          return TE.right(publishToBus('user-emails', validatedEvent))
        case 'OrderPlaced':
          return TE.right(publishToBus('order-processing', validatedEvent))
        case 'PaymentProcessed':
          return validatedEvent.status === 'success'
            ? TE.right(publishToBus('fulfillment', validatedEvent))
            : TE.left({ _tag: 'PaymentError', details: validatedEvent })
        default:
          return TE.left({ _tag: 'UnknownEvent', event })
      }
    })
  )

// Kimi validates event router: ✓ Exhaustive pattern matching
//                                ✓ Error channel usage
//                                ✓ Event bus permissions
```

### Dead Letter Queue with Result

```typescript
// Graceful degradation with DLQ
const processEventWithRetry = (event: DomainEvent): TE.TaskEither<DeadLetterError, ProcessedResult> =>
  pipe(
    attemptProcessing(event),
    TE.orElse((error) =>
      pipe(
        TE.Do,
        TE.bind('dlqResult', () => sendToDLQ(event, error)),
        TE.bind('alertResult', () => sendAlert(error)),
        TE.chainW(() => TE.left({ _tag: 'ProcessingFailed', originalEvent: event, error }))
      )
    )
  )

// Kimi validates DLQ: ✓ Error preservation
//                       ✓ Alert sending
//                       ✓ Idempotency of DLQ send
```

---

## 5. S3 with Immutable Storage

### Versioned Objects as Immutable Facts

```typescript
// Treat S3 object versions as immutable facts
const storeImmutableLog = (bucket: string, logEntry: LogEntry) =>
  pipe(
    TE.Do,
    TE.bind('key', () => TE.right(`logs/${logEntry.timestamp}_${ulid()}.json`)),
    TE.bind('content', () => TE.right(JSON.stringify(logEntry))),
    TE.chainW(({ key, content }) =>
      TE.tryCatch(
        () =>
          s3Client.putObject({
            Bucket: bucket,
            Key: key,
            Body: content,
            ServerSideEncryption: 'aws:kms',
            Metadata: {
              // Immutable metadata
              createdAt: logEntry.timestamp,
              source: logEntry.source,
            },
          }),
        (error): S3Error => ({ _tag: 'S3Error', operation: 'put', details: error })
      )
    ),
    TE.map(({ key }) => key)
  )

// Kimi validates S3 operations: ✓ Immutability enforcement
//                                  ✓ Versioning configuration
//                                  ✓ Encryption at rest
//                                  ✓ Metadata completeness
```

### Snapshot Pattern for DynamoDB Backups

```typescript
// Immutable snapshots with S3
const createSnapshot = (tableName: string) =>
  pipe(
    TE.Do,
    TE.bind('snapshotId', () => TE.right(ulid())),
    TE.bind('timestamp', () => TE.right(new Date().toISOString())),
    TE.bind('scanResult', () => scanEntireTable(tableName)),
    TE.chainW(({ snapshotId, timestamp, scanResult }) =>
      pipe(
        serializeSnapshot({ items: scanResult.items, metadata: { timestamp, tableName } }),
        TE.chain((serialized) =>
          TE.tryCatch(
            () =>
              s3Client.putObject({
                Bucket: SNAPSHOTS_BUCKET,
                Key: `snapshots/${tableName}/${timestamp}_${snapshotId}.jsonl.gz`,
                Body: zlib.gzipSync(serialized),
                ContentType: 'application/jsonlines+gzip',
                Metadata: {
                  tableName,
                  itemCount: scanResult.count.toString(),
                  scannedCount: scanResult.scannedCount.toString(),
                },
              }),
            (error): SnapshotError => ({ _tag: 'SnapshotError', operation: 'store', details: error })
          )
        ),
        TE.map(() => ({ snapshotId, itemCount: scanResult.count }))
      )
    )
  )

// Kimi validates snapshots: ✓ Point-in-time consistency
//                              ✓ Compression verification
//                              ✓ Metadata tracking
```

---

## 6. Testing AWS Patterns

### LocalStack Testing with FP

```typescript
// Test against LocalStack (AWS emulator)
import { StartedTestContainer } from 'testcontainers'

const setupLocalStack = (): TE.TaskEither<SetupError, StartedTestContainer> =>
  pipe(
    TE.tryCatch(
      () =>
        new GenericContainer('localstack/localstack:latest')
          .withEnvironment({
            SERVICES: 'lambda,dynamodb,stepfunctions,s3,events',
            DEFAULT_REGION: 'us-east-1',
          })
          .withExposedPorts(4566)
          .start(),
      (err): SetupError => ({ _tag: 'SetupError', details: err })
    ),
    TE.tap((container) =>
      TE.tryCatch(
        () =>
          pipe(
            waitForPort(container.getMappedPort(4566)),
            TE.map((available) => {
              if (!available) throw new Error('LocalStack not ready')
            })
          ),
        (err): SetupError => ({ _tag: 'SetupError', details: err })
      )
    )
  )

// Kimi validates tests: ✓ Container orchestration
//                         ✓ Service availability checks
//                         ✓ Resource cleanup
```

### Property-Based Testing

```typescript
// Test AWS operations with property-based testing
import * as fc from 'fast-check'

describe('DynamoDB event sourcing', () => {
  it('should maintain event ordering', () =>
    fc.assert(
      fc.asyncProperty(fc.array(fc.string(), { minLength: 1 }), async (events) => {
        const userId = ulid()
        
        // Append events
        await pipe(
          events,
          A.traverse(TE.taskEither)(appendEvent(userId)),
          TE.getOrElse(() => TE.left({ _tag: 'TestError' }))
        )()
        
        // Retrieve and verify ordering
        const retrieved = await getAllEvents(userId)()
        
        expect(retrieved).toEqual(events)
      })
    )
  )
})

// Kimi validates: ✓ Property definition
//                 ✓ Test coverage
//                 ✓ Shrinking behavior
```

---

## 7. Kimi CLI Integration

### Project Setup

```bash
# 1. Environment variables
export KIMI_RULES_PATH="$HOME/projects/rules"
export KIMI_AWS_RULES="$KIMI_RULES_PATH/kimi/aws-fp-style-guide.md"

# 2. Project .kimirules
cat > .kimirules << 'EOF'
## AWS FP Rules
@${KIMI_RULES_PATH}/kimi/KIMI.md
@${KIMI_RULES_PATH}/kimi/aws-fp-style-guide.md
@${KIMI_RULES_PATH}/kimi/typescript-fp-style-guide.md

## Language-Specific
!if-exists package.json @${KIMI_RULES_PATH}/kimi/typescript-fp-style-guide.md
!if-exists requirements.txt @${KIMI_RULES_PATH}/kimi/python-fp-style-guide.md

## AWS Services
!if-exists template.yaml @${KIMI_RULES_PATH}/kimi/aws-fp-style-guide.md
!if-exists serverless.yml @${KIMI_RULES_PATH}/kimi/aws-fp-style-guide.md
EOF

# 3. Validate setup
kimi read-file .kimirules
```

### Deployment Validation Script

```bash
#!/bin/bash
# Kimi can validate AWS deployment in parallel

# Validate SAM template
sam validate --template template.yaml &

# Check Lambda handler types
python -m mypy src/handlers/ &

# Verify IAM policies (security)
python -m parliament --file iam-policies.yaml &

# Run integration tests
pytest tests/integration/ -v &

# All validations in parallel
wait
```

---

## 8. Cost Optimization with FP

### Lazy Initialization

```typescript
// Lazy resource initialization
const getDatabaseConnection = (): TE.TaskEither<ConnectionError, Database> =>
  TE.taskEither.chain(
    TE.tryCatch(
      async () => {
        // Only connect when actually needed
        if (!global.dbConnection) {
          global.dbConnection = await connectWithRetry(DATABASE_URL)
        }
        return global.dbConnection
      },
      (err): ConnectionError => ({ _tag: 'ConnectionError', details: err })
    )
  )

// Kimi validates: ✓ Lazy initialization
//                 ✓ Error recovery
//                 ✓ Singleton pattern
```

---

## Summary

AWS serverless architecture is well-suited for functional programming. Use Lambda handlers as pure functions, Step Functions for railway-oriented workflows, DynamoDB with event sourcing, EventBridge for event-driven patterns, and S3 for immutable snapshots. Kimi excels at validating multi-region deployments and complex state machines in parallel.

**Key Takeaways**:
- Lambda handlers with explicit Result types (no exceptions)
- Step Functions for railway-oriented workflows (sequential/parallel)
- DynamoDB with event sourcing (immutable events)
- EventBridge for event-driven architecture (type-safe routing)
- S3 with versioning (immutable facts)
- Kimi's parallel validation for multi-service architectures

**Why This Matters**: FP patterns eliminate entire classes of bugs in distributed systems (state mutations, race conditions, partial failures).

**Next**: See [gcp-fp-style-guide.md](gcp-fp-style-guide.md) for Google Cloud Platform FP patterns.

---

**Last Updated**: 2025-11-14  
**Maintained By**: Kimi CLI Global Rules System  
**Status**: Active
