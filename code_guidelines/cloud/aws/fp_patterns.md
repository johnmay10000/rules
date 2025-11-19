# AWS Functional Programming Patterns

**Version**: 1.0.0
**Part of**: [code_guidelines/cloud/index.json](../index.json)

> **📖 Universal Rules**: This document outlines Functional Programming patterns for AWS services (Lambda, DynamoDB, Step Functions, EventBridge).

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
```

---

## 2. Step Functions with Railway Pattern

### State Machine as Railway

```typescript
import * as sfn from 'aws-cdk-lib/aws-stepfunctions'
import * as tasks from 'aws-cdk-lib/aws-stepfunctions-tasks'

// Railway-oriented state machine
const userOnboardingMachine = new sfn.StateMachine(this, 'UserOnboardingMachine', {
  definitionBody: sfn.DefinitionBody.fromChainable(
    sfn.Chain.start(
      new tasks.LambdaInvoke(this, 'ValidateInput', {
        lambdaFunction: validateLambda,
        // Success: go to ProcessUser
        // Failure: go to HandleValidationError
      }).addCatch(
        new sfn.Fail(this, 'ValidationFailed', { error: 'Input validation failed' }),
        { errors: ['ValidationError'] }
      )
    )
    .next(
      new tasks.LambdaInvoke(this, 'ProcessUser', {
        lambdaFunction: processLambda,
        resultPath: '$.user',
      }).addRetry({ errors: ['DatabaseError'], maxAttempts: 3 })
    )
  )
})
```

### Parallel Processing with Map State

```typescript
// Parallel processing with automatic result aggregation
const processOrdersInParallel = new sfn.Map(this, 'ProcessOrdersMap', {
  inputPath: '$.orders',
  maxConcurrency: 10,
  itemSelector: {
    'order.$': '$$.Map.Item.Value',
    'config.$': '$.config',
  },
}).iterator(
  new tasks.LambdaInvoke(this, 'ProcessSingleOrder', {
    lambdaFunction: processOrderLambda,
  })
)
```

---

## 3. DynamoDB with Immutable Data

### Event-Sourced DynamoDB Pattern

```typescript
// Immutable event store
type UserEvent =
  | { type: 'UserCreated'; payload: UserData; timestamp: string }
  | { type: 'UserUpdated'; payload: Partial<UserData>; timestamp: string }

interface UserEventRecord {
  PK: string // USER#<userId>
  SK: string // EVENT#<timestamp>#<eventId>
  event: UserEvent
  ttl: number
}

const appendEvent = (userId: string, event: UserEvent) =>
  pipe(
    TE.Do,
    TE.bind('eventRecord', () => TE.right({
      PK: `USER#${userId}`,
      SK: `EVENT#${Date.now()}#${ulid()}`,
      event,
      ttl: Math.floor(Date.now() / 1000) + 31536000,
    })),
    TE.chainW(({ eventRecord }) =>
      TE.tryCatch(
        () => dynamoClient.put({ TableName: EVENTS_TABLE, Item: marshall(eventRecord) }),
        (err): DynamoError => ({ _tag: 'DynamoError', operation: 'put', details: err })
      )
    )
  )
```

---

## 4. EventBridge with Railway Pattern

### Event Router with Either

```typescript
// Event router with railway pattern
const eventRouter = (event: DomainEvent) =>
  pipe(
    validateEvent(event),
    TE.fromEither,
    TE.chain((validatedEvent) => {
      switch (validatedEvent.type) {
        case 'UserRegistered':
          return TE.right(publishToBus('user-emails', validatedEvent))
        case 'OrderPlaced':
          return TE.right(publishToBus('order-processing', validatedEvent))
        default:
          return TE.left({ _tag: 'UnknownEvent', event })
      }
    })
  )
```

---

## 5. S3 with Immutable Storage

### Versioned Objects as Immutable Facts

```typescript
// Treat S3 object versions as immutable facts
const storeImmutableLog = (bucket: string, logEntry: LogEntry) =>
  pipe(
    TE.tryCatch(
      () => s3Client.putObject({
        Bucket: bucket,
        Key: `logs/${logEntry.timestamp}_${ulid()}.json`,
        Body: JSON.stringify(logEntry),
        Metadata: { createdAt: logEntry.timestamp }
      }),
      (error): S3Error => ({ _tag: 'S3Error', operation: 'put', details: error })
    )
  )
```

---

## 6. Testing AWS Patterns

### LocalStack Testing with FP

```typescript
// Test against LocalStack (AWS emulator)
const setupLocalStack = (): TE.TaskEither<SetupError, StartedTestContainer> =>
  pipe(
    TE.tryCatch(
      () => new GenericContainer('localstack/localstack:latest')
        .withEnvironment({ SERVICES: 'lambda,dynamodb' })
        .start(),
      (err): SetupError => ({ _tag: 'SetupError', details: err })
    )
  )
```

### Property-Based Testing

```typescript
// Test AWS operations with property-based testing
describe('DynamoDB event sourcing', () => {
  it('should maintain event ordering', () =>
    fc.assert(
      fc.asyncProperty(fc.array(fc.string()), async (events) => {
        // Append events and verify retrieval order
      })
    )
  )
})
```
