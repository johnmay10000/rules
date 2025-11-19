# Universal Serverless Functional Programming Patterns

**Version**: 1.0.0
**Part of**: [code_guidelines/index.json](../index.json)

> **📖 Universal Rules**: This document outlines Functional Programming patterns that apply across all serverless platforms (AWS Lambda, Google Cloud Functions, Azure Functions, etc.).

---

## Core Principles

1.  **Immutable Infrastructure**: Deployments are immutable artifacts. Never mutate running services; redeploy.
2.  **Pure Functions**: Treat handlers as pure functions where possible. Isolate side effects (DB, API calls) to the edges.
3.  **Event Sourcing**: Treat events as immutable facts. Store events, not just current state.
4.  **Explicit Error Handling**: Use `Result`/`Either` types for all operations. No unchecked exceptions.
5.  **Idempotency**: Design for at-least-once delivery. Handlers must be safe to retry.

---

## 1. The Pure Handler Pattern

Isolate your business logic from the cloud provider's specific handler signature.

### ❌ Avoid: Logic Mixed with Infrastructure

```typescript
// BAD: Business logic mixed with AWS/GCP specifics
export const handler = async (event: any) => {
  try {
    // Validation logic mixed with parsing
    if (!event.body) throw new Error("No body");
    const data = JSON.parse(event.body);
    
    // Business logic mixed with DB calls
    const result = await db.save(data);
    
    return { statusCode: 200, body: JSON.stringify(result) };
  } catch (e) {
    return { statusCode: 500, body: "Error" };
  }
};
```

### ✅ Prefer: Generic Handler Factory

Create a factory that adapts a pure function to the cloud provider's signature.

```typescript
// Universal Pure Function Signature
type PureHandler<Input, Output, Error> = (input: Input) => TaskEither<Error, Output>;

// Generic Factory (Provider Agnostic Concept)
const createHandler = <I, O, E>(
  validator: (raw: unknown) => TaskEither<E, I>,
  businessLogic: PureHandler<I, O, E>,
  errorMapper: (e: E) => Response
) => {
  return async (rawEvent: unknown) => {
    return pipe(
      validator(rawEvent),
      TE.chain(businessLogic),
      TE.match(
        errorMapper,
        (success) => ({ statusCode: 200, body: JSON.stringify(success) })
      )
    )();
  };
};
```

---

## 2. Railway-Oriented Programming (ROP)

Use "Railway Oriented Programming" to handle errors as data flows, not control flow jumps.

-   **Success Track**: The happy path (Right/Ok).
-   **Failure Track**: The error path (Left/Err).
-   **Switch**: Functions that can fail (`Input -> Result<Output, Error>`).

```typescript
// The "Railway" pipeline
const processEvent = (event: Event) => pipe(
  TE.Do,
  TE.bind('validated', () => validate(event)),       // Switch 1
  TE.bind('enriched',  ({validated}) => enrich(validated)), // Switch 2
  TE.bind('saved',     ({enriched}) => save(enriched)),     // Switch 3
  TE.map(({saved}) => createResponse(saved))
);
```

---

## 3. Event Sourcing & Immutability

In serverless, state should be external and immutable where possible.

### The Pattern
Instead of updating a record in place (`UPDATE users SET email = '...'`), append an event (`INSERT INTO events (type: 'EmailUpdated', ...)`) and project the current state.

1.  **Append**: Write immutable event to storage (DynamoDB, Firestore, S3).
2.  **Project**: Read all events for an entity and reduce them to current state.

```typescript
// Immutable Event
type UserEvent = 
  | { type: 'Created', data: UserData }
  | { type: 'EmailChanged', newEmail: string };

// Projection (Pure Function)
const projectUser = (events: UserEvent[]): User => 
  events.reduce((state, event) => {
    switch(event.type) {
      case 'Created': return { ...state, ...event.data };
      case 'EmailChanged': return { ...state, email: event.newEmail };
    }
  }, initialUser);
```

---

## 4. Testing Strategies

### Property-Based Testing
Since serverless functions are pure transformations of data, they are perfect for property-based testing.

-   **Property**: "For any valid event, the handler should never throw an exception."
-   **Property**: "For any sequence of events, the projected state should be consistent."

### Emulator / LocalStack Testing
Use containerized emulators (LocalStack for AWS, Firebase Emulators for GCP) to test integration without deploying.

---

## 5. Idempotency

Serverless functions may be retried. Your logic must handle this.

-   **Input**: Event ID (e.g., `context.awsRequestId` or `event.id`).
-   **Check**: Has this ID been processed?
-   **Action**: If yes, return stored result. If no, process and store result.

```typescript
const withIdempotency = (handler) => (event) => pipe(
  checkIdempotencyStore(event.id),
  TE.chain((stored) => stored ? TE.right(stored) : handler(event)),
  TE.chainFirst((result) => saveToIdempotencyStore(event.id, result))
);
```
