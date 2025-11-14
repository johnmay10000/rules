# TypeScript FP Example: Next.js Web App with Effect-ts

**Project Type**: Full-Stack Web Application  
**Framework**: Next.js 13+ (App Router)  
**Language**: TypeScript 5.0+  
**Backend**: Supabase (PostgreSQL)  
**Job Queue**: Inngest (Serverless functions)  
** demonstrates**: Railway-oriented programming, Effect-ts, error handling, type safety  

---

## 📋 Project Overview  

This example demonstrates functional programming patterns in TypeScript using Effect-ts for a full-stack web application. The application follows railway-oriented programming principles with explicit error handling, comprehensive type safety, and composable business logic.

### Tech Stack  

- **Frontend**: Next.js 13+ (App Router), React Server Components  
- **Backend**: Supabase (PostgreSQL with real-time subscriptions)  
- **API Layer**: tRPC for type-safe API calls  
- **Job Processing**: Inngest for serverless background jobs  
- **FP Library**: Effect-ts (Result, Option, Either, Stream)  
- **Styling**: Tailwind CSS + shadcn/ui  
- **Validation**: Zod schemas with type inference  

### Key FP Concepts Demonstrated  

- **Effect-ts**: Railway programming with Result, Option, Either  
- **Monadic composition**: Using `pipe`, `chain`, `map` for sequencing  
- **Type safety**: End-to-end type safety from DB to frontend  
- **Error accumulation**: Collect multiple validation errors  
- **Streaming data**: Real-time updates with Effect streams  
- **Dependency injection**: Reader pattern with Effect  

---  

## 🏗️ Project Structure  

```
typescript_project/
├── package.json                       # Dependencies and scripts
├── tsconfig.json                      # TypeScript configuration
├── .env.local.example                 # Environment variables template
├── README.md                          # This file
├── ARCHITECTURE_PLAN.md              # Tier 1: Strategic overview
├── docs/
│   ├── plans/
│   │   ├── APP_DESIGN.md             # Tier 2: Technical design
│   │   └── IMPLEMENTATION_TODO.md    # Tier 2: Task tracking
│   └── 2025_11_14/                   # Tier 3: Execution logs
│       ├── 20251114_0001_PROJECT_SETUP.md
│       ├── 20251114_0002_API_DESIGN.md
│       └── 20251114_0003_EFFECT_INTEGRATION.md
├── src/
│   ├── app/                          # Next.js App Router
│   │   ├── layout.tsx                # Root layout
│   │   ├── page.tsx                  # Home page
│   │   ├── dashboard/
│   │   │   └── page.tsx              # Dashboard with real-time data
│   │   └── api/                      # API routes
│   │       └── trpc/
│   │           └── [trpc]/route.ts   # tRPC route handler
│   ├── components/                   # React components
│   │   ├── DataTable.tsx            # Table with streaming updates
│   │   ├── ErrorBoundary.tsx        # Error handling
│   │   └── JobStatus.tsx            # Inngest job status display
│   ├── lib/
│   │   ├── db.ts                     # Supabase client
│   │   ├── inngest.ts                # Inngest client
│   │   └── trpc.ts                   # tRPC client/server setup
│   ├── server/
│   │   ├── context.ts                # Effect-ts context
│   │   ├── errors.ts                 # Error definitions
│   │   ├── repositories/             # Data access layer
│   │   │   └── userRepository.ts
│   │   └── trpc/
│   │       ├── _app.ts               # tRPC router
│   │       └── user.ts               # User procedures
│   └── shared/
│       ├── domain.ts                 # Domain types and schemas
│       ├── effectTypes.ts            # Effect-ts type helpers
│       └── utils.ts                  # FP utilities
└── inngest/                          # Background job functions
    ├── functions/
    │   ├── processUserUpload.ts
    │   └── sendNotification.ts
    └── middleware/
        └── auth.ts
```  

---  

## 🚀 Quick Start  

### Prerequisites  

- Node.js 18+ with pnpm/npm/yarn  
- Supabase account and project  
- Inngest account (optional, for background jobs)  

### Installation  

```bash  
# Install dependencies  
pnpm install  

# Copy environment variables  
cp .env.local.example .env.local  
# Edit .env.local with your Supabase and Inngest credentials  

# Start development server  
pnpm dev  

# Run tests  
pnpm test  

# Type check  
pnpm type-check  
```  

### Running the Application  

```bash  
# Development  
pnpm dev          # Start Next.js dev server  
pnpm supabase     # Start Supabase local (if using local)  
pnpm inngest:dev  # Start Inngest dev server  

# Production build  
pnpm build  
pnpm start  
```  

---  

## 💡 FP Patterns Demonstrated  

### 1. Effect-ts Result Type for Error Handling  

Instead of throwing exceptions, functions return `Result` types:  

```typescript  
import { Effect, pipe } from "effect"  
import { Schema } from "@effect/schema/Schema"  

// Domain types  
const UserSchema = Schema.struct({  
  id: Schema.number,  
  email: Schema.string,  
  name: Schema.string,  
  createdAt: Schema.date,  
})  

type User = Schema.To<typeof UserSchema>  

// Repository with explicit errors  
type DatabaseError = { readonly _tag: "DatabaseError"; message: string }  
type NotFoundError = { readonly _tag: "NotFoundError"; id: number }  
type ValidationError = { readonly _tag: "ValidationError"; message: string }  

// Result type combining possible errors  
type UserError = DatabaseError | NotFoundError | ValidationError  

// Repository function returning Result  
function getUserById(  
  userId: number  
): Effect.Effect<User, UserError, { db: SupabaseClient }> {  
  return Effect.gen(function* (_) {  
    const { db } = yield* _(Effect.context<{ db: SupabaseClient }>())  
    
    const { data, error } = yield* _(Effect.tryPromise({  
      try: () => db.from("users").select("*").eq("id", userId).single(),  
      catch: (e) => ({ _tag: "DatabaseError", message: String(e) } as DatabaseError)  
    }))  
    
    if (error) {  
      return yield* _(Effect.fail({ _tag: "DatabaseError", message: error.message } as DatabaseError))  
    }  
    
    if (!data) {  
      return yield* _(Effect.fail({ _tag: "NotFoundError", id: userId } as NotFoundError))  
    }  
    
    // Validate with schema  
    const user = yield* _(Effect.try({  
      try: () => Schema.decode(UserSchema)(data),  
      catch: (e) => ({ _tag: "ValidationError", message: String(e) } as ValidationError)  
    }))  
    
    return user  
  })  
}  

// Usage  
const program = pipe(  
  getUserById(123),  
  Effect.tap((user) => Effect.log(`Found user: ${user.name}`)),  
  Effect.catchAll((error) =>  
    pipe(  
      Effect.log(`Error: ${error._tag}`),  
      Effect.flatMap(() => Effect.fail(error))  
    )  
  )  
)  
```  

### 2. Railway-Oriented Programming with Pipes  

Pipeline composition with error handling:  

```typescript  
import { Effect, pipe } from "effect"  

// Data flow: Input → Validation → Processing → Validation → Output  
function processUserUpload(  
  file: File  
): Effect.Effect<ProcessedUserData, ProcessingError, { db: SupabaseClient }> {  
  return pipe(  
    // Step 1: Validate file type  
    validateFileType(file),  
    
    // Step 2: Upload to storage (if validation passed)  
    Effect.flatMap((validFile) =>  
      uploadToStorage(validFile)  
    ),  
    
    // Step 3: Parse CSV/JSON content  
    Effect.flatMap((uploadResult) =>  
      parseFileContent(uploadResult.url)  
    ),  
    
    // Step 4: Validate each record  
    Effect.flatMap((records) =>  
      validateRecords(records)  
    ),  
    
    // Step 5: Save to database  
    Effect.flatMap((validRecords) =>  
      saveToDatabase(validRecords)  
    ),  
    
    // Step 6: Format success response  
    Effect.map((savedRecords) => ({  
      count: savedRecords.length,  
      status: "success",  
      timestamp: new Date(),  
    })),  
    
    // Error handling for entire pipeline  
    Effect.catchAll((error) =>  
      pipe(  
        Effect.logError(`Processing failed: ${error}`),  
        Effect.flatMap(() => Effect.fail(error))  
      )  
    )  
  )  
}  

// Type definitions  
type ProcessingError =   
  | { _tag: "InvalidFileType"; message: string }  
  | { _tag: "UploadFailed"; message: string }  
  | { _tag: "ParseError"; message: string; line?: number }  
  | { _tag: "ValidationError"; message: string }  
  | { _tag: "DatabaseError"; message: string }  

interface ProcessedUserData {  
  count: number  
  status: "success"  
  timestamp: Date  
}  
```  

### 3. Option Type for Null Safety  

Handling optional values without null/undefined:  

```typescript  
import { Option, pipe } from "effect"  

// Safe property access  
function findUserByEmail(  
  email: string  
): Effect.Effect<Option<User>, DatabaseError, { db: SupabaseClient }> {  
  return Effect.gen(function* (_) {  
    const { db } = yield* _(Effect.context<{ db: SupabaseClient }>())  
    
    const { data } = yield* _(Effect.tryPromise({  
      try: () => db.from("users").select("*").eq("email", email).maybeSingle(),  
      catch: (e) => ({ _tag: "DatabaseError", message: String(e) } as DatabaseError)  
    }))  
    
    return Option.fromNullable(data)  
  })  
}  

// Usage with Option  
const program = pipe(  
  findUserByEmail("user@example.com"),  
  Effect.flatMap((maybeUser) =>  
    pipe(  
      maybeUser,  
      Option.match({  
        onNone: () => Effect.log("User not found"),  
        onSome: (user) => Effect.log(`Found user: ${user.name}`)  
      })  
    )  
  )  
)  
```  

### 4. Error Accumulation (Multiple Errors)  

Collect and report multiple validation errors:  

```typescript  
import { Effect, Either } from "effect"  

// Validate a user record and collect all errors  
function validateUser(  
  user: unknown  
): Either.Either<User, ReadonlyArray<ValidationError>> {  
  const errors: ValidationError[] = []  
  
  // Check email format  
  if (!user.email || !user.email.includes("@")) {  
    errors.push({ _tag: "ValidationError", field: "email", message: "Invalid email format" })  
  }  
  
  // Check name length  
  if (!user.name || user.name.length < 2) {  
    errors.push({ _tag: "ValidationError", field: "name", message: "Name too short" })  
  }  
  
  // Check age range (if present)  
  if (user.age && (user.age < 18 || user.age > 120)) {  
    errors.push({ _tag: "ValidationError", field: "age", message: "Age out of range" })  
  }  
  
  // Return either valid user or all collected errors  
  return errors.length > 0   
    ? Either.left(errors)  
    : Either.right(user as User)  
}  

// Process batch with error accumulation  
function validateRecords(  
  records: unknown[]  
): Effect.Effect<User[], ReadonlyArray<{ record: number; errors: ValidationError[] }>>, never> {  
  return Effect.gen(function* (_) {  
    const results = records.map((record, index) => ({  
      record: index,  
      result: validateUser(record),  
    }))  
    
    // Separate successes from failures  
    const validUsers: User[] = []  
    const failures: Array<{ record: number; errors: ValidationError[] }> = []  
    
    for (const { record, result } of results) {  
      Either.match(result, {  
        onLeft: (errors) => failures.push({ record, errors }),  
        onRight: (user) => validUsers.push(user),  
      })  
    }  
    
    if (failures.length > 0) {  
      return yield* _(Effect.fail(failures))  
    }  
    
    return validUsers  
  })  
}  
```  

### 5. Streaming with Effect  

Real-time data streaming for live updates:  

```typescript  
import { Stream, Effect, Sink } from "effect"  

// Create a stream of database changes  
function watchUsers(): Stream.Stream<User, DatabaseError, { db: SupabaseClient }> {  
  return Stream.async<User, DatabaseError>((emit) => {  
    const { db } = Effect.unsafeRunSync(Effect.context<{ db: SupabaseClient }>())  
    
    // Subscribe to real-time changes  
    const subscription = db  
      .channel("users")  
      .on("postgres_changes", { event: "*", schema: "public", table: "users" }, (payload) => {  
        const user = payload.new as User  
        emit.single(user)  
      })  
      .subscribe()  
    
    return Effect.sync(() => {  
      subscription.unsubscribe()  
    })  
  })  
}  

// Consume the stream  
const program = pipe(  
  watchUsers(),  
  Stream.tap((user) => Effect.log(`User ${user.id} changed`)),  
  Stream.run(Sink.forEach((user) =>   
    Effect.sync(() => {  
      // Update UI in real-time  
      console.log("Update UI with:", user)  
    })  
  ))  
)  
```  

### 6. Reader Pattern for Dependency Injection  

Managing dependencies with Effect context:  

```typescript  
import { Context, Effect, Layer } from "effect"  

// Define service interfaces  
interface Database {  
  readonly query: (sql: string) => Effect.Effect<any, DatabaseError>  
  readonly getUser: (id: number) => Effect.Effect<User, NotFoundError | DatabaseError>  
}  

interface EmailService {  
  readonly sendEmail: (to: string, subject: string, body: string) => Effect.Effect<void, EmailError>  
}  

// Create service tags  
const Database = Context.Tag<Database>()  
const EmailService = Context.Tag<EmailService>()  

// Implement services  
const DatabaseLive = Layer.succeed(Database, {  
  query: (sql) => Effect.tryPromise({  
    try: () => supabase.rpc("query", { sql }),  
    catch: (e) => ({ _tag: "DatabaseError", message: String(e) } as DatabaseError),  
  }),  
  getUser: (id) => Effect.gen(function* (_) {  
    const db = yield* _(Database)  
    const result = yield* _(db.query(`SELECT * FROM users WHERE id = ${id}`))  
    if (!result.data) {  
      return yield* _(Effect.fail({ _tag: "NotFoundError", id } as NotFoundError))  
    }  
    return result.data  
  }),  
})  

const EmailServiceLive = Layer.succeed(EmailService, {  
  sendEmail: (to, subject, body) => Effect.tryPromise({  
    try: () => emailClient.send({ to, subject, body }),  
    catch: (e) => ({ _tag: "EmailError", message: String(e) } as EmailError),  
  }),  
})  

// Complex business logic with dependencies  
function registerUser(  
  userData: NewUserData  
): Effect.Effect<User, UserError, Database | EmailService> {  
  return Effect.gen(function* (_) {  
    const db = yield* _(Database)  
    const email = yield* _(EmailService)  
    
    // Validate user data  
    const validated = yield* _(validateUserData(userData))  
    
    // Check if email already exists  
    const existing = yield* _(db.query(`SELECT * FROM users WHERE email = '${validated.email}'`))  
    if (existing.data) {  
      return yield* _(Effect.fail({  
        _tag: "ValidationError",  
        message: "Email already registered"  
      } as ValidationError))  
    }  
    
    // Create user  
    const newUser = yield* _(db.query(`INSERT INTO users ... RETURNING *`))  
    
    // Send welcome email  
    yield* _(email.sendEmail(  
      validated.email,  
      "Welcome!",  
      "Welcome to our platform..."  
    ))  
    
    return newUser.data  
  })  
}  

// Run with services  
const program = pipe(  
  registerUser({ email: "user@example.com", name: "John Doe" }),  
  Effect.provideLayer(Layer.merge(DatabaseLive, EmailServiceLive))  
)  
```  

---  

## 📊 Application Architecture  

```  
Client Request  
    ↓  
[Next.js Route Handler] → Effect<HttpError, { db, inngest }>  
    ↓  
[tRPC createCaller] (Context creation)  
    → Effect<TRPCError, { db, session, user }>  
    ↓  
[tRPC Procedure] (Business logic)  
    → Effect<BusinessError, { db, context }>  
    ↓  
[Repository Layer] (Data access)  
    → Effect<DataError, { db }>  
    ↓  
[Supabase/Postgres] (Persistence)  
    → Raw data  
    ↓  
[Schema Validation] (Zod/Effect Schema)  
    → Result<DomainType, ValidationError>  
    ↓  
[Domain Mapping] (Type transformation)  
    → Effect<DomainType, never>  
    ↓  
[Response Formation] (API response)  
    → Effect<SuccessResponse, Error>  
    ↓  
[Client-Side Cache Update] (Optimistic update)  
    → tRPC query cache update  
    ↓  
[React UI] (Rendering)  
    → User interface with error boundaries  
```  

---  

## 🎛️ Inngest Integration  

### Background Jobs with FP  

```typescript  
import { inngest } from "./inngest/client"  
import { Effect } from "effect"  

// Define job types  
interface UserUploadJob {  
  name: "user/upload"  
  data: { fileUrl: string; uploadedBy: string }  
}  

// Create serverless function with Effect  
export default inngest.createFunction(  
  { name: "Process User Upload" },  
  { event: "user/upload" },  
  async ({ event, step }) => {  
    const program = Effect.gen(function* (_) {  
      // Step 1: Download file  
      const fileContent = yield* _(step.run("download-file", async () => {  
        const response = await fetch(event.data.fileUrl)  
        return response.text()  
      }))  
      
      // Step 2: Parse and validate  
      const records = yield* _(parseFileContent(fileContent))  
      
      // Step 3: Validate records  
      const validRecords = yield* _(validateRecords(records))  
      
      // Step 4: Save to database  
      const saved = yield* _(saveToDatabase(validRecords))  
      
      // Step 5: Send notifications  
      yield* _(sendNotificationToUploader(event.data.uploadedBy, saved.length))  
      
      return { processed: saved.length }  
    })  
    
    // Run with error handling  
    return Effect.runPromise(program).catch((error) => {  
      console.error("Job failed:", error)  
      throw error // Inngest will handle retry  
    })  
  }  
)  
```  

---  

## 🎯 Real-World Scenarios  

### Scenario 1: Transactional Operations  

```typescript  
function transferFunds(  
  fromAccount: string,  
  toAccount: string,  
  amount: number  
): Effect.Effect<TransferResult, TransferError, { db: SupabaseClient }> {  
  return Effect.gen(function* (_) {  
    const { db } = yield* _(Effect.context<{ db: SupabaseClient }>())  
    
    // Start transaction  
    const tx = yield* _(db.rpc("begin"))  
    
    try {  
      // Check from account balance  
      const fromBalance = yield* _(checkBalance(fromAccount))  
      
      if (fromBalance < amount) {  
        yield* _(db.rpc("rollback"))  
        return yield* _(Effect.fail({  
          _tag: "InsufficientFunds",  
          message: "Not enough balance"  
        } as TransferError))  
      }  
      
      // Debit from account  
      yield* _(updateBalance(fromAccount, -amount))  
      
      // Credit to account  
      yield* _(updateBalance(toAccount, amount))  
      
      // Record transaction  
      const transaction = yield* _(recordTransaction({  
        from: fromAccount,  
        to: toAccount,  
        amount  
      }))  
      
      // Commit transaction  
      yield* _(db.rpc("commit"))  
      
      return transaction  
    } catch (error) {  
      yield* _(db.rpc("rollback"))  
      return yield* _(Effect.fail({  
        _tag: "TransactionError",  
        message: "Transfer failed",  
        cause: error  
      } as TransferError))  
    }  
  })  
}  
```  

### Scenario 2: Retry with Backoff  

```typescript  
import { Schedule } from "effect"  

function fetchWithRetry<T>(  
  url: string,  
  options?: RequestInit  
): Effect.Effect<T, NetworkError> {  
  return pipe(  
    Effect.tryPromise({  
      try: () => fetch(url, options).then(r => r.json()),  
      catch: (e) => ({ _tag: "NetworkError", message: String(e) } as NetworkError)  
    }),  
    Effect.retry(  
      Schedule.exponential(1000).pipe(  // Start with 1s, doubling each time  
        Schedule.jittered,              // Add random jitter  
        Schedule.whileOutput((n) => n < 32000),  // Max 32s between retries  
        Schedule.recurs(5)              // Max 5 attempts  
      )  
    )  
  )  
}  
```  

### Scenario 3: Parallel Data Fetching  

```typescript  
function fetchDashboardData(  
  userId: number  
): Effect.Effect<DashboardData, DashboardError, { db: SupabaseClient }> {  
  return Effect.gen(function* (_) {  
    const { db } = yield* _(Effect.context<{ db: SupabaseClient }>())  
    
    // Fetch all data in parallel (common pattern in Kimi!)  
    const [user, stats, recentActivity] = yield* _(Effect.all([  
      getUserById(userId),  
      getUserStats(userId),  
      getRecentActivity(userId)  
    ], { concurrency: "parallel" }))  
    
    return {  
      user,  
      stats,  
      recentActivity  
    }  
  })  
}  
```  

---  

## 🧪 Testing Strategy  

### Unit Tests with Effect  

```typescript  
import { Effect, Either } from "effect"  
import { describe, it, expect } from "vitest"  

describe("validateUser", () => {  
  it("should return valid user", () => {  
    const validUser = {  
      email: "user@example.com",  
      name: "John Doe",  
      age: 30  
    }  
    
    const result = Effect.runSync(validateUser(validUser))  
    
    expect(Either.isRight(result)).toBe(true)  
    if (Either.isRight(result)) {  
      expect(result.right.email).toBe("user@example.com")  
    }  
  })  
  
  it("should return validation errors", () => {  
    const invalidUser = {  
      email: "invalid-email",  
      name: "J",  // Too short  
      age: 200    // Too old  
    }  
    
    const result = Effect.runSync(validateUser(invalidUser))  
    
    expect(Either.isLeft(result)).toBe(true)  
    if (Either.isLeft(result)) {  
      expect(result.left).toHaveLength(3)  // 3 validation errors  
      expect(result.left.some(e => e.field === "email")).toBe(true)  
      expect(result.left.some(e => e.field === "name")).toBe(true)  
      expect(result.left.some(e => e.field === "age")).toBe(true)  
    }  
  })  
})  

describe("processUserUpload", () => {  
  it("should process upload successfully", async () => {  
    const mockFile = new File(["test content"], "test.csv", { type: "text/csv" })  
    
    const result = await Effect.runPromise(  
      pipe(  
        processUserUpload(mockFile),  
        Effect.provideLayer(Layer.merge(MockDatabase, MockStorage))  
      )  
    )  
    
    expect(result.status).toBe("success")  
    expect(result.count).toBe(1)  
  })  
})  
```  

---  

## 🔧 Kimi-Specific Integration  

### Using SetTodoList for App Development  

```typescript  
// Task structure for development  
interface DevelopmentTask {  
  id: string  
  title: string  
  status: "pending" | "in-progress" | "done"  
  estimate?: number  // hours  
  actual?: number   // hours  
}  

const appDevelopmentTasks: DevelopmentTask[] = [  
  {  
    id: "task-1",  
    title: "Setup Next.js project with TypeScript",  
    status: "done",  
    estimate: 0.5,  
    actual: 0.5  
  },  
  {  
    id: "task-2",  
    title: "Setup Supabase client and types",  
    status: "done",   
    estimate: 0.5,  
    actual: 0.75  
  },  
  {  
    id: "task-3",  
    title: "Setup tRPC with Effect-ts integration",  
    status: "in-progress",  
    estimate: 1.0,  
    actual: 0.8  
  },  
  // ... more tasks  
]  
```  

### Parallel Validation with Kimi  

```bash  
# Kimi can validate multiple components simultaneously (parallel execution)  
ReadFile: src/server/repositories/userRepository.ts  
ReadFile: src/server/trpc/user.ts  
ReadFile: src/components/DataTable.tsx  
ReadFile: src/lib/db.ts  

# Type check everything in parallel  
pnpm type-check:server &  # Server types  
cd src && npx tsc --noEmit &  # Client types  
pnpm check:effect-types &  # Effect type checking  
wait  

# Run all tests in parallel  
pnpm test:unit &  
pnpm test:integration &  
pnpm test:e2e &  
wait  
```  

---  

## 📈 Benefits of FP Approach  

### Compared to Traditional TypeScript/React Code:  

| Aspect | Traditional | FP with Effect-ts |
|--------|-------------|-------------------|
| Error handling | Try/catch | Explicit Result types |
| Null safety | Optional chaining | Option type |
| Async handling | Promises/await | Effect with full control |
| Type safety | Partial (type assertions) | End-to-end (Effect + Zod) |
| Testing | Mock everything | Test pure Effect values |
| Composition | Manual promise chains | Railway composition |
| Dependency injection | Global instances | Reader pattern |

---  

## 🔗 Cross-References  

**Project Documentation:**  
- [ARCHITECTURE_PLAN.md](./ARCHITECTURE_PLAN.md) - Strategic overview  
- [docs/plans/APP_DESIGN.md](./docs/plans/APP_DESIGN.md) - Technical design  
- [docs/plans/IMPLEMENTATION_TODO.md](./docs/plans/IMPLEMENTATION_TODO.md) - Task tracking  

**Kimi Guides:**  
- [kimi/KIMI_WORKFLOW_GUIDE.md](../../kimi/KIMI_WORKFLOW_GUIDE.md) - Workflow patterns  
- [kimi/typescript-fp-style-guide.md](../../kimi/typescript-fp-style-guide.md) - TypeScript FP patterns  
- [kimi/DATA_STRUCTURE_PATTERNS.md](../../kimi/DATA_STRUCTURE_PATTERNS.md) - FP data structures  

**Technology References:**  
- [Effect-ts Documentation](https://effect.website/)  
- [Next.js Documentation](https://nextjs.org/)  
- [Supabase Documentation](https://supabase.com/docs)  
- [Inngest Documentation](https://inngest.com/docs)  

---  

## ✅ Completion Checklist  

This example is complete when:  
- [ ] Next.js app running with TypeScript  
- [ ] Supabase integration with type-safe queries  
- [ ] tRPC setup with Effect-ts procedures  
- [ ] Inngest functions for background jobs  
- [ ] Client components with error boundaries  
- [ ] Real-time features with Supabase subscriptions  
- [ ] Comprehensive tests (unit + integration)  
- [ ] Documentation hierarchy established (3 tiers)  
- [ ] SetTodoList usage demonstrated  
- [ ] Parallel validation examples created  
- [ ] Cross-references functional  
- [ ] Follows KIMI.md conventions  
- [ ] Git checkpoint with completion summary  

---  

**Status**: 🔄 IN PROGRESS (baseline established)  
**Last Updated**: 2025-11-14 20:15  
**Maintained By**: Kimi CLI Global Rules System  

🤖 Generated with [Kimi](https://kimi.ai)  

---  

*This is a full-stack TypeScript example using Effect-ts for functional programming patterns. See [kimi/KIMI_WORKFLOW_GUIDE.md](../../kimi/KIMI_WORKFLOW_GUIDE.md) for Kimi-specific usage patterns.*  
