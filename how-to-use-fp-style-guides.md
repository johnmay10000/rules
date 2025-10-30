# How to Use FP Style Guides with Claude Code

This guide explains how to integrate the Python and TypeScript functional programming style guides into your development workflow with Claude Code.

---

## Table of Contents

1. [Quick Setup](#quick-setup)
2. [Integration Methods](#integration-methods)
3. [Using with Claude Code](#using-with-claude-code)
4. [Project Setup](#project-setup)
5. [Key Differences Between Guides](#key-differences-between-guides)
6. [Quick Start Examples](#quick-start-examples)
7. [Best Practices](#best-practices)
8. [Troubleshooting](#troubleshooting)

---

## Quick Setup

### Python (MLX Project)

```bash
# Install required libraries
pip install returns toolz --break-system-packages
pip install mypy --break-system-packages

# Optional but recommended
pip install attrs  # For frozen dataclasses alternative
```

### TypeScript (Next.js/Supabase/Inngest)

```bash
# Choose one FP library:

# Option 1: fp-ts (closer to Haskell)
npm install fp-ts

# Option 2: Effect (more ergonomic, recommended)
npm install effect

# Optional but recommended
npm install monocle-ts  # For lenses
npm install io-ts       # For runtime validation
```

---

## Integration Methods

### Method 1: Upload as Custom Skills (Recommended)

Claude Code automatically reads skills before generating code. This is the most seamless integration.

**Steps:**

1. Create the skills directory structure:
```bash
mkdir -p /mnt/skills/user/python-fp
mkdir -p /mnt/skills/user/typescript-fp
```

2. Copy the style guides:
```bash
cp python-fp-style-guide.md /mnt/skills/user/python-fp/SKILL.md
cp typescript-fp-style-guide.md /mnt/skills/user/typescript-fp/SKILL.md
```

3. Claude Code will now:
   - Automatically read these skills when working on Python/TypeScript projects
   - Follow the patterns and conventions
   - Use the recommended libraries
   - Apply the functional programming idioms

**Verification:**
```bash
# List your custom skills
ls -la /mnt/skills/user/
```

### Method 2: Project-Level Documentation

Place style guides directly in your project repositories.

**Structure:**
```
project-root/
├── docs/
│   ├── CODE_STYLE.md          # Your style guide
│   └── examples/
│       ├── error-handling.ts
│       └── composition.ts
├── .claude/
│   └── style-guide.md         # Alternative location
└── README.md
```

**Usage:**
When prompting Claude Code, reference the guide:
```
"Follow the style guide in docs/CODE_STYLE.md when creating this function"
```

### Method 3: Git Repository Template

Create template repositories with the style guides baked in.

```bash
# Create template
git init my-fp-template
cd my-fp-template
mkdir -p docs
cp typescript-fp-style-guide.md docs/CODE_STYLE.md

# Add to git
git add .
git commit -m "Add FP style guide"

# Use as template
git clone my-fp-template new-project
```

---

## Using with Claude Code

### Basic Workflow

1. **Start Claude Code** in your project directory:
```bash
claude-code
```

2. **Reference the style guide** in your prompts:
```
"Create a function to fetch user data from Supabase following the FP style guide"
```

3. **Claude Code will**:
   - Read the relevant skill automatically (if set up as Method 1)
   - Generate code using `TaskEither` or `Effect`
   - Follow immutability patterns
   - Use composition over imperative code

### Example Prompts

**Python:**
```
"Create a training loop for MLX that uses Result types for error handling and composes pure functions"
```

**TypeScript:**
```
"Build a Next.js server action that fetches and validates user input using TaskEither and railway-oriented programming"
```

### Project-Specific Instructions

Add a `.claude-instructions` file to your project root:

```markdown
# .claude-instructions

## Code Style
- Follow functional programming patterns from docs/CODE_STYLE.md
- Use Effect for all async operations
- No try/catch blocks - use TaskEither
- All data structures must be immutable (readonly)

## Libraries
- Use fp-ts for core FP utilities
- Use Effect for async/error handling
- Use monocle-ts for deep object updates
```

---

## Project Setup

### Python MLX Project

**1. Install dependencies:**
```bash
pip install returns toolz mlx --break-system-packages
pip install mypy --break-system-packages
```

**2. Configure mypy:**
```toml
# pyproject.toml
[tool.mypy]
strict = true
warn_return_any = true
warn_unused_configs = true
disallow_untyped_defs = true
```

**3. Create project structure:**
```
project/
├── docs/
│   └── CODE_STYLE.md
├── src/
│   ├── types/          # ADTs, type aliases
│   ├── pure/           # Pure business logic
│   ├── io/             # IO operations
│   └── pipeline/       # Composed pipelines
├── tests/
└── pyproject.toml
```

**4. Import pattern:**
```python
# src/__init__.py
from returns.result import Result, Success, Failure
from returns.pipeline import flow, pipe
from returns.curry import curry
```

### TypeScript Next.js Project

**1. Install dependencies:**
```bash
npm install effect
npm install -D @types/node typescript
```

**2. Configure TypeScript:**
```json
// tsconfig.json
{
  "compilerOptions": {
    "strict": true,
    "strictNullChecks": true,
    "noUncheckedIndexedAccess": true,
    "noImplicitAny": true,
    "exactOptionalPropertyTypes": true,
    "moduleResolution": "bundler"
  }
}
```

**3. Configure ESLint:**
```bash
npm install -D eslint-plugin-functional
```

```json
// .eslintrc.json
{
  "plugins": ["functional"],
  "rules": {
    "@typescript-eslint/no-explicit-any": "error",
    "@typescript-eslint/explicit-function-return-type": "warn",
    "functional/immutable-data": "error",
    "functional/no-let": "warn",
    "functional/no-loop-statements": "warn"
  }
}
```

**4. Create project structure:**
```
src/
├── types/              # ADTs, domain types
│   ├── errors.ts
│   ├── domain.ts
│   └── api.ts
├── lib/
│   ├── fp/            # FP utilities
│   │   ├── result.ts
│   │   ├── validation.ts
│   │   └── compose.ts
│   ├── db/            # Database operations
│   │   ├── users.ts
│   │   └── posts.ts
│   └── api/           # API clients
├── app/
│   ├── api/           # Next.js API routes
│   └── actions/       # Server actions
└── inngest/           # Inngest functions
```

**5. Import pattern:**
```typescript
// lib/fp/index.ts
export { Effect, pipe } from 'effect'
export * as E from 'fp-ts/Either'
export * as TE from 'fp-ts/TaskEither'
export * as O from 'fp-ts/Option'
```

---

## Key Differences Between Guides

### Python (MLX Project)

| Aspect | Approach |
|--------|----------|
| **Library** | `returns` for monads, Result types |
| **Style** | Haskell-inspired but Python-idiomatic |
| **Type System** | Leverage mypy's strict mode |
| **Error Handling** | `Result[T, E]` with `Success`/`Failure` |
| **Async** | `IO` and `IOResult` for side effects |
| **Focus** | Type safety, explicit effects, ML pipelines |
| **Best For** | Data transformations, training loops, pure computation |

**Example Pattern:**
```python
from returns.result import Result, Success, Failure
from returns.pipeline import pipe

def process(x: int) -> Result[int, str]:
    return pipe(
        validate(x),
        lambda r: r.bind(transform),
        lambda r: r.map(finalize)
    )
```

### TypeScript (Next.js/Supabase/Inngest)

| Aspect | Approach |
|--------|----------|
| **Library** | `fp-ts` or `Effect` (Effect recommended) |
| **Style** | Very close to Haskell with TS types |
| **Type System** | Discriminated unions, HKT simulation |
| **Error Handling** | `TaskEither<E, A>` or `Effect<A, E>` |
| **Async** | Built into TaskEither/Effect |
| **Focus** | Railway-oriented programming, composable async |
| **Best For** | API routes, server actions, event handlers |

**Example Pattern:**
```typescript
import { Effect, pipe } from 'effect'

const process = (x: number): Effect.Effect<number, string> =>
  pipe(
    validate(x),
    Effect.flatMap(transform),
    Effect.map(finalize)
  )
```

---

## Quick Start Examples

### Python: Simple Pipeline

```python
from returns.result import Result, Success, Failure
from returns.pipeline import flow

def validate_positive(x: float) -> Result[float, str]:
    return Success(x) if x > 0 else Failure("Must be positive")

def safe_sqrt(x: float) -> Result[float, str]:
    return Success(x ** 0.5)

def format_result(x: float) -> str:
    return f"Result: {x:.2f}"

# Compose the pipeline
pipeline = flow(
    validate_positive,
    lambda r: r.bind(safe_sqrt),
    lambda r: r.map(format_result)
)

# Use it
result = pipeline(16.0)  # Success("Result: 4.00")
```

### TypeScript: Supabase Query

```typescript
import { Effect, pipe } from 'effect'

type DbError = { readonly _tag: 'QueryError'; readonly message: string }

const fetchUser = (id: string): Effect.Effect<User, DbError> =>
  Effect.tryPromise({
    try: async () => {
      const { data, error } = await supabase
        .from('users')
        .select('*')
        .eq('id', id)
        .single()
      
      if (error) throw error
      return data as User
    },
    catch: (e): DbError => ({
      _tag: 'QueryError',
      message: String(e)
    })
  })

// Use it
const program = pipe(
  fetchUser('123'),
  Effect.map(user => user.name),
  Effect.catchAll(error => Effect.succeed('Unknown'))
)

const name = await Effect.runPromise(program)
```

---

## Best Practices

### 1. Start Small
- Convert one module at a time
- Begin with pure utility functions
- Gradually add monadic error handling

### 2. Layer Your Application

**Python:**
```
Pure Core (pure functions, no IO)
    ↓
Result Layer (error handling)
    ↓
IO Layer (side effects)
    ↓
Application Entry Points
```

**TypeScript:**
```
Domain Logic (pure functions)
    ↓
TaskEither/Effect Layer (async + errors)
    ↓
API/Server Layer (HTTP, DB)
    ↓
Route Handlers/Actions
```

### 3. Use Type-Driven Development
1. Define types first (ADTs, errors)
2. Write function signatures
3. Implement with compiler guidance
4. Let types guide refactoring

### 4. Prefer Composition Over Abstraction
```typescript
// ❌ Too abstract
const genericDataProcessor = <A, B, C>(/* ... */) => { /* ... */ }

// ✅ Compose small functions
const processUser = flow(
  validateUser,
  enrichWithProfile,
  saveToDb
)
```

### 5. Handle Errors at Boundaries
```typescript
// Business logic returns Effect/Result
const processData = (x: number): Effect.Effect<Result, Error> => { /* ... */ }

// API route handles the Effect
export async function POST(req: Request) {
  const result = await Effect.runPromise(
    processData(data).pipe(
      Effect.catchAll(handleError)
    )
  )
  return Response.json(result)
}
```

### 6. Document Effect Signatures Clearly
```python
def load_model(path: str) -> Result[Model, str]:
    """
    Load a model from disk.
    
    Pure computation, no side effects.
    
    Returns:
        Success(Model): Model loaded successfully
        Failure(str): File not found or parse error
    """
```

### 7. Use Point-Free Style Judiciously
```typescript
// ✅ Clear
const processUsers = (users: User[]) =>
  pipe(
    users,
    A.filter(isActive),
    A.map(formatUser)
  )

// 🤔 Too clever?
const processUsers = flow(
  A.filter(isActive),
  A.map(formatUser)
)
```

---

## Troubleshooting

### Python

**Issue: `returns` not found**
```bash
pip install returns --break-system-packages
```

**Issue: Mypy errors with returns**
```toml
# pyproject.toml
[tool.mypy]
plugins = ["returns.contrib.mypy.returns_plugin"]
```

**Issue: Type inference fails**
```python
# Be explicit with generic types
result: Result[int, str] = Success(42)
```

### TypeScript

**Issue: fp-ts types too complex**
- Consider switching to `Effect` for better DX
- Use type aliases to simplify:
```typescript
type AsyncResult<A, E = Error> = TE.TaskEither<E, A>
```

**Issue: Cannot use `TaskEither` with Next.js**
```typescript
// Convert to Promise at boundaries
const result = await fetchUser('123')()

// Or use Effect's runPromise
const result = await Effect.runPromise(program)
```

**Issue: ESLint complains about mutations**
```json
// Disable for specific lines
{
  "rules": {
    "functional/immutable-data": ["error", {
      "ignorePattern": "^draft"  // Allow Immer drafts
    }]
  }
}
```

### Claude Code

**Issue: Claude Code not following style guide**

1. Verify skill is in correct location:
```bash
cat /mnt/skills/user/typescript-fp/SKILL.md
```

2. Be explicit in prompts:
```
"Use the TypeScript FP style guide when creating this function.
Specifically, use Effect and railway-oriented programming."
```

3. Reference specific sections:
```
"Follow section 6 of the style guide for error handling"
```

**Issue: Code mixes FP and imperative styles**

Add to project `.claude-instructions`:
```markdown
CRITICAL: All code must be purely functional
- No try/catch blocks
- No let/var, only const
- No mutations
- All async operations use TaskEither or Effect
```

---

## Next Steps

1. **Set up skills**: Copy guides to `/mnt/skills/user/`
2. **Configure project**: Set up TypeScript/Python configs
3. **Start small**: Convert one module to FP style
4. **Iterate**: Gradually expand FP patterns
5. **Measure**: Track code quality improvements

---

## Additional Resources

### Python
- **returns**: https://returns.readthedocs.io/
- **toolz**: https://toolz.readthedocs.io/
- **mypy**: https://mypy.readthedocs.io/

### TypeScript
- **fp-ts**: https://gcanti.github.io/fp-ts/
- **Effect**: https://effect.website/
- **monocle-ts**: https://github.com/gcanti/monocle-ts

### Learning
- **Railway-Oriented Programming**: https://fsharpforfunandprofit.com/rop/
- **Domain Modeling Made Functional**: Book by Scott Wlaschin
- **Haskell Programming from First Principles**: For FP fundamentals

---

## Support

If you encounter issues:
1. Check this guide's troubleshooting section
2. Review the relevant style guide (Python or TypeScript)
3. Verify library versions match recommendations
4. Test with minimal examples before full integration

Remember: Functional programming is a journey, not a destination. Start simple, learn the patterns, and gradually build up your codebase.
