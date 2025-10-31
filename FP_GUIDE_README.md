# Functional Programming Guides Collection

Complete set of Haskell-inspired functional programming guides for TypeScript and Python projects.

---

## 📚 Guide Overview

### 1. Python FP Style Guide
**File:** `python-fp-style-guide.md`

Comprehensive functional programming style guide for Python MLX and ML projects.

**Covers:**
- Error handling with Result/Either types
- Monadic composition (do-notation style)
- Currying and partial application
- Function composition and pipelines
- Immutable data structures
- Railway-oriented programming
- IO and side effect management
- Higher-order functions
- Type-level programming with Protocols

**Libraries:** `returns`, `toolz`, `mypy`

---

### 2. TypeScript FP Style Guide
**File:** `typescript-fp-style-guide.md`

Comprehensive functional programming style guide for TypeScript Next.js, Supabase, and Inngest projects.

**Covers:**
- Error handling with Either/TaskEither
- Monadic composition with fp-ts or Effect
- Currying and function composition
- Immutable data with readonly
- Railway-oriented programming for APIs
- Next.js server actions with Effect
- Inngest functions with TaskEither
- Higher-kinded type simulation
- Pattern matching with discriminated unions

**Libraries:** `fp-ts` or `Effect`, `monocle-ts`, `io-ts`

---

### 3. Swift FP Style Guide
**File:** `swift-fp-style-guide.md`

Comprehensive functional programming style guide for Swift iOS, macOS, and SwiftUI projects.

**Covers:**
- Error handling with Result type
- Monadic composition with flatMap
- Immutable data with structs and let
- Function composition with custom operators
- Railway-oriented programming
- Async/await with Result
- Pattern matching with switch
- Optics (Lenses and Prisms)
- SwiftUI integration

**Libraries:** Built-in Swift features, optional: `Bow`, `Composable Architecture`

---

### 4. Kotlin FP Style Guide
**File:** `kotlin-fp-style-guide.md`

Comprehensive functional programming style guide for Kotlin Android, Backend (Ktor), and Multiplatform projects.

**Covers:**
- Error handling with Either/Result
- Monadic composition with flatMap
- Immutable data classes
- Function composition with operators
- Railway-oriented programming
- Suspending functions with Either
- Sealed classes for ADTs
- Arrow Optics for lenses
- Jetpack Compose integration

**Libraries:** `Arrow` (core, fx-coroutines, optics), `kotlinx-coroutines`

---

### 5. How to Use FP Style Guides
**File:** `how-to-use-fp-style-guides.md`

Complete setup and integration guide for using the FP style guides with Claude Code and VS Code.

**Covers:**
- Quick setup for Python and TypeScript
- Integration methods (custom skills, project docs, templates)
- Using with VS Code Claude Code extension
- Project setup and directory structure
- Best practices for FP development
- Troubleshooting common issues
- Tooling configuration

**Essential reading** for setting up your development environment.

---

### 6. Automatic Type & ADT Generation Guide
**File:** `automatic-type-generation-guide.md`

Shows Claude how to automatically generate types and ADTs for any data flow scenario.

**Covers:**
- Polars DataFrame operations → Schema types, Error ADTs, Result wrappers
- MLX array operations → Shape types, Training state, Model errors
- Supabase queries → Database types, Domain types, TaskEither operations
- React UI components → RemoteData ADT, UI state, Form validation
- Cross-system data flows → End-to-end type safety
- Pattern recognition rules for automatic inference

**What Claude Auto-Generates:**
- Domain types for data structure
- State ADTs for operation states
- Error types for failure modes
- Validation types for boundaries
- Transformation types for processing
- Result/Either/TaskEither wrappers

---

### 7. Automatic Chain Generation Guide ⭐⭐⭐
**File:** `automatic-chain-generation-guide.md`

**⭐ MOST IMPORTANT:** Shows how Claude automatically generates complete, error-checked function chains with **identical patterns across Python, TypeScript, Swift, and Kotlin**. Developers don't need to understand monads - they just follow the pattern.

**Covers:**
- Universal pipeline pattern (same across all languages)
- Automatic chain generation for any task
- Pattern library (fetch-validate-save, ETL, parallel operations)
- Maintenance guide for non-FP developers
- Real-world examples (API endpoints, data processing, ML training)
- Quick reference card

**What This Enables:**
- Consistent code across all projects and languages
- Maintainable by developers who don't know FP
- Automatic error handling
- Same mental model (factory assembly line)
- Copy-paste patterns

**Example:** User says "fetch user, validate age, update profile" → Claude generates complete pipeline with types, error handling, and composition in Python, TypeScript, Swift, or Kotlin.

---

### 8. Traversable and Foldable Guide
**File:** `traversable-foldable-guide.md`

Deep dive into implementing Haskell's Traversable and Foldable typeclasses in TypeScript and Python.

**Covers:**
- Haskell typeclass refresher
- Type system comparison and HKT challenges
- Complete Foldable implementations
- Complete Traversable implementations
- Practical examples (validation, APIs, ETL)
- Limitations and workarounds
- Real-world usage patterns
- Library support comparison

**Use cases:**
- Collection validation with early exit
- Parallel API calls with error handling
- Batch database operations
- ETL pipelines
- Form validation

---

## 🚀 Quick Start

### For VS Code Claude Code Extension Users

1. **Download all guides** from this conversation

2. **Add to your projects:**
   ```bash
   # Python MLX Project
   cd your-mlx-project
   mkdir -p docs
   cp python-fp-style-guide.md docs/CODE_STYLE.md
   
   # TypeScript Next.js Project
   cd your-nextjs-project
   mkdir -p docs
   cp typescript-fp-style-guide.md docs/CODE_STYLE.md
   ```

3. **Reference in Claude Code:**
   ```
   @docs/CODE_STYLE.md create a user service with proper error handling
   ```

### For Claude.ai Web Interface Users

The guides are already installed as custom skills in this interface at:
- `/mnt/skills/user/python-fp/SKILL.md`
- `/mnt/skills/user/typescript-fp/SKILL.md`

Just start coding and Claude will automatically apply these patterns!

---

## 📖 Recommended Reading Order

1. **Start here:** `how-to-use-fp-style-guides.md` - Setup and integration
2. **Most important:** `automatic-chain-generation-guide.md` ⭐ - How Claude generates code
3. **Also essential:** `automatic-type-generation-guide.md` - Type generation patterns
4. **Language reference (choose your stack):**
   - Python: `python-fp-style-guide.md`
   - TypeScript: `typescript-fp-style-guide.md`
   - Swift: `swift-fp-style-guide.md`
   - Kotlin: `kotlin-fp-style-guide.md`
5. **Advanced (optional):** `traversable-foldable-guide.md` - Advanced typeclasses

---

## 🎯 What These Guides Enable

### Automatic Code Generation

**You say:** "Fetch user from database, validate age, update profile"

**Claude generates complete pipeline in your language:**

**Python:**
```python
def process_user(user_id: str) -> Result[dict, UserError]:
    return (
        fetch_user(user_id)      # Get from DB
        .bind(validate_age)       # Check age >= 18
        .bind(update_profile)     # Update verified status
        .map(format_response)     # Format output
    )
```

**TypeScript:**
```typescript
const processUser = (userId: string): TE.TaskEither<UserError, object> =>
  pipe(
    fetchUser(userId),           // Get from DB
    TE.flatMap(validateAge),     // Check age >= 18
    TE.flatMap(updateProfile),   // Update verified status
    TE.map(formatResponse)       // Format output
  )
```

**Swift:**
```swift
func processUser(_ userId: String) -> Result<[String: Any], UserError> {
    fetchUser(userId)
        .flatMap(validateAge)
        .flatMap(updateProfile)
        .map(formatResponse)
}
```

**Kotlin:**
```kotlin
fun processUser(userId: String): Either<UserError, Map<String, Any>> =
    fetchUser(userId)
        .flatMap { validateAge(it) }
        .flatMap { updateProfile(it) }
        .map { formatResponse(it) }
```

**Same pattern. Same mental model. All languages.**

### Code Transformation

**Before (Imperative):**
```typescript
async function fetchUser(id: string): Promise<User> {
  try {
    const response = await fetch(`/api/users/${id}`)
    if (!response.ok) throw new Error('Failed')
    return await response.json()
  } catch (error) {
    throw error
  }
}
```

**After (Functional):**
```typescript
const fetchUser = (id: string): TE.TaskEither<FetchError, User> =>
  pipe(
    TE.tryCatch(
      () => fetch(`/api/users/${id}`),
      (error): FetchError => ({ _tag: 'NetworkError', message: String(error) })
    ),
    TE.filterOrElse(
      (response) => response.ok,
      (response): FetchError => ({ _tag: 'NotFound' })
    ),
    TE.flatMap((response) =>
      TE.tryCatch(
        () => response.json(),
        (error): FetchError => ({ _tag: 'ParseError', message: String(error) })
      )
    )
  )
```

### Key Benefits

✅ **No more try/catch** - Use Result/Either types
✅ **Composable error handling** - Railway-oriented programming
✅ **Type-safe** - Compiler catches errors
✅ **Immutable** - No accidental mutations
✅ **Testable** - Pure functions are easy to test
✅ **Refactorable** - Small, composable functions
✅ **Consistent across stacks** - Same patterns in Python, TypeScript, Swift, and Kotlin
✅ **Maintainable by anyone** - Developers don't need to understand monads
✅ **Cross-platform** - Works for web, mobile, backend, and ML

**Mental Model for Your Team:**
> "It's like a factory assembly line. Each station does one thing. If any station fails, the line stops. If all succeed, you get the final product."

No category theory required. Just follow the pattern.

---

## 🛠️ Technology Stack

### Python (MLX Projects)
```python
# Core FP
from returns.result import Result, Success, Failure
from returns.pipeline import flow, pipe
from returns.curry import curry
from toolz import compose

# Type checking
mypy --strict
```

### TypeScript (Next.js/Supabase/Inngest)
```typescript
// Option 1: fp-ts (Haskell-like)
import * as E from 'fp-ts/Either'
import * as TE from 'fp-ts/TaskEither'
import { pipe } from 'fp-ts/function'

// Option 2: Effect (Ergonomic)
import { Effect, pipe } from 'effect'
```

### Swift (iOS/macOS/SwiftUI)
```swift
// Built-in Result type
func divide(_ a: Double, by b: Double) -> Result<Double, Error>

// Optional: Advanced FP
import Bow                    // FP library
import ComposableArchitecture // App architecture
```

### Kotlin (Android/Backend/Multiplatform)
```kotlin
// Arrow for FP
import arrow.core.Either
import arrow.core.raise.either
import arrow.fx.coroutines.*

// Data classes + sealed classes for ADTs
```

---

## 📋 Style Rules Summary

### Universal Rules (Both Languages)

1. **No naked exceptions** - Wrap in Result/Either
2. **No mutations** - Use immutable data structures
3. **Explicit effects** - Mark IO and side effects
4. **Compose functions** - Build complexity from small pieces
5. **Curry for reuse** - Enable partial application
6. **Type everything** - Leverage type systems
7. **Pattern match** - Use exhaustive matching for ADTs
8. **Small functions** - One responsibility per function
9. **Railway-oriented** - Chain operations, handle errors at boundaries
10. **Document purity** - Make pure vs impure clear

---

## 🔧 Tooling Recommendations

### Python
```toml
# pyproject.toml
[tool.mypy]
strict = true

[tool.ruff]
select = ["E", "F", "I", "N", "UP", "RUF"]
```

### TypeScript
```json
{
  "compilerOptions": {
    "strict": true,
    "noUncheckedIndexedAccess": true
  }
}
```

```json
{
  "plugins": ["functional"],
  "rules": {
    "functional/immutable-data": "error",
    "functional/no-let": "warn"
  }
}
```

---

## 🎓 Learning Path

### Beginner
1. Read "How to Use" guide
2. Set up tooling
3. Convert one module to FP style
4. Focus on Result/Either types

### Intermediate
1. Learn function composition
2. Master pipe and flow
3. Implement currying patterns
4. Build complete pipelines

### Advanced
1. Study Traversable and Foldable
2. Create custom type classes
3. Implement monad transformers
4. Design pure core architectures

---

## 📚 Additional Resources

### Haskell Learning
- **Learn You a Haskell** - Free online book
- **Haskell Programming from First Principles** - Comprehensive

### Functional Programming
- **Domain Modeling Made Functional** - Scott Wlaschin
- **Railway-Oriented Programming** - fsharpforfunandprofit.com

### Libraries
- **fp-ts docs:** https://gcanti.github.io/fp-ts/
- **Effect docs:** https://effect.website/
- **returns docs:** https://returns.readthedocs.io/
- **toolz docs:** https://toolz.readthedocs.io/

---

## 💡 Tips for Success

1. **Start small** - Convert utilities first
2. **Be consistent** - Apply patterns uniformly
3. **Team buy-in** - Share guides with your team
4. **Iterate** - Refine patterns as you learn
5. **Measure** - Track bug reduction and testability
6. **Document** - Explain why FP benefits your project

---

## 🤝 Using with Claude Code

When working with Claude Code (VS Code extension), reference the guides:

```
Following the FP style guide in docs/CODE_STYLE.md, create a user 
service that fetches data from Supabase using TaskEither for error 
handling and railway-oriented programming patterns.
```

Claude Code will:
- Read the style guide
- Apply functional patterns
- Use recommended libraries
- Follow immutability rules
- Generate type-safe code

---

## 📥 Download Links

All guides are available in the `/mnt/user-data/outputs/` directory:

1. `python-fp-style-guide.md`
2. `typescript-fp-style-guide.md`
3. `how-to-use-fp-style-guides.md`
4. `traversable-foldable-guide.md`

---

## 📞 Support

If you encounter issues:
1. Check the "How to Use" troubleshooting section
2. Review language-specific style guide
3. Consult Traversable/Foldable guide for advanced patterns
4. Test with minimal examples first

---

**Happy Functional Programming! 🎉**

Transform your codebase into a type-safe, composable, and maintainable system using Haskell-inspired patterns.
