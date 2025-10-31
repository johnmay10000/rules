# CURSOR.md - Global Rules for All Projects

**Version**: 1.0.0  
**Last Updated**: 2025-10-31  
**Status**: Active  
**Scope**: Universal rules for ALL projects and tech stacks

---

## Overview

This document defines **mandatory** and **recommended** rules that apply to ALL projects using Cursor, regardless of technology stack. These rules ensure:

- ✅ Consistent workflow across projects
- ✅ Clear documentation and progress tracking
- ✅ High code quality and maintainability
- ✅ Type safety and error handling
- ✅ Easy onboarding for new team members

**Important**: This document contains:
- **MANDATORY** rules (must follow)
- **RECOMMENDED** patterns (strongly encouraged)
- References to language-specific guides (Python, TypeScript, Swift, Kotlin)
- References to platform-specific guides (GCP, AWS)

---

## Table of Contents

### MANDATORY RULES
1. [Git Workflow](#1-git-workflow-mandatory)
2. [Documentation Structure](#2-documentation-structure-mandatory)
3. [Testing Requirements](#3-testing-requirements-mandatory)
4. [File Size Limits](#4-file-size-limits-mandatory)

### RECOMMENDED PATTERNS
5. [Functional Programming Principles](#5-functional-programming-principles-recommended)
6. [Code Organization](#6-code-organization-recommended)
7. [Type Safety](#7-type-safety-recommended)

### INTEGRATION
8. [Cursor Integration](#8-cursor-integration)
9. [Language-Specific Rules](#9-language-specific-rules)
10. [Platform-Specific Rules](#10-platform-specific-rules)

---

## 1. GIT WORKFLOW (MANDATORY)

### 1.1 Git Checkpoints - When to Commit

**CRITICAL**: Git commits are MANDATORY at these checkpoints:

#### Checkpoint Triggers

✅ **Every 30-60 minutes** - Maximum time between commits during active work  
✅ **After completing a phase** - Any phase in multi-phase work  
✅ **After creating core documents** - Major deliverables  
✅ **After completing logical units** - Set of related files (templates, examples)  
✅ **Before context switch** - Switching between different work areas  
✅ **After bug fixes** - Any fix that resolves an issue  
✅ **After documentation updates** - Significant doc changes  
✅ **At end of work session** - Daily checkpoint before stopping

**Why This Matters**:
- Prevents work loss from crashes or errors
- Creates clear project history
- Enables easy progress review
- Allows safe reversion if needed
- Preserves context across sessions

### 1.2 Commit Message Format (MANDATORY)

**Required Structure**:

```
<Brief summary (50-72 chars)>

<Detailed description of changes>

<Rationale - why the change was made>

<Impact - what files/features affected>

<Context - relevant decisions or background>

Status: <Current state>

🤖 Generated with Cursor
Co-Authored-By: Claude <noreply@anthropic.com>
```

**All Elements Required**:
1. Brief summary line (50-72 characters)
2. Detailed description (bullet points preferred)
3. Rationale (why the change was needed)
4. Impact (what was affected)
5. Context (relevant decisions)
6. Status indicator (current state)
7. Co-author attribution

**Good Example**:

```
Complete Phase 0: Portability foundation ✅

Created all portability deliverables:
- SETUP_GUIDE.md (machine setup for all platforms)
- .cursorrules_smart_template_envvar (portable via env var)
- .cursorrules_smart_template_submodule (self-contained)
- Updated FILE_LOCATIONS_USER_GUIDE.md

Rationale: User requirement for self-contained, portable,
deterministic setup across machines.

Impact: All future deliverables will use portable paths.
No rework needed.

Status: Phase 0 complete ✅

🤖 Generated with Cursor
Co-Authored-By: Claude <noreply@anthropic.com>
```

**Bad Examples**:

```
# TOO VAGUE
Update files

# NO CONTEXT
WIP

# MULTIPLE UNRELATED (should be 3 separate commits)
Create CURSOR.md, fix typos, update README
```

---

## 2. DOCUMENTATION STRUCTURE (MANDATORY)

### 2.1 Three-Tier Documentation Hierarchy

**CRITICAL**: All projects MUST follow this structure.

```
project-root/
├── ARCHITECTURE_PLAN.md          # Tier 1: Strategic
├── docs/
│   ├── plans/                     # Tier 2: Tactical
│   │   ├── FEATURE_X_PLAN.md
│   │   └── FEATURE_X_TODO.md     # Paired with plan
│   └── YYYY_MM_DD/                # Tier 3: Execution
│       └── YYYYMMDD_HHMM_*.md
```

---

#### Tier 1: Strategic - ARCHITECTURE_PLAN.md

**Location**: Project root  
**Purpose**: High-level architecture and major components  
**Updated**: Rarely (major changes only)  
**Format**: `ARCHITECTURE_PLAN.md` (no timestamp)

**Required Sections**:
- System architecture overview
- Major components and their interactions
- Technology stack with rationale
- Links to Tier 2 tactical plans
- High-level success criteria

**Template**:
```markdown
# Project Architecture Plan

## Overview
[High-level system description]

## Major Components
- Component 1: [Purpose and technology]
- Component 2: [Purpose and technology]

## Technology Stack
- Language: [X] because [rationale]
- Platform: [Y] because [rationale]
- Database: [Z] because [rationale]

## Sub-Plans
- [Feature A Plan](docs/plans/FEATURE_A_PLAN.md)
- [Feature B Plan](docs/plans/FEATURE_B_PLAN.md)

## Success Criteria
- [Measurable criterion 1]
- [Measurable criterion 2]
```

---

#### Tier 2: Tactical - docs/plans/*.md

**Location**: `docs/plans/`  
**Purpose**: Feature-specific plans with implementation details  
**Updated**: Regularly as work progresses  
**Format**: `FEATURE_NAME_PLAN.md` + `FEATURE_NAME_TODO.md` (paired)

**When Required**:
- New features (3+ hours estimated)
- Architectural changes or refactoring
- Bug fixes requiring investigation
- System integrations
- Multi-step implementations

**Plan Document Template**:
```markdown
# Feature Name Plan

**Status**: ⏳ IN PROGRESS / ✅ COMPLETE  
**Last Updated**: YYYY-MM-DD HH:MM  
**Estimated Time**: X hours

## Overview
What needs to be done and why

## Phases
- [ ] Phase 1: [Description] (X hours)
- [ ] Phase 2: [Description] (Y hours)
- [ ] Phase 3: [Description] (Z hours)

## Implementation Details
- **Code Locations**: Where files will be created/modified
- **ADT Structures**: What types will be defined
- **Key Functions**: Main entry points and APIs

## Success Criteria
How to verify completion

## Update History
### YYYY-MM-DD HH:MM
- Completed Phase 1
- Started Phase 2
- [Any decisions or issues]
```

**Paired TODO List Template**:
```markdown
# Feature Name TODO

**Plan Reference**: [FEATURE_NAME_PLAN.md](FEATURE_NAME_PLAN.md)  
**Status**: ⏳ IN PROGRESS  
**Last Updated**: YYYY-MM-DD HH:MM  
**Progress**: X/Y tasks (Z%)

## Phase 1: [Phase Name]

**Status**: ⏳ IN PROGRESS  
**Progress**: X/Y tasks complete

- [x] Task 1.1: [Description] (Est: 2h, Actual: 1.5h)
- [x] Task 1.2: [Description] (Est: 1h, Actual: 1.2h)
- [ ] Task 1.3: [Description] (Est: 3h, Actual: -)
- [ ] Task 1.4: [Description] (Est: 1h, Actual: -)

**Phase Total**: Est 7h | Actual 2.7h | Remaining ~4.3h

## Overall Progress

**Total Tasks**: X / Y (Z%)  
**Total Time**: Est 10h | Actual 2.7h | Remaining ~7.3h

## Update History

### YYYY-MM-DD HH:MM
- Completed tasks 1.1, 1.2
- Started task 1.3
```

**Cursor's Responsibility**:
- Automatically update TODO lists as tasks complete
- Mark tasks with [x]
- Add actual time spent
- Update progress percentages
- Add timestamped update history entries

---

#### Tier 3: Execution - docs/YYYY_MM_DD/*.md

**Location**: `docs/2025_10_31/` (dated folders)  
**Purpose**: Daily work logs, decisions, summaries  
**Updated**: Multiple times per day  
**Format**: `YYYYMMDD_HHMM_DESCRIPTIVE_NAME.md` (timestamped)

**Filename Format (STRICT)**:
```
YYYYMMDD_HHMM_DESCRIPTIVE_NAME.md
```

**NO separators in date/time!**

**Examples**:
- ✅ `20251031_0900_CURSOR_MD_IMPLEMENTATION.md`
- ✅ `20251031_1530_BUG_FIX_VALIDATION.md`
- ✅ `20251031_2100_SESSION_SUMMARY.md`
- ❌ `2025_10_31_SUMMARY.md` (has separators)
- ❌ `SUMMARY.md` (no timestamp)
- ❌ `20251031_SUMMARY.md` (no time)

**Why This Format**:
- Files sort chronologically automatically
- Exact creation date AND time in filename
- No ambiguity about latest document
- Easy to find work from specific periods
- Prevents filename collisions

**Content Types**:
- Implementation notes
- Decision logs
- Bug investigations
- Session summaries
- Analysis documents
- Technical specifications

---

### 2.2 Cross-Referencing (MANDATORY)

**Documents must reference each other**:

**Tier 1 → Tier 2**: Link to all sub-plans  
**Tier 2 → Tier 1**: Reference architecture  
**Tier 2 → Tier 3**: Link to daily work  
**Tier 3 → Tier 2**: Reference active plan

**Example**:
```markdown
# In ARCHITECTURE_PLAN.md
## Sub-Plans
- [Portability Setup](docs/plans/PORTABILITY_PLAN.md)

# In docs/plans/PORTABILITY_PLAN.md
**Architecture**: [ARCHITECTURE_PLAN.md](../../ARCHITECTURE_PLAN.md)
**Daily Work**: [docs/2025_10_31/](../2025_10_31/)

# In docs/2025_10_31/20251031_0900_WORK.md
**Plan**: [Portability Plan](../plans/PORTABILITY_PLAN.md)
```

---

## 3. TESTING REQUIREMENTS (MANDATORY)

### 3.1 Core Rule

**CRITICAL**: ALL production code MUST have comprehensive tests that pass before committing.

### 3.2 "Comprehensive" Defined

**Four Test Categories Required**:

1. ✅ **Happy Path** - Normal expected behavior
   - Valid inputs → correct outputs
   - Main flow executes successfully
   - Example: User registration with valid data succeeds

2. ✅ **Error Cases** - All failure modes
   - Invalid inputs handled correctly
   - Expected errors return proper error types
   - Example: Registration with invalid email returns ValidationError

3. ✅ **Edge Cases** - Boundary conditions
   - Empty inputs ([], "", null, undefined)
   - Maximum/minimum values
   - Large inputs
   - Example: Name with 1 char or 1000 chars

4. ✅ **Regression Tests** - Past bugs stay fixed
   - Test for each previously fixed bug
   - Ensures fixes don't regress
   - Example: Test for bug #123 stays in suite

### 3.3 Coverage Targets

**Minimum Coverage**:

| Code Type | Tests | Coverage |
|-----------|-------|----------|
| Functions with business logic | 3+ | One per category |
| Business logic modules | 80%+ | All branches |
| Utility functions | 2+ | Happy + error |
| Integration points | 100% | All error paths |

**Business Logic** = Code that implements domain rules, calculations, validations

### 3.4 Special Requirements

**FP Migration** (CRITICAL):
- When migrating code to functional style
- Old and new code must have **equivalent** test coverage
- Cannot remove tests during migration
- Must add tests for new error handling paths

**Test Quality**:
- ✅ No test skipping without documented reason
- ✅ All tests must pass before commit
- ✅ Tests must be deterministic (repeatable)
- ✅ No flaky tests (random failures)

### 3.5 Language-Specific Tools

**Python**: `pytest`, run with `uv run pytest`  
**TypeScript**: `jest` or `vitest`  
**Swift**: XCTest  
**Kotlin**: JUnit + Kotest

See language-specific guides for detailed setup.

---

## 4. FILE SIZE LIMITS (MANDATORY)

### 4.1 Universal Rule

**CRITICAL**: ALL code files MUST be 250-300 lines maximum.

**Target**: 250-300 lines  
**Absolute Maximum**: 350 lines (with justification)

**Applies To**:
- Python (`.py`)
- TypeScript (`.ts`, `.tsx`)
- Swift (`.swift`)
- Kotlin (`.kt`)
- All other code files

### 4.2 How to Comply

**When file exceeds 300 lines**:

1. ✅ Split into multiple files
2. ✅ Create folder for module
3. ✅ Organize by feature/responsibility
4. ❌ Do NOT modify functionality to reduce size

**Good Split**:
```
# Before (400 lines)
user_service.py

# After (organized)
user_service/
├── user_repository.py     # 150 lines - DB operations
├── user_validator.py      # 120 lines - Validation logic
├── user_transformer.py    # 130 lines - Data transformation
└── __init__.py            # Exports
```

### 4.3 Acceptable Exceptions

**With Documentation**:
- Generated code (mark as generated)
- Single complex algorithm (rare)
- Configuration files (if unavoidable)

**Must Document in File**:
```python
# WARNING: File exceeds 300 lines (currently 350)
# Reason: Complex ML training algorithm that can't be split
# without losing coherence.
# Approved by: [Name] on [Date]
# Review: Attempted split in commit abc123 was reverted
```

### 4.4 Rationale

**Why 250-300 lines**:
- Fits on ~2 screens (~120 lines per screen)
- Forces focused modules (single responsibility)
- Easier code review (understand quickly)
- Better testing (smaller = easier to test)
- Reduces merge conflicts
- Encourages composition

---

## 5. FUNCTIONAL PROGRAMMING PRINCIPLES (RECOMMENDED)

**User Decision**: Mandatory for new code, incremental migration for old code.

### 5.1 Core Principles

#### 1. Pure Functions

**Definition**: Same input → same output, no side effects

**Good**:
```python
def add(a: int, b: int) -> int:
    return a + b  # Pure - always same result
```

**Bad**:
```python
counter = 0
def increment() -> int:
    global counter
    counter += 1  # Impure - modifies external state
    return counter
```

**Benefits**: Easy to test, easy to reason about, safe to parallelize

---

#### 2. Immutable Data Structures

**Definition**: Data cannot be modified after creation

**Python**:
```python
from dataclasses import dataclass

@dataclass(frozen=True)  # frozen = immutable
class User:
    id: str
    name: str
    email: str
```

**TypeScript**:
```typescript
interface User {
  readonly id: string
  readonly name: string
  readonly email: string
}
```

**Benefits**: No accidental mutations, thread-safe, easier debugging

---

#### 3. Algebraic Data Types (ADTs)

**Definition**: Type-safe data modeling with exhaustive pattern matching

**Example - Result Type**:
```python
from dataclasses import dataclass
from typing import TypeVar, Generic

T = TypeVar('T')
E = TypeVar('E')

@dataclass(frozen=True)
class Success(Generic[T]):
    value: T

@dataclass(frozen=True)
class Failure(Generic[E]):
    error: E

Result = Success[T] | Failure[E]
```

**Pattern Matching**:
```python
match result:
    case Success(value):
        print(f"Success: {value}")
    case Failure(error):
        print(f"Error: {error}")
```

**Benefits**: Compiler catches missing cases, explicit error handling, type-safe

---

#### 4. Result/Either Types for Error Handling

**Definition**: Explicit error handling without exceptions

**Instead of**:
```python
def divide(a: float, b: float) -> float:
    if b == 0:
        raise ValueError("Division by zero")
    return a / b
```

**Use**:
```python
def divide(a: float, b: float) -> Result[float, str]:
    if b == 0:
        return Failure("Division by zero")
    return Success(a / b)
```

**Benefits**: Errors in type signature, forces error handling, no hidden exceptions

---

#### 5. Function Composition

**Definition**: Build complex functions from simple ones

**Universal Pattern** (works in ALL languages):

**Python**:
```python
result = (
    fetch_data(id)
    .bind(validate)
    .bind(transform)
    .map(format)
)
```

**TypeScript**:
```typescript
const result = pipe(
  fetchData(id),
  TE.flatMap(validate),
  TE.flatMap(transform),
  TE.map(format)
)
```

**Swift**:
```swift
let result = fetchData(id)
    .flatMap(validate)
    .flatMap(transform)
    .map(format)
```

**Kotlin**:
```kotlin
val result = fetchData(id)
    .flatMap { validate(it) }
    .flatMap { transform(it) }
    .map { format(it) }
```

**Mental Model**: "Factory assembly line"
- Each function = one station
- Each does one thing
- If any fails, line stops
- If all succeed, get final product

**No category theory needed!**

---

#### 6. Pattern Matching

**Definition**: Exhaustive case handling with compiler checks

**Python (3.10+)**:
```python
match user_type:
    case "admin":
        return AdminUser(...)
    case "regular":
        return RegularUser(...)
    case "guest":
        return GuestUser(...)
    case _:
        return Failure("Unknown user type")
```

**TypeScript** (discriminated unions):
```typescript
type Result<T, E> = 
  | { _tag: 'Success'; value: T }
  | { _tag: 'Failure'; error: E }

function handle<T, E>(result: Result<T, E>) {
  switch (result._tag) {
    case 'Success':
      return result.value
    case 'Failure':
      return result.error
  }
}
```

**Benefits**: Compiler ensures all cases handled, no `isinstance()` checks

---

#### 7. No Defaults/Fallbacks

**Definition**: Signal failures explicitly, don't hide them

**Bad**:
```python
def get_user(id: str) -> User:
    user = db.find(id)
    return user or DEFAULT_USER  # Hides failure!
```

**Good**:
```python
def get_user(id: str) -> Result[User, str]:
    user = db.find(id)
    if user is None:
        return Failure(f"User {id} not found")
    return Success(user)
```

**Rule**: Never use defaults for business logic, record failures

---

### 5.2 Universal FP Pattern

**Same pattern across ALL languages**:

| Language | Bind/FlatMap | Map | Syntax |
|----------|--------------|-----|--------|
| Python | `.bind()` | `.map()` | `result.bind(f).map(g)` |
| TypeScript | `TE.flatMap()` | `TE.map()` | `pipe(x, TE.flatMap(f), TE.map(g))` |
| Swift | `.flatMap()` | `.map()` | `result.flatMap(f).map(g)` |
| Kotlin | `.flatMap {}` | `.map {}` | `result.flatMap { f(it) }.map { g(it) }` |

**Mental Model**: Factory assembly line
- Each function is a station
- Each does one transformation
- Errors stop the line
- Success continues to next station

**Example** (same logic, different syntax):

**Task**: "Fetch user, validate age ≥18, update profile, format response"

**Python**:
```python
def process_user(user_id: str) -> Result[dict, UserError]:
    return (
        fetch_user(user_id)      # Station 1: Get from DB
        .bind(validate_age)       # Station 2: Check age ≥ 18
        .bind(update_profile)     # Station 3: Update verified
        .map(format_response)     # Station 4: Format output
    )
```

**TypeScript**:
```typescript
const processUser = (userId: string): TE.TaskEither<UserError, object> =>
  pipe(
    fetchUser(userId),           // Station 1
    TE.flatMap(validateAge),     // Station 2
    TE.flatMap(updateProfile),   // Station 3
    TE.map(formatResponse)       // Station 4
  )
```

**Same pattern, same logic, different language!**

---

## 6. CODE ORGANIZATION (RECOMMENDED)

### 6.1 Layered Architecture

**Four Layers** (bottom-up):

```
Entry Points (HTTP, CLI, Events)
    ↓
IO Layer (Database, API, Files)
    ↓
Result/Either Layer (Error Handling)
    ↓
Pure Core (Business Logic)
```

**Layer 1: Pure Core** (Domain Logic)
- Pure functions only
- No IO, no side effects
- Business rules
- Data transformations
- Easy to test

**Layer 2: Result Layer** (Error Handling)
- Wrap operations in Result/Either
- Explicit error types
- Pattern matching
- Railway-oriented programming

**Layer 3: IO Layer** (Side Effects)
- Database operations
- API calls
- File system access
- External services
- Returns Result types

**Layer 4: Entry Points** (Application)
- HTTP handlers
- CLI interfaces
- Event handlers
- Orchestration
- Handle Result types

---

### 6.2 Project Structure

**Backend**:
```
src/
├── types/              # ADTs, domain types, errors
│   ├── domain.ts
│   ├── errors.ts
│   └── result.ts
├── domain/             # Pure business logic (Layer 1)
│   ├── user_logic.ts
│   └── order_logic.ts
├── services/           # IO operations (Layer 3)
│   ├── user_service.ts
│   └── order_service.ts
├── api/                # HTTP handlers (Layer 4)
│   ├── users.ts
│   └── orders.ts
└── utils/              # Utility functions
    └── validation.ts
```

**Frontend**:
```
src/
├── types/              # ADTs, domain types
├── lib/
│   ├── fp/            # FP utilities (Result, pipe)
│   ├── api/           # API clients (Layer 3)
│   └── db/            # Database ops (Layer 3)
├── components/         # UI components
│   ├── UserProfile.tsx
│   └── OrderList.tsx
└── app/                # Next.js routes (Layer 4)
    ├── api/           # API routes
    └── page.tsx
```

---

## 7. TYPE SAFETY (RECOMMENDED)

### 7.1 Type-Driven Development

**Workflow**:

**1. Define Types First**
```python
@dataclass(frozen=True)
class User:
    id: str
    email: str
    age: int

@dataclass(frozen=True)
class ValidationError:
    field: str
    message: str
```

**2. Write Function Signatures**
```python
def validate_user(data: dict) -> Result[User, ValidationError]:
    ...
```

**3. Implement with Compiler Guidance**
```python
def validate_user(data: dict) -> Result[User, ValidationError]:
    if 'email' not in data:
        return Failure(ValidationError('email', 'Required'))
    if 'age' not in data:
        return Failure(ValidationError('age', 'Required'))
    if data['age'] < 0:
        return Failure(ValidationError('age', 'Must be positive'))
    
    return Success(User(
        id=data.get('id', generate_id()),
        email=data['email'],
        age=data['age']
    ))
```

**Benefits**: Types guide implementation, compiler catches errors, refactoring is safe

---

## 8. CURSOR INTEGRATION

### 8.1 Setup Methods

**Two Portable Approaches**:

#### Method 1: Environment Variable (Recommended)

**Setup** (one-time per machine):
```bash
# macOS/Linux
echo 'export CURSOR_RULES_PATH="$HOME/projects/rules"' >> ~/.zshrc
source ~/.zshrc

# Windows
[System.Environment]::SetEnvironmentVariable('CURSOR_RULES_PATH', "$HOME\projects\rules", 'User')
```

**In Project** (`.cursorrules`):
```markdown
@${CURSOR_RULES_PATH}/CURSOR.md
@${CURSOR_RULES_PATH}/python-fp-style-guide.md
```

#### Method 2: Git Submodule (Self-Contained)

**Setup** (one-time per project):
```bash
git submodule add <rules-repo-url> .cursor-rules
```

**In Project** (`.cursorrules`):
```markdown
@.cursor-rules/CURSOR.md
@.cursor-rules/python-fp-style-guide.md
```

See [SETUP_GUIDE.md](SETUP_GUIDE.md) for detailed instructions.

---

### 8.2 Auto-Detection

**Smart templates automatically detect your tech stack**:

**Detection Logic**:
- Python: `requirements.txt`, `pyproject.toml`, `*.py` files
- TypeScript: `package.json` with `typescript` dependency
- GCP: `gc/` folder, `workflows/` folder
- AWS: `lambda/` folder, `serverless.yml`

**Usage**:
```bash
# Copy smart template
cp ${CURSOR_RULES_PATH}/templates/.cursorrules_smart_template_envvar .cursorrules

# Uncomment sections for your stack
# Cursor will auto-detect and apply rules
```

---

## 9. LANGUAGE-SPECIFIC RULES

**This document contains universal rules**. For language-specific patterns:

**Python**: See [`python-fp-style-guide.md`](python-fp-style-guide.md)
- Libraries: `returns`, `toolz`, `mypy`
- Tools: `uv`, `pytest`, `black`, `ruff`
- Data: `polars` (NEVER pandas)

**TypeScript**: See [`typescript-fp-style-guide.md`](typescript-fp-style-guide.md)
- Libraries: `fp-ts` or `Effect`
- Patterns: `TaskEither`, `pipe`, `flow`

**Swift**: See [`swift-fp-style-guide.md`](swift-fp-style-guide.md)
- Built-in: `Result` type
- Libraries: `Bow`, `Composable Architecture`

**Kotlin**: See [`kotlin-fp-style-guide.md`](kotlin-fp-style-guide.md)
- Libraries: `Arrow`, `kotlinx-coroutines`
- Patterns: `Either`, sealed classes

---

## 10. PLATFORM-SPECIFIC RULES

**GCP (Google Cloud Functions)**: See [`CURSOR_CLOUD_GCP.md`](CURSOR_CLOUD_GCP.md)
- Import rules (no `__init__.py`)
- Test structure (`sys.path.append`)
- Deployment patterns

**AWS (Lambda)**: See [`CURSOR_CLOUD_AWS.md`](CURSOR_CLOUD_AWS.md)
- (Coming soon)

---

## Quick Reference Card

### Mandatory Checklist

Before every commit:
- [ ] Git checkpoint at appropriate trigger
- [ ] Commit message follows format
- [ ] Documentation updated (if applicable)
- [ ] All tests pass
- [ ] Files under 300 lines
- [ ] TODO lists updated (if applicable)

### FP Pattern (All Languages)

```
fetch → validate → transform → save
  ↓        ↓          ↓         ↓
bind     bind       bind      map
```

**Mental Model**: Factory assembly line

### File Organization

```
project-root/
├── ARCHITECTURE_PLAN.md          # Strategic
├── docs/
│   ├── plans/                     # Tactical
│   │   ├── FEATURE_PLAN.md
│   │   └── FEATURE_TODO.md
│   └── YYYY_MM_DD/                # Execution
│       └── YYYYMMDD_HHMM_*.md
```

---

## FAQ

**Q: Do I need to know category theory for FP?**  
A: No! Use the "factory assembly line" mental model. Each function is a station, errors stop the line.

**Q: What if my file is 350 lines?**  
A: Split it. Create a folder, organize by responsibility. Don't modify functionality.

**Q: Can I skip tests for simple functions?**  
A: No. Even simple functions need happy path + error case tests minimum.

**Q: Do I need plan documents for small changes?**  
A: Only for 3+ hour work. Small changes just need good commit messages.

**Q: Which FP library should I use?**  
A: See language guides. Python: `returns`, TypeScript: `Effect`, etc.

---

## Additional Resources

- [SETUP_GUIDE.md](SETUP_GUIDE.md) - Machine setup instructions
- [FILE_LOCATIONS_USER_GUIDE.md](FILE_LOCATIONS_USER_GUIDE.md) - Where to put files
- [CURSOR_FP_PRINCIPLES.md](CURSOR_FP_PRINCIPLES.md) - FP deep dive
- [CURSOR_WORKFLOW_GUIDE.md](CURSOR_WORKFLOW_GUIDE.md) - Workflow details

---

**Version**: 1.0.0  
**Last Updated**: 2025-10-31  
**Maintained By**: Global Rules Repository  
**License**: Internal Use

---

**Follow these rules for consistent, maintainable, type-safe code across all projects!**

