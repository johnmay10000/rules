# Global Cursor Rules Repository

**Universal functional programming rules and guidelines for Cursor AI**

[![Version](https://img.shields.io/badge/version-1.0.0-blue.svg)]()
[![Languages](https://img.shields.io/badge/languages-Python%20%7C%20TypeScript%20%7C%20Swift%20%7C%20Kotlin-green.svg)]()
[![Platforms](https://img.shields.io/badge/platforms-GCP%20%7C%20AWS%20%7C%20iOS%20%7C%20Android-orange.svg)]()

> **Cross-language functional programming patterns, mandatory development practices, and intelligent auto-detection for Cursor AI.**

---

## 🎯 What Is This?

A **portable, production-tested** global rule set for Cursor that enforces:
- ✅ Mandatory universal practices (Git, docs, testing, file size)
- ✅ Functional programming patterns (Result types, railway-oriented programming)
- ✅ Language-specific guidelines (Python, TypeScript, Swift, Kotlin)
- ✅ Platform-specific rules (GCP, AWS)
- ✅ Auto-detection (detects your stack automatically)

**One setup, works everywhere.** Use the same FP patterns across all languages.

---

## ⚡ Quick Start

### 1. One-Time Machine Setup

Choose your approach:

**Option A: Environment Variable** (Recommended)
```bash
# Add to ~/.zshrc or ~/.bashrc
export CURSOR_RULES_PATH="$HOME/path/to/rules"
```

**Option B: Git Submodule**
```bash
# In your project
git submodule add https://github.com/your-org/rules .cursor-rules
```

See [SETUP_GUIDE.md](SETUP_GUIDE.md) for detailed instructions.

### 2. Create `.cursorrules` in Your Project

```markdown
# .cursorrules

## Global Rules
@${CURSOR_RULES_PATH}/cursor/CURSOR.md

## Language-Specific Rules
@${CURSOR_RULES_PATH}/cursor/python-fp-style-guide.md
# Or: cursor/typescript-fp-style-guide.md, cursor/swift-fp-style-guide.md, cursor/kotlin-fp-style-guide.md

## Project-Specific Overrides
[Your project-specific rules here]
```

### 3. Start Coding!

Cursor now enforces:
- Git checkpoints every 30-60 min
- Result types for error handling
- File size limits (250 lines)
- Comprehensive testing
- FP principles

---

## 📚 Core Documents

### Essential Reading

**[cursor/CURSOR.md](cursor/CURSOR.md)** ⭐ START HERE
- Main global rule set
- Mandatory universal rules
- FP principles overview
- Quick reference card

**[cursor/SETUP_GUIDE.md](cursor/SETUP_GUIDE.md)** 🚀 SETUP
- One-time machine configuration
- Both portable approaches
- Platform-specific setup (macOS, Linux, Windows)

**[cursor/FILE_LOCATIONS_USER_GUIDE.md](cursor/FILE_LOCATIONS_USER_GUIDE.md)** 📁 FILE LOCATIONS
- Where to put global rules
- Where to put project rules
- How Cursor finds files

### Deep Dives

**[cursor/CURSOR_FP_PRINCIPLES.md](cursor/CURSOR_FP_PRINCIPLES.md)** 🧠 FP DEEP DIVE
- ADTs explained
- Result/Either types
- Monadic composition
- Railway-oriented programming
- Real-world examples

**[cursor/CURSOR_WORKFLOW_GUIDE.md](cursor/CURSOR_WORKFLOW_GUIDE.md)** 🔄 WORKFLOW
- Git checkpoint strategy
- Commit message templates
- Documentation hierarchy
- TODO list management

---

## 🗂️ Language Guides

### Functional Programming Style Guides

**[cursor/python-fp-style-guide.md](cursor/python-fp-style-guide.md)** 🐍
- For ML, data processing, cloud functions
- Libraries: `returns`, `toolz`, `polars`
- Testing: `pytest` with Result types

**[cursor/typescript-fp-style-guide.md](cursor/typescript-fp-style-guide.md)** 📘
- For Next.js, backends, serverless
- Libraries: `fp-ts`, `Effect`
- Patterns: TaskEither, pipe, discriminated unions

**[cursor/swift-fp-style-guide.md](cursor/swift-fp-style-guide.md)** 🍎
- For iOS, macOS, SwiftUI
- Libraries: Built-in Result, Bow, TCA
- Patterns: Result types, value types, Combine

**[cursor/kotlin-fp-style-guide.md](cursor/kotlin-fp-style-guide.md)** 🤖
- For Android, Ktor, multiplatform
- Libraries: Arrow
- Patterns: Either, sealed classes, coroutines

---

## 📖 Universal FP Pattern

**The same pattern works in ALL languages**:

```python
# Python
result = (
    Success(data)
    .bind(validate)      # Returns Result
    .bind(transform)     # Returns Result
    .map(format)         # Pure function
)
```

```typescript
// TypeScript
const result = pipe(
  data,
  TE.flatMap(validate),    // Returns TaskEither
  TE.flatMap(transform),   // Returns TaskEither
  TE.map(format)           // Pure function
)
```

```swift
// Swift
let result = loadData()
    .flatMap(validate)       // Returns Result
    .flatMap(transform)      // Returns Result
    .map(format)             // Pure function
```

```kotlin
// Kotlin
val result = loadData()
    .flatMap { validate(it) }      // Returns Either
    .flatMap { transform(it) }     // Returns Either
    .map { format(it) }            // Pure function
```

**Mental Model**: Factory assembly line
- Each function = one station
- Errors stop the line
- Success continues to next station

---

## 🎨 Templates & Examples

### Ready-to-Use Examples

**[cursor/examples/python_project/](cursor/examples/python_project/)**
- Python + GCP Cloud Functions
- Data processing pipeline
- Complete `.cursorrules` example

**[cursor/examples/typescript_project/](cursor/examples/typescript_project/)**
- TypeScript + Next.js + Supabase
- SaaS application
- Background jobs with Inngest

**[cursor/examples/polyglot_project/](cursor/examples/polyglot_project/)**
- Swift (iOS) + TypeScript (API) + Python (ML)
- Multi-language full-stack
- Shared type definitions

**[cursor/examples/plan_with_todo/](cursor/examples/plan_with_todo/)**
- 3-tier documentation hierarchy
- ARCHITECTURE_PLAN.md example
- Sub-plan with paired TODO list
- Cursor auto-update workflow

### Smart Templates

**[cursor/templates/.cursorrules_smart_template_envvar](cursor/templates/.cursorrules_smart_template_envvar)**
- Auto-detects language and platform
- Uses `${CURSOR_RULES_PATH}`
- Copy and customize

**[cursor/templates/.cursorrules_smart_template_submodule](cursor/templates/.cursorrules_smart_template_submodule)**
- Auto-detects language and platform
- Uses `.cursor-rules/` relative path
- For git submodule approach

---

## 🔧 Auto-Detection

Cursor **automatically detects** your stack:

**Languages**:
- `.py` files → Python guide
- `.ts`, `.tsx` files → TypeScript guide
- `.swift` files → Swift guide
- `.kt` files → Kotlin guide

**Platforms**:
- `google-cloud-*` deps → GCP guidelines
- `aws-sdk` deps → AWS guidelines
- `Package.swift` → iOS patterns
- `build.gradle.kts` → Android patterns

**Frameworks**:
- `next.config.js` → Next.js patterns
- `Podfile` → SwiftUI patterns
- `inngest` → Background job patterns

**No manual configuration needed!**

---

## 📏 Mandatory Rules Summary

From [CURSOR.md](CURSOR.md):

### 1. Git Checkpoints (MANDATORY)
- ✅ Commit every **30-60 minutes**
- ✅ After bug fixes, features, docs
- ✅ Specific commit message format
- ✅ Small, frequent commits

### 2. Documentation (MANDATORY)
- ✅ **3-tier hierarchy**: ARCHITECTURE_PLAN → plans/ → YYYY_MM_DD/
- ✅ Timestamped daily work docs
- ✅ TODO lists paired with plans
- ✅ Cursor auto-updates TODOs

### 3. Testing (MANDATORY)
- ✅ **Comprehensive coverage**: happy path + errors + edge cases
- ✅ **80%+ coverage** for business logic
- ✅ **All tests pass** before commit
- ✅ 3+ tests per function

### 4. File Size (MANDATORY)
- ✅ **250-300 lines** target
- ✅ **350 lines** absolute maximum
- ✅ Split into modules if exceeded
- ✅ Document exceptions

---

## 🚀 Recommended Patterns

From [cursor/CURSOR.md](cursor/CURSOR.md):

### 5. Functional Programming
- ✅ Pure functions (no side effects)
- ✅ Immutable data structures
- ✅ Result/Either types (no exceptions)
- ✅ Pattern matching (exhaustive)
- ✅ Railway-oriented programming
- ✅ ADTs for domain modeling
- ✅ No defaults/fallbacks

### 6. Code Organization
- ✅ 4-layer architecture (Entry → IO → Result → Pure)
- ✅ Clear separation of concerns
- ✅ Small, composable functions

### 7. Type Safety
- ✅ Type-driven development
- ✅ Define types first
- ✅ Compiler-guided implementation

---

## 🌍 Platform Guidelines

### Google Cloud Platform

**Coming Soon**: [GCP_GUIDELINES.md](GCP_GUIDELINES.md)
- Cloud Run Functions structure
- GCS operations
- Pub/Sub patterns
- Testing with `sys.path.append()`

### Amazon Web Services

**Coming Soon**: [AWS_GUIDELINES.md](AWS_GUIDELINES.md)
- Lambda function structure
- DynamoDB operations
- S3 patterns
- CDK infrastructure

---

## 🎓 Learning Path

**New to FP?** Follow this path:

1. Read [cursor/CURSOR.md](cursor/CURSOR.md) (30 min)
   - Understand mandatory rules
   - See universal FP pattern

2. Read [cursor/CURSOR_FP_PRINCIPLES.md](cursor/CURSOR_FP_PRINCIPLES.md) (1 hour)
   - Learn ADTs
   - Understand Result types
   - Master railway-oriented programming

3. Choose your language guide (30 min)
   - [Python](cursor/python-fp-style-guide.md)
   - [TypeScript](cursor/typescript-fp-style-guide.md)
   - [Swift](cursor/swift-fp-style-guide.md)
   - [Kotlin](cursor/kotlin-fp-style-guide.md)

4. Try an example (15 min)
   - Copy example `.cursorrules`
   - Adapt to your project
   - Start coding!

**Total time**: ~2 hours to full productivity

---

## 💡 FAQ

### Why functional programming?

**Benefits**:
- ✅ **Testability**: Pure functions easy to test
- ✅ **Reliability**: Type system catches errors
- ✅ **Maintainability**: Explicit, no hidden state
- ✅ **Composability**: Build complex from simple
- ✅ **Concurrency**: Immutability = thread-safe

### Do I need to know category theory?

**No!** We use the "factory assembly line" mental model:
- Each function = one station
- Errors stop the line
- Success continues

No monads, functors, or category theory required.

### What if my team uses imperative style?

**Incremental adoption**:
- ✅ Start with new code (mandatory FP)
- ✅ Refactor old code gradually
- ✅ Small changes with tests
- ✅ Monitor for improvements

### Can I use this with existing codebases?

**Yes!** Two approaches:
- Add `.cursorrules` to root (new code follows rules)
- Gradual migration (see migration guide)

### What about other languages?

The principles apply to **any language**:
- Same Result/Either pattern
- Same railway-oriented programming
- Same ADT concepts
- Adapt syntax to your language

---

## 🗺️ Repository Structure

```
rules/
├── cursor/                             # 🎯 All Cursor files (isolated)
│   ├── CURSOR.md                       # ⭐ Main global rule set
│   ├── CURSOR_FP_PRINCIPLES.md         # 🧠 FP deep dive
│   ├── CURSOR_WORKFLOW_GUIDE.md        # 🔄 Git and docs workflow
│   ├── SETUP_GUIDE.md                  # 🚀 Initial setup
│   ├── FILE_LOCATIONS_USER_GUIDE.md    # 📁 Where files go
│   │
│   ├── python-fp-style-guide.md        # 🐍 Python guide
│   ├── typescript-fp-style-guide.md    # 📘 TypeScript guide
│   ├── swift-fp-style-guide.md         # 🍎 Swift guide
│   ├── kotlin-fp-style-guide.md        # 🤖 Kotlin guide
│   │
│   ├── templates/                      # 📋 Smart templates
│   │   ├── .cursorrules_smart_template_envvar
│   │   └── .cursorrules_smart_template_submodule
│   │
│   └── examples/                       # 🎨 Real-world examples
│       ├── python_project/
│       ├── typescript_project/
│       ├── polyglot_project/
│       └── plan_with_todo/
│
├── claude/                             # 🔮 Future: Claude files (isolated)
│
├── docs/                               # 📚 Planning docs (for this repo)
│   ├── 2025_10_30/                    # Daily work
│   ├── 2025_10_31/                    # Daily work
│   └── plans/                          # Sub-plans
│
├── .cursorrules                        # 🔧 Rules for this repo
├── README.md                           # 📖 This file
└── MIGRATION_GUIDE.md                  # 🚀 Migration guide
```

---

## 🤝 Contributing

This is a living repository! To contribute:

1. Follow the rules in [CURSOR.md](CURSOR.md)
2. Create feature plan in `docs/plans/`
3. Create paired TODO list
4. Make changes with tests
5. Update documentation
6. Submit PR

---

## 📜 License

See LICENSE file for details.

---

## 📞 Support

**Issues**: GitHub Issues  
**Docs**: This README + [CURSOR.md](CURSOR.md)  
**Examples**: `examples/` directory  

---

## 🎯 Quick Links

**Essential**:
- [cursor/CURSOR.md](cursor/CURSOR.md) - Main rules
- [cursor/SETUP_GUIDE.md](cursor/SETUP_GUIDE.md) - Setup
- [Examples](cursor/examples/) - Real-world templates

**Guides**:
- [Python](cursor/python-fp-style-guide.md)
- [TypeScript](cursor/typescript-fp-style-guide.md)
- [Swift](cursor/swift-fp-style-guide.md)
- [Kotlin](cursor/kotlin-fp-style-guide.md)

**Deep Dives**:
- [FP Principles](cursor/CURSOR_FP_PRINCIPLES.md)
- [Workflow](cursor/CURSOR_WORKFLOW_GUIDE.md)

---

**Version**: 1.0.0  
**Last Updated**: 2025-10-31  
**Status**: Production Ready ✅  

---

**Start here**: Read [cursor/CURSOR.md](cursor/CURSOR.md), follow [cursor/SETUP_GUIDE.md](cursor/SETUP_GUIDE.md), try an [example](cursor/examples/)!

---

## 📂 Why the `cursor/` Folder?

**Isolation**: All Cursor-specific files are self-contained in `cursor/`
- Easy to add parallel `claude/` folder for Claude-specific rules
- Clear separation of concerns
- Portable as a unit
- No conflicts between AI tools

**Future**: Add `claude/`, `copilot/`, etc. as needed!

