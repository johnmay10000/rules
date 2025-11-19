# Global AI Rules Repository

**Universal functional programming rules and guidelines for AI coding assistants**

[![Version](https://img.shields.io/badge/version-1.0.0-blue.svg)]()
[![AI Tools](https://img.shields.io/badge/AI-Cursor%20%7C%20Kimi%20%7C%20Claude%20%7C%20Gemini-purple.svg)]()
[![Languages](https://img.shields.io/badge/languages-Haskell%20%7C%20Python%20%7C%20TypeScript%20%7C%20Kotlin%20%7C%20Swift%20%7C%20Rust-green.svg)]()
[![Platforms](https://img.shields.io/badge/platforms-GCP%20%7C%20AWS%20%7C%20iOS%20%7C%20Android-orange.svg)]()

> **Cross-language functional programming patterns (6 languages - Haskell as reference implementation!), mandatory development practices, and intelligent auto-detection for AI coding assistants including Cursor, Kimi, Claude, and Gemini.**

**One ruleset, multiple AI tools.** Use the same functional programming patterns across all 6 languages, from Haskell (the reference implementation) to Rust (zero-cost abstractions), with your favorite AI assistant.

---

## 🎯 What Is This?

A **portable, production-tested** global rule set for AI coding assistants that enforces:
- ✅ Mandatory universal practices (Git, docs, testing, file size)
- ✅ Functional programming patterns (Result types, railway-oriented programming)
- ✅ Language-specific guidelines (Haskell, Python, TypeScript, Kotlin, Swift, Rust)
- ✅ Platform-specific rules (GCP, AWS)
- ✅ Auto-detection (detects your stack automatically)
- ✅ Multi-AI tool support (Cursor, Kimi, Claude, Gemini)

**One setup, multiple AI tools, universal patterns.** Use the same FP patterns across all 6 languages with your favorite AI assistant - Cursor, Kimi CLI, Claude Code, or Gemini.

---

## ⚡ Quick Start

Choose your AI tool and follow the setup:

### Setup for **Cursor AI** 🎯

**Step 1: Set environment variable**
```bash
# Add to ~/.zshrc or ~/.bashrc
export CURSOR_RULES_PATH="$HOME/path/to/rules"
```

**Step 2: Create `.cursorrules`**
```markdown
# .cursorrules
@${CURSOR_RULES_PATH}/universal_rules/index.json
@${CURSOR_RULES_PATH}/code_guidelines/languages/python/fp_style_guide.md
```

**Step 3**: Start coding! Cursor enforces Git checkpoints, Result types, and FP patterns.

---

### Setup for **Kimi CLI** 🤖

**Step 1: Set environment variable**
```bash
# Add to ~/.zshrc or ~/.bashrc
export KIMI_RULES_PATH="$HOME/path/to/rules"
```

**Step 2: Create `.kimirules`**
```markdown
# .kimirules
@${KIMI_RULES_PATH}/universal_rules/index.json
@${KIMI_RULES_PATH}/code_guidelines/languages/python/fp_style_guide.md
```

**Step 3**: Start coding! Kimi uses parallel tools and SetTodoList for task tracking.

**Note**: Kimi supports parallel tool execution - read/write multiple files simultaneously for 30-50% speed improvements.

---

### Setup for **Claude Code** 🔮

**Step 1: Set environment variable**
```bash
# Add to ~/.zshrc or ~/.bashrc
export CLAUDE_RULES_PATH="$HOME/path/to/rules"
```

**Step 2: Create `.claude-rules`**
```markdown
# .claude-rules
@${CLAUDE_RULES_PATH}/claude/CLAUDE.md
@${CLAUDE_RULES_PATH}/claude/python-fp-style-guide.md
```

**Step 3**: Start coding! Claude follows similar patterns to Cursor.

---

### Setup for **Gemini** 💎

**Step 1: Set environment variable**
```bash
# Add to ~/.zshrc or ~/.bashrc
export GEMINI_RULES_PATH="$HOME/path/to/rules"
```

**Step 2: Create `.gemini-rules`**
```markdown
# .gemini-rules
@${GEMINI_RULES_PATH}/universal_rules/index.json
@${GEMINI_RULES_PATH}/code_guidelines/languages/python/fp_style_guide.md
```

**Step 3**: Start coding! Gemini follows the same FP patterns.

---

### Alternative: Git Submodule (All Tools)

```bash
# In your project
git submodule add https://github.com/your-org/rules .ai-rules
```

Then reference with relative paths in your rules file.

See [SETUP_GUIDE.md](SETUP_GUIDE.md) for detailed instructions for each AI tool.

---

## 📚 Core Documents

### Universal Rules (All Tools)

**[universal_rules/](universal_rules/)** ⭐ START HERE
- **[universal_rules/git/git_checkpoint_rules.md](universal_rules/git/git_checkpoint_rules.md)** - Mandatory Git rules
- **[universal_rules/project_structure/file_size_limits.md](universal_rules/project_structure/file_size_limits.md)** - File size limits
- **[universal_rules/testing/testing_philosophy.md](universal_rules/testing/testing_philosophy.md)** - Testing standards

### Tool-Specific Guides

**For Cursor AI Users** 🎯
- **[cursor/SETUP_GUIDE.md](cursor/SETUP_GUIDE.md)** 🚀 Setup instructions
- **[cursor/examples/](cursor/examples/)** 🎨 Real-world examples

**For Kimi CLI Users** 🤖
- **[kimi/SETUP_GUIDE.md](kimi/SETUP_GUIDE.md)** 🚀 Setup instructions
- **[kimi/examples/](kimi/examples/)** 🎨 Real-world examples

**For Claude Code Users** 🔮
- **Structure mirrors cursor/** folder (coming soon)
- Same FP patterns, Claude-specific workflow adaptations

**For Gemini Users** 💎
- **Structure mirrors cursor/** folder (coming soon)
- Same FP patterns, Gemini-specific optimizations

### Universal FP Principles

All AI tools share the same core principles:

**[code_guidelines/principles/](code_guidelines/principles/)**
- ADTs explained
- Result/Either types
- Monadic composition
- Railway-oriented programming

**[code_guidelines/languages/](code_guidelines/languages/)**
- Language-specific implementations
- Foldable/Traversable patterns

**[universal_rules/documentation/](universal_rules/documentation/)**
- File organization
- Naming conventions

---

## 🗂️ Language Guides

### Choose Your Language & AI Tool

**All languages have guides for both Cursor and Kimi.** Other AI tools (Claude, Gemini) use the same patterns with tool-specific adaptations.

#### 🎩 Haskell (Reference Implementation!)

The **gold standard** where FP concepts originated:

**For Cursor:** [code_guidelines/languages/haskell/fp_style_guide.md](code_guidelines/languages/haskell/fp_style_guide.md)  
**For Kimi:** [code_guidelines/languages/haskell/fp_style_guide.md](code_guidelines/languages/haskell/fp_style_guide.md)

- **Use for**: Compilers, DSLs, financial systems, type-safe web APIs
- **Tools**: Stack, GHC, Hspec + QuickCheck
- **Libraries**: `base`, `containers`, `text`, `mtl`, `aeson`, `servant`
- **Unique**: Native HKT, lazy evaluation, infinite data structures, type-driven development
- **Patterns**: `Maybe`, `Either`, Monads, `Foldable`, `Traversable` (the originals!)

---

#### 🐍 Python

**For Cursor:** [code_guidelines/languages/python/fp_style_guide.md](code_guidelines/languages/python/fp_style_guide.md)  
**For Kimi:** [code_guidelines/languages/python/fp_style_guide.md](code_guidelines/languages/python/fp_style_guide.md)

- **Use for**: ML, data processing, cloud functions
- **Libraries**: `returns` (Result types), `toolz`, `polars`, `mypy`
- **Testing**: `pytest` with Result type assertions
- **Patterns**: Railway composition, Maybe/Option, immutable transforms

---

#### 📘 TypeScript

**For Cursor:** [code_guidelines/languages/typescript/fp_style_guide.md](code_guidelines/languages/typescript/fp_style_guide.md)  
**For Kimi:** [code_guidelines/languages/typescript/fp_style_guide.md](code_guidelines/languages/typescript/fp_style_guide.md)

- **Use for**: Next.js, backends, serverless, full-stack apps
- **Libraries**: `fp-ts` (Cursor) or `Effect` (Kimi recommended)
- **Patterns**: TaskEither, pipe/flow, discriminated unions, exhaustiveness checking
- **Kimi Difference**: Uses Effect-ts for better TypeScript integration

---

#### 🍎 Swift

**For Cursor:** [code_guidelines/languages/swift/fp_style_guide.md](code_guidelines/languages/swift/fp_style_guide.md)  
**For Kimi:** [code_guidelines/languages/swift/fp_style_guide.md](code_guidelines/languages/swift/fp_style_guide.md)

- **Use for**: iOS, macOS, SwiftUI, Combine
- **Libraries**: Built-in Result, Bow, TCA (The Composable Architecture)
- **Patterns**: Result types, value types (struct), Combine framework

---

#### 🤖 Kotlin

**For Cursor:** [code_guidelines/languages/kotlin/fp_style_guide.md](code_guidelines/languages/kotlin/fp_style_guide.md)  
**For Kimi:** [code_guidelines/languages/kotlin/fp_style_guide.md](code_guidelines/languages/kotlin/fp_style_guide.md)

- **Use for**: Android, Ktor, multiplatform development
- **Libraries**: Arrow (Either, Option, Optics)
- **Patterns**: Either types, sealed classes, coroutines, railway composition

---

#### 🦀 Rust

**For Cursor:** [code_guidelines/languages/rust/fp_style_guide.md](code_guidelines/languages/rust/fp_style_guide.md)  
**For Kimi:** [code_guidelines/languages/rust/fp_style_guide.md](code_guidelines/languages/rust/fp_style_guide.md)

- **Use for**: Systems programming, performance-critical code, CLI tools
- **Libraries**: `serde` (serialization), `rayon` (parallel), `tokio` (async)
- **Patterns**: Result, Option, Iterator, zero-cost abstractions
- **Performance**: Best of all 6 languages!
- **Benefits**: Compile-time guarantees, no null, thread-safe by default

---

## 📖 Universal FP Pattern

**The same pattern works in ALL languages**:

```haskell
-- Haskell (THE REFERENCE IMPLEMENTATION!)
result = loadData
    >>= validate    -- Returns Maybe/Either/IO
    >>= transform   -- Returns Maybe/Either/IO
    >>= return . format  -- Pure function
-- Or with do-notation:
result = do
    data <- loadData
    valid <- validate data
    trans <- transform valid
    return (format trans)
```

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

### AI Tool Examples

**For Cursor AI** 🎯 **[cursor/examples/](cursor/examples/)**
- Python + GCP Cloud Functions
- TypeScript + Next.js + Supabase
- Swift + TypeScript + Python polyglot
- Plan with TODO (3-tier documentation)

**For Kimi CLI** 🤖 **[kimi/examples/](kimi/examples/)**
- **plan_with_todo** - SetTodoList integration demonstration
- **python_project** - ML pipeline with `returns` library
- **rust_project** - CLI tool with Result<T, E> types
- **typescript_project** - Full-stack Next.js with Effect-ts
- **Key features**: Parallel validation, subagent workflows, batch operations

**For Claude Code** 🔮 **[claude/examples/](claude/examples/)**
- Coming soon! (Structure mirrors cursor/)

**For Gemini** 💎 **[gemini/examples/](gemini/examples/)**
- Coming soon! (Structure mirrors cursor/)

### Smart Templates by AI Tool

**For Cursor** 🎯 **[cursor/templates/](cursor/templates/)**
- `.cursorrules_smart_template_envvar` - Uses `${CURSOR_RULES_PATH}`
- `.cursorrules_smart_template_submodule` - Uses relative paths
- Copy and customize - auto-detects language and platform

**For Kimi** 🤖 **[kimi/templates/](kimi/templates/)**
- `.kimirules_smart_template_envvar` - Uses `${KIMI_RULES_PATH}`
- `.kimirules_smart_template_submodule` - Uses relative paths
- `.kimirules_basic_template` - Simple manual configuration
- **Kimi advantage**: Parallel file operations supported (30-50% faster)

**For Claude** 🔮 **[claude/templates/](claude/templates/)**
- Coming soon! Mirrors Kimi/Cursor structure

**For Gemini** 💎 **[gemini/templates/](gemini/templates/)**
- Coming soon! Mirrors Kimi/Cursor structure

---

## 🔧 Auto-Detection

**All AI tools automatically detect your stack** - no manual configuration needed!

### Language Detection

**Python** `.py`, `requirements.txt`, `pyproject.toml` → Python guide  
**TypeScript** `.ts`, `.tsx`, `tsconfig.json` → TypeScript guide  
**Swift** `.swift`, `Package.swift` → Swift guide  
**Kotlin** `.kt`, `build.gradle.kts` → Kotlin guide  
**Rust** `.rs`, `Cargo.toml` → Rust guide  
**Haskell** `.hs`, `*.cabal` → Haskell guide  

### Platform Detection

**Google Cloud** `google-cloud-*`, `app.yaml` → GCP guidelines  
**AWS** `aws-sdk`, `serverless.yml` → AWS guidelines  
**iOS** `Package.swift`, `*.xcodeproj` → iOS patterns  
**Android** `build.gradle.kts`, `AndroidManifest.xml` → Android patterns  

### Framework Detection

**Next.js** `next.config.js` → Next.js patterns  
**SwiftUI** `Podfile`, `*.swift` → SwiftUI patterns  
**Inngest** `inngest` → Background job patterns  

### AI Tool Differences

**Cursor**: Single file operations, markdown-based TODOs  
**Kimi**: **Parallel execution** of multiple files (30-50% faster!), SetTodoList tool, subagent spawning  
**Claude**: Command-based workflow (similar to Kimi)  
**Gemini**: Concise, fast pattern matching

**All tools use the same FP patterns** - just adapted to each tool's architecture!

**Result**: Drop a template into your project and your AI assistant automatically loads the right rules for your stack!

---

## 📏 Mandatory Rules Summary

**All AI tools enforce the same core rules**, adapted to each tool's architecture:

### 1. Git Checkpoints (MANDATORY for all tools)
- ✅ Commit every **30-60 minutes** during active work
- ✅ After bug fixes, features, documentation updates
- ✅ Specific commit message format (detailed, with context)
- ✅ Small, frequent, logical commits

**Tool formats:**
- **Cursor**: [cursor/CURSOR.md](cursor/CURSOR.md)
- **Kimi**: [kimi/KIMI.md](kimi/KIMI.md) - Includes parallel execution guidance
- **Claude**: Coming soon!
- **Gemini**: Coming soon!

### 2. Documentation (MANDATORY)
- ✅ **3-tier hierarchy**: ARCHITECTURE_PLAN → plans/ → YYYY_MM_DD/
- ✅ Sequential daily work docs (YYYYMMDD_NNNN_NAME.md format)
- ✅ TODO lists paired with plans (FEATURE_PLAN.md + FEATURE_TODO.md)
- ✅ Auto-update progress tracking

**Tool differences:**
- **Cursor**: Markdown TODOs, manual updates
- **Kimi**: SetTodoList tool (formal task tracking) + parallel validation
- **Claude**: Similar to Kimi (command-based)
- **Gemini**: Similar to Cursor (pattern-based)

### 3. Testing (MANDATORY)
- ✅ **Comprehensive coverage**: happy path + errors + edge cases
- ✅ **80%+ coverage** for business logic
- ✅ **All tests pass** before commit
- ✅ 3+ tests per function
- ✅ Test pure functions (easy), test effectful functions (use mocks)

### 4. File Size (MANDATORY)
- ✅ **250-300 lines** target (sweet spot for maintainability)
- ✅ **350 lines** absolute maximum
- ✅ Split into modules if exceeded
- ✅ Document exceptions with rationale

**Rationale**: Small files are easier to review, understand, test, and maintain. AI assistants work better with focused, cohesive files.

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

### Multi-Cloud FP Patterns

**For Cursor:** **[cursor/aws-fp-style-guide.md](cursor/aws-fp-style-guide.md)** / **[cursor/gcp-fp-style-guide.md](cursor/gcp-fp-style-guide.md)**  
**For Kimi:** **[kimi/aws-fp-style-guide.md](kimi/aws-fp-style-guide.md)** / **[kimi/gcp-fp-style-guide.md](kimi/gcp-fp-style-guide.md)**

### Amazon Web Services (AWS)

✅ **Available Now** (not "Coming Soon"):

**[cursor/aws-fp-style-guide.md](cursor/aws-fp-style-guide.md)** / **[kimi/aws-fp-style-guide.md](kimi/aws-fp-style-guide.md)**

- **AWS Lambda** with railway-oriented handlers (Result types)
- **Step Functions** workflows (sequential + parallel Map)
- **DynamoDB** event sourcing pattern with immutable events
- **EventBridge** type-safe event routing
- **S3** immutable snapshots + versioned objects
- **Multi-region deployment** validation strategies
- **LocalStack** testing patterns
- **Cost optimization** with lazy initialization

**Use Cases**: Serverless APIs, event-driven architectures, data pipelines, microservices

---

### Google Cloud Platform (GCP)

✅ **Available Now** (not "Coming Soon"):

**[cursor/gcp-fp-style-guide.md](cursor/gcp-fp-style-guide.md)** / **[kimi/gcp-fp-style-guide.md](kimi/gcp-fp-style-guide.md)**

- **Cloud Functions** with pure handler factories
- **Cloud Run + Express** with type-safe middleware
- **Firestore** event sourcing with transactions
- **Pub/Sub** parallel batch processing
- **Eventarc** circuit breaker pattern
- **Cloud Build** railway CI/CD pipeline
- **Firebase Emulator** local development

**Use Cases**: Real-time applications, mobile backends, data streaming, event-driven systems

---

### iOS & SwiftUI

- **SwiftUI patterns** with Combine framework
- **Result types** for error handling
- **MVVM + FP** architecture
- **Value types** (struct over class)
- **Async/await** with error boundaries

---

### Android & Kotlin Multiplatform

- **Jetpack Compose** with state management
- **Ktor** for multiplatform HTTP clients
- **Arrow-kt** for FP patterns
- **Coroutines** with structured concurrency
- **Multiplatform** shared code

---

## 🎓 Learning Path

**New to FP and AI coding assistants?** Follow this path:

### Step 1: Choose Your AI Tool (5 min)

Pick the AI assistant you'll use:

- **[Cursor AI](https://cursor.sh/)** 🎯 - Great IDE integration, single-file focus
- **[Kimi CLI](https://kimi.ai)** 🤖 - **Parallel execution** (30-50% faster!), SetTodoList tool
- **[Claude Code](https://www.anthropic.com/claude-code)** 🔮 - Command-based workflow, excellent reasoning
- **[Gemini Code Assist](https://gemini.google.com/)** 💎 - Google's AI, concise patterns

**New to AI assistants?** Start with Cursor (easiest IDE integration) or Kimi (fastest with parallel tools).

### Step 2: Read Your Tool's Core Rules (30 min)

**For Cursor users:**
1. Read [cursor/CURSOR.md](cursor/CURSOR.md) - Main mandatory rules
2. See universal FP pattern and examples

**For Kimi users:**
1. Read [kimi/KIMI.md](kimi/KIMI.md) - Main mandatory rules
2. Understand parallel execution benefits
3. Learn SetTodoList tool for task tracking

### Step 3: Learn FP Principles (1 hour)

**All tools share the same FP principles:**

Read [cursor/CURSOR_FP_PRINCIPLES.md](cursor/CURSOR_FP_PRINCIPLES.md) or [kimi/KIMI_FP_PRINCIPLES.md](kimi/KIMI_FP_PRINCIPLES.md):
- ADTs (Algebraic Data Types)
- Result/Either types (error handling without exceptions)
- Monadic composition (railway programming)
- Railway-oriented programming (the "assembly line" model)

**Mental model**: Factory assembly line - each function is a station, errors stop the line, success continues.

### Step 4: Choose Your Language Guide (30 min)

Pick your language and read both Cursor and Kimi guides:

- **Python** 🐍: [cursor/python-fp-style-guide.md](cursor/python-fp-style-guide.md) / [kimi/python-fp-style-guide.md](kimi/python-fp-style-guide.md)
- **TypeScript** 📘: [cursor/typescript-fp-style-guide.md](cursor/typescript-fp-style-guide.md) / [kimi/typescript-fp-style-guide.md](kimi/typescript-fp-style-guide.md)
- **Kotlin** 🤖: [cursor/kotlin-fp-style-guide.md](cursor/kotlin-fp-style-guide.md) / [kimi/kotlin-fp-style-guide.md](kimi/kotlin-fp-style-guide.md)
- **Swift** 🍎: [cursor/swift-fp-style-guide.md](cursor/swift-fp-style-guide.md) / [kimi/swift-fp-style-guide.md](kimi/swift-fp-style-guide.md)
- **Rust** 🦀: [cursor/rust-fp-style-guide.md](cursor/rust-fp-style-guide.md) / [kimi/rust-fp-style-guide.md](kimi/rust-fp-style-guide.md)
- **Haskell** 🎩: [cursor/haskell-fp-style-guide.md](cursor/haskell-fp-style-guide.md) / [kimi/haskell-fp-style-guide.md](kimi/haskell-fp-style-guide.md) ⭐ **Reference implementation**

### Step 5: Try an Example (15-30 min)

**For Cursor:** Copy an example from [cursor/examples/](cursor/examples/)  
**For Kimi:** Copy an example from [kimi/examples/](kimi/examples/) - shows SetTodoList and parallel tools

Adapt the `.cursorrules` or `.kimirules` to your project and start coding!

**Total time**: ~2-3 hours to full productivity with FP and your AI assistant

**Pro tip**: Start small - apply FP patterns to one function, then expand.

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
├── universal_rules/                    # 🌍 Mandatory rules for all tools
│   ├── index.json                      # 📖 Master index
│   ├── git/                            # 🔧 Git rules
│   ├── testing/                        # 🧪 Testing rules
│   ├── documentation/                  # 📚 Documentation rules
│   ├── project_structure/              # 🏗️ Structure rules
│   └── ai_tool_usage/                  # 🤖 Tool usage rules
│
├── code_guidelines/                    # 📝 Language-specific guidelines
│   ├── index.json                      # 📖 Guidelines index
│   ├── languages/                      # 🏳️ Language guides
│   │   ├── python/
│   │   ├── typescript/
│   │   ├── rust/
│   │   ├── kotlin/
│   │   ├── swift/
│   │   └── haskell/
│   └── principles/                     # 🧠 FP Principles
│
├── cursor/                             # 🎯 Cursor-specific files
│   ├── SETUP_GUIDE.md                  # 🚀 Initial setup
│   ├── templates/                      # 📋 Smart templates
│   └── examples/                       # 🎨 Real-world examples
│
├── kimi/                               # ✨ Kimi-specific files
│   ├── SETUP_GUIDE.md                  # 🚀 Initial setup
│   ├── templates/                      # 📋 Smart templates
│   └── examples/                       # 🎨 Real-world examples
│
├── claude/                             # 🔮 Claude-specific files
│
├── gemini/                             # 💎 Gemini-specific files
│
├── docs/                               # 📚 Planning docs
│   ├── plans/                          # Sub-plans
│   └── YYYY_MM_DD/                     # Daily work
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

## 🤖 Kimi CLI Integration

**NEW!** ✨ Kimi CLI now supports this repository with complete rules system:

**Location**: `kimi/` folder contains full Kimi rules system mirroring cursor/ structure

**Quick Start for Kimi**:
1. Navigate to project root
2. Copy appropriate template: `cp kimi/templates/.kimirules_smart_template_envvar ./.kimirules`
3. Set environment: `export KIMI_RULES_PATH="$HOME/projects/rules"`
4. Start working with Kimi - rules auto-load!

**Kimi Rules Mirror Cursor**:
- [kimi/KIMI.md](kimi/KIMI.md) - Main Kimi rule set
- [kimi/KIMI_WORKFLOW_GUIDE.md](kimi/KIMI_WORKFLOW_GUIDE.md) - Kimi-specific workflows
- Language guides for Python, TypeScript, Rust, Kotlin, Swift, Haskell
- Platform guides for AWS and GCP
- Smart templates with auto-detection
- 4 comprehensive examples

**Key Differences**:
- Kimi uses tool-based architecture (parallel execution)
- SetTodoList integration for task management
- Subagent spawning for complex tasks
- Effect-ts for TypeScript (vs fp-ts in Cursor)

**Generate with Kimi**:
```
All commits labeled "Generated with [Kimi](https://kimi.ai)"
```

---

## 🎯 Quick Links

**Essential**:
- [cursor/CURSOR.md](cursor/CURSOR.md) - Main Cursor rules
- [cursor/SETUP_GUIDE.md](cursor/SETUP_GUIDE.md) - Setup
- [kimi/KIMI.md](kimi/KIMI.md) - Kimi rules
- [Examples](cursor/examples/) - Real-world templates

**Guides**:
- [Python (Cursor)](cursor/python-fp-style-guide.md) | [Python (Kimi)](kimi/python-fp-style-guide.md)
- [TypeScript (Cursor)](cursor/typescript-fp-style-guide.md) | [TypeScript (Kimi)](kimi/typescript-fp-style-guide.md)
- [Kotlin (Cursor)](cursor/kotlin-fp-style-guide.md) | [Kotlin (Kimi)](kimi/kotlin-fp-style-guide.md)
- [Swift (Cursor)](cursor/swift-fp-style-guide.md) | [Swift (Kimi)](kimi/swift-fp-style-guide.md)
- [Rust (Cursor)](cursor/rust-fp-style-guide.md) ⭐ NEW! | [Rust (Kimi)](kimi/rust-fp-style-guide.md)

**Deep Dives**:
- [FP Principles (Cursor)](cursor/CURSOR_FP_PRINCIPLES.md)
- [Workflow (Cursor)](cursor/CURSOR_WORKFLOW_GUIDE.md)
- [FP Principles (Kimi)](kimi/KIMI_FP_PRINCIPLES.md)
- [Workflow (Kimi)](kimi/KIMI_WORKFLOW_GUIDE.md)

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

