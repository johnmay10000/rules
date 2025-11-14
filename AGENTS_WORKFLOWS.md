---

## 🤖 How to Work with This Repository

### For Content Creation/Editing Tasks

**When asked to create new documentation or guides:**

1. **Check for existing plans** - Look in `docs/plans/` for related TODO files
2. **Follow the hierarchy** - Create/update ARCHITECTURE_PLAN → plans/ → daily logs
3. **Update TODO lists** - Mark tasks as complete with `[x]`, add actual time, update progress %
4. **Follow naming conventions** - Use exact formats specified
5. **Cross-reference everything** - Link to related documents
6. **Git checkpoint** - Commit every 30-60 minutes, follow format exactly
7. **Verify before commit** - Read your work, check formatting, test templates

**When creating language-specific guides:**
- Reference `cursor/CURSOR.md` for mandatory rules
- Follow existing guide structure (see `cursor/python-fp-style-guide.md` as template)
- Include code examples showing before/after transformations
- Add quick links section referencing related documents
- Include setup instructions specific to that language

**When working on Kimi-specific files:**
- Reference `kimi/KIMI.md` for mandatory rules
- Use Kimi's `SetTodoList` tool for task tracking (not just markdown TODOs)
- Demonstrate parallel tool execution where possible (Read multiple files, Write multiple files)
- Follow Kimi-specific formatting: include "Generated with Kimi" in commits
- Use subagents for complex or independent subtasks
- Document Kimi-specific patterns: parallel validation, subagent workflows

### For Template Creation Tasks

**When creating project templates:**

1. **Choose right template type**:
   - `envvar` - Uses `${CURSOR_RULES_PATH}` or `${KIMI_RULES_PATH}` environment variable (portable)
   - `submodule` - Uses relative paths (self-contained)
   - `basic` - Simple projects

2. **Include auto-detection logic** for languages and platforms
3. **Test the template** by creating a test project and verifying Cursor/Kimi detects the stack
4. **Document usage** in template comments and SETUP_GUIDE.md

**When creating Kimi templates:**
- Use `.kimirules` filename instead of `.cursorrules`
- Demonstrate parallel file operations: `ReadFile` multiple files, `WriteFile` multiple files
- Show subagent spawning for complex operations
- Include SetTodoList initialization examples
- Document Kimi-specific workflow patterns (parallel validation, batch operations)

### For Example Project Tasks

**When creating example projects:**

1. **Choose real-world scenario** that demonstrates FP patterns
2. **Include complete `.cursorrules`** that references global rules
3. **Show functional programming patterns** with Result/Either types
4. **Document the example** with README explaining what it demonstrates
5. **Verify it works** - Follow instructions yourself to ensure they work

---

## 📚 Key Documentation Files to Reference

### Essential Reading (Always Read First)

1. **`cursor/CURSOR.md`** - Universal mandatory rules (Git, docs, testing, file size)
2. **`cursor/SETUP_GUIDE.md`** - How end users set up their machines
3. **`README.md`** - Project overview and quick start

### When Working with Specific Languages

| Language | Guide File | Key Patterns | Libraries |
|----------|------------|--------------|-----------|
| Python | `cursor/python-fp-style-guide.md` | Result, Maybe, flow, pipe | `returns`, `toolz`, `mypy` |
| TypeScript | `cursor/typescript-fp-style-guide.md` | Either, TaskEither, pipe | `fp-ts`, `Effect` |
| Swift | `cursor/swift-fp-style-guide.md` | Result, flatMap, Combine | Built-in, Bow, TCA |
| Kotlin | `cursor/kotlin-fp-style-guide.md` | Either, flatMap, coroutines | Arrow |
| Rust | `cursor/rust-fp-style-guide.md` | Result, Option, Iterator | `serde`, `rayon`, `tokio` |
| Haskell | `cursor/haskell-fp-style-guide.md` | Maybe, Either, monads | `base`, `containers`, `mtl` |

### When Working with Platforms

- **GCP**: Cloud Run Functions, GCS, Pub/Sub patterns (coming soon)
- **AWS**: Lambda, DynamoDB, S3 patterns (coming soon)
- **iOS**: SwiftUI patterns, Combine framework integration
- **Android**: Jetpack Compose, Ktor, multiplatform patterns

### When Creating Documentation

- **`cursor/CURSOR_WORKFLOW_GUIDE.md`** - Git checkpoint strategy and commit templates
- **`cursor/FILE_LOCATIONS_USER_GUIDE.md`** - Where to put rules, plans, daily logs
- **`cursor/DATA_STRUCTURE_PATTERNS.md`** - Data structure design guidelines
- **`cursor/NAMING_CONVENTION.md`** - Consistent naming across all guides

---

## 🎯 Functional Programming Patterns Used

### Universal Pattern (Same Across All Languages)

```haskell
-- Haskell (Reference Implementation)
result = loadData
    >>= validate
    >>= transform
    >>= return . format
```

```python
# Python
result = (
    Success(data)
    .bind(validate)
    .bind(transform)
    .map(format)
)
```

```typescript
// TypeScript
const result = pipe(
  data,
  TE.flatMap(validate),
  TE.flatMap(transform),
  TE.map(format)
)
```

**Mental Model**: Factory assembly line - each function is a station, errors stop the line, success continues

### Key FP Principles Enforced

1. ✅ **No naked exceptions** - Wrap in Result/Either types
2. ✅ **Immutable data** - No mutations (use frozen dataclasses, readonly, etc.)
3. ✅ **Pure functions** - No side effects unless explicitly marked
4. ✅ **Composable functions** - Build complexity from small pieces
5. ✅ **Explicit effects** - Mark IO and side effects clearly
6. ✅ **Type-driven development** - Define types first
7. ✅ **Pattern matching** - Exhaustive matching for ADTs
8. ✅ **Railway-oriented programming** - Chain operations, handle errors at boundaries

---

## 🚀 Workflow for Common Tasks

### Task: Create New Language Guide

1. Read `cursor/CURSOR.md` for mandatory structure
2. Study existing guide (e.g., `python-fp-style-guide.md`) as template
3. Create `docs/plans/LANGUAGE_FP_GUIDE_PLAN.md` + `TODO.md`
4. Research language-specific FP libraries and patterns
5. Create guide following established structure
6. Add code examples showing before/after transformations
7. Test examples by creating minimal working code
8. Update TODO list and create daily log
9. Git checkpoint with complete commit message

### Task: Create Project Template

1. Choose scenario (Python GCP function, TypeScript Next.js app, etc.)
2. Create directory in `cursor/examples/`
3. Create complete `.cursorrules` with auto-detection
4. Add realistic project structure with FP patterns
5. Include README explaining the example
6. Test by creating new project from template
7. Verify Cursor auto-detects language and platform
8. Document in `cursor/examples/README.md`
9. Git checkpoint with all files

### Task: Update Existing Guide

1. Read current guide and identify gaps
2. Check `docs/plans/` for related improvement plans
3. Make targeted updates (keep files under 350 lines)
4. Update all cross-references
5. Verify consistency with other guides
6. Test any code examples
7. Update version number and last updated date
8. Git checkpoint with rationale for changes

---

## 🎓 Key Concepts for AI Agents

### This is NOT a Traditional Codebase

**DO NOT expect:**
- Build commands (`npm build`, `cargo build`, `make`)
- Test commands (`pytest`, `jest`, `cargo test`)
- Dependency files (`package.json`, `Cargo.toml`, `requirements.txt`)
- Source code to compile or run
- CI/CD pipelines or deployment processes

**DO expect:**
- Markdown documentation to read and write
- Git commits every 30-60 minutes
- Structured documentation hierarchy
- Cross-references between documents
- Template files to test by copying to projects

### Your Role as AI Agent

You are **not** building software - you are:
- Creating and maintaining documentation
- Establishing coding standards and patterns
- Creating project templates

---

**Part 2 of 3** | Back: **[AGENTS_OVERVIEW.md](AGENTS_OVERVIEW.md)** | Next: **[AGENTS_REFERENCE.md](AGENTS_REFERENCE.md)**

**Last Updated**: 2025-11-14
**Version**: 1.0.1

---

## File Split Notice

**Reason for Split**: This document was split from the original `AGENTS.md` (480+ lines) to comply with the mandatory 350-line maximum file size limit.

**Action Taken**: Divided into three logical modules:
- `AGENTS_OVERVIEW.md` (157 lines) - Project overview and architecture
- `AGENTS_WORKFLOWS.md` (193 lines) - Workflows and how-to guides  
- `AGENTS_REFERENCE.md` (122 lines) - Tools, warnings, and reference materials
