# AGENTS.md - AI Agent Guide for Global Cursor Rules Repository

**Repository Type**: Documentation & Rule Sets (No Runtime Code)  
**Primary Purpose**: Global functional programming style guides and workflow rules for AI coding assistants  
**Target Users**: Cursor AI, Gemini, Claude Code, and other AI coding assistants  
**Languages Covered**: Python, TypeScript, Swift, Kotlin, Rust, Haskell (6 languages total)  

---

## 🎯 Project Overview

This is a **pure documentation repository** containing global rule sets and functional programming style guides for AI coding assistants. Unlike traditional software projects, this repository:

- ✅ Contains **no build system, dependencies, or runtime code**
- ✅ Distributes **104+ Markdown files** with comprehensive documentation
- ✅ Provides **portable rule sets** for Cursor AI and other tools
- ✅ Enforces **consistent functional programming patterns** across 6 languages
- ✅ Includes **project templates and real-world examples**

### Core Components

1. **`cursor/`** - Main Cursor AI rule set (70+ files)
   - `CURSOR.md` - Universal mandatory rules (Git, docs, testing, file size)
   - `CURSOR_FP_PRINCIPLES.md` - Functional programming deep dive
   - `CURSOR_WORKFLOW_GUIDE.md` - Git checkpoint and documentation workflow
   - `SETUP_GUIDE.md` - One-time machine setup instructions
   - `FILE_LOCATIONS_USER_GUIDE.md` - Documentation hierarchy and file placement
   - Language-specific FP guides (Python, TypeScript, Swift, Kotlin, Rust, Haskell)
   - `examples/` - Real-world project templates
   - `templates/` - Smart `.cursorrules` templates

2. **`gemini/`** - Gemini-specific rule set (parallel structure to `cursor/`)
   - Isolated Gemini-specific documentation
   - Same content organization as Cursor files

3. **`kimi/`** - Kimi CLI rule set (parallel structure to `cursor/`)
   - Isolated Kimi-specific documentation
   - Mirrors Cursor organization: `KIMI.md`, `KIMI_FP_PRINCIPLES.md`, `KIMI_WORKFLOW_GUIDE.md`
   - Language-specific FP guides (6 languages + 2 platforms)
   - `templates/` - Smart `.kimirules` templates with auto-detection
   - `examples/` - Real-world project templates demonstrating Kimi patterns
   - **Status**: ✅ Implementation complete (Phases 0-4)

4. **`docs/`** - Repository planning and tracking
   - `ARCHITECTURE_PLAN.md` - High-level project strategy
   - `plans/` - Feature-specific plans paired with TODO lists
   - `YYYY_MM_DD/` - Daily work logs following mandatory naming: `YYYYMMDD_NNNN_DESCRIPTIVE_NAME.md`

4. **Root documentation** - Project overview files
   - `README.md` - Main project documentation and quick start
   - `FP_GUIDE_README.md` - Functional programming guides overview
   - `MIGRATION_GUIDE.md` - Migration strategies for existing codebases

---

## 🏗️ Architecture & Technology Stack

### No Traditional Tech Stack

This repository **does not have**:
- ❌ Package managers (npm, pip, cargo, etc.)
- ❌ Build tools (webpack, make, cmake, etc.)
- ❌ Runtime dependencies or libraries
- ❌ Test frameworks or CI/CD pipelines
- ❌ Docker containers or deployment configurations
- ❌ Source code files (`.py`, `.ts`, `.rs`, etc.)

### Pure Documentation Architecture

- **Format**: Markdown (`.md`) and Cursor rules (`.cursorrules`, `.mdc`)
- **Version Control**: Git with MANDATORY checkpoint strategy
- **Distribution**: Git repository, environment variables, or git submodules
- **Structure**: Hierarchical documentation with cross-references

### Key Files and Their Purpose

| File | Purpose | AI Agent Action |
|------|---------|-----------------|
| `cursor/CURSOR.md` | Universal mandatory rules for ALL projects | **READ FIRST** - Understand Git checkpoints, docs hierarchy, testing |
| `cursor/SETUP_GUIDE.md` | Machine setup for end users | Reference when explaining setup to users |
| `cursor/*-fp-style-guide.md` | Language-specific FP patterns | Use when generating code in specific languages |
| `cursor/examples/*` | Real-world project templates | Copy and adapt for user projects |
| `.cursorrules` | Rules for THIS repository | Follow when editing this repo |
| `docs/plans/*_TODO.md` | Task tracking for features | Check and update when implementing features |

---

## 📋 Mandatory Project Conventions (CRITICAL FOR AI AGENTS)

### 1. Git Workflow (MANDATORY - Must Follow Exactly)

**Checkpoint Triggers** - Commit MUST occur after:
- ✅ **Every 30-60 minutes** during active work (maximum time between commits)
- ✅ **Completing a phase** (Phase 0, Phase 1, etc.)
- ✅ **Creating core documents** (major deliverables)
- ✅ **Completing logical units** (set of related templates, examples)
- ✅ **Before context switch** (switching between work areas)
- ✅ **After bug fixes** (any fix that resolves an issue)
- ✅ **After documentation updates** (significant changes)

**Commit Message Format** (All elements required):
```
<Brief summary (50-72 chars)>

<Detailed description of what was done>

<Why it was done (rationale)>

<What deliverables/files were affected>

<Any relevant context or decisions>

Status: <Current state>

🤖 Generated with Cursor
Co-Authored-By: Claude <noreply@anthropic.com>
```

**Why This Matters**:
- Prevents work loss from crashes or errors
- Creates clear project history for review
- Enables safe reversion if needed
- Preserves context across AI agent sessions
- Demonstrates thoroughness and reliability

### 2. Documentation Structure (MANDATORY Hierarchy)

**Three-Tier Structure** (must follow exactly):

1. **ARCHITECTURE_PLAN.md** (Root level)
   - High-level strategy and goals
   - Links to sub-plans
   - Overall project roadmap

2. **`docs/plans/FEATURE_NAME_PLAN.md`** (Strategic plans)
   - Specific feature implementation plans
   - Paired with `FEATURE_NAME_TODO.md`
   - Links to daily work logs

3. **`docs/YYYY_MM_DD/YYYYMMDD_NNNN_NAME.md`** (Daily execution)
   - **Naming format: `YYYYMMDD_NNNN_DESCRIPTIVE_NAME.md`**
   - NNNN = 4-digit sequence (0000, 0001, 0002...)
   - Sequential daily work documentation
   - Example: `20251101_0000_PHASE_3_COMPLETE_SWIFT.md`

**Cross-References**: All documents must link to related plans, TODOs, and daily logs

### 3. File Size Limits (MANDATORY)

- **Target**: 250-300 lines per file
- **Absolute Maximum**: 350 lines
- **Action if exceeded**: Split into modules with clear naming
- **Exceptions**: Document why exception was made

**Rationale**: Small files are easier to maintain, review, and understand

### 4. Testing Requirements (For Documentation)

Before committing:
1. ✅ **Read all created/modified files** to verify content quality
2. ✅ **Check markdown formatting** (headers, lists, code blocks)
3. ✅ **Verify all references/links work**
4. ✅ **Ensure consistent style and terminology**
5. ✅ **Test any templates** (copy to test project, verify they work)


---

**Part 1 of 3** | Next: **[AGENTS_WORKFLOWS.md](AGENTS_WORKFLOWS.md)** | Also see: **[AGENTS_REFERENCE.md](AGENTS_REFERENCE.md)**

**Last Updated**: 2025-11-14
**Version**: 1.0.1
**Status**: Production Ready
