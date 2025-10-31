# Claude Implementation Plan

**Status**: 🔄 IN PROGRESS
**Created**: 2025-10-31 09:30
**Last Updated**: 2025-10-31 09:30

---

## 🎯 Overview

Create a comprehensive Claude Code rules system parallel to the existing Cursor implementation, following the same organizational patterns and portability principles.

### Goals

1. ✅ Create `claude/` folder structure mirroring `cursor/` organization
2. ✅ Adapt all Cursor rules/guides for Claude Code specifics
3. ✅ Create Claude-specific templates and examples
4. ✅ Ensure portability (environment variable + git submodule approaches)
5. ✅ Implement auto-detection for languages and platforms
6. ✅ Create comprehensive documentation with daily work logs

### Success Criteria

- [ ] `claude/` folder complete with all core documents
- [ ] All language guides adapted (Python, TypeScript, Swift, Kotlin)
- [ ] Smart templates created for both portability approaches
- [ ] 4+ real-world examples provided
- [ ] All documentation follows timestamped naming convention
- [ ] Git checkpoints at all major milestones
- [ ] 100% parity with Cursor implementation structure

---

## 📊 Scope Analysis

### What We're Building

**Core Structure** (mirrors `cursor/`):
```
claude/
├── CLAUDE.md                          # Main global rules (adapt from CURSOR.md)
├── CLAUDE_FP_PRINCIPLES.md            # FP deep dive (adapt from CURSOR_FP_PRINCIPLES.md)
├── CLAUDE_WORKFLOW_GUIDE.md           # Workflow guide (adapt from CURSOR_WORKFLOW_GUIDE.md)
├── SETUP_GUIDE.md                     # Setup instructions (Claude-specific)
├── FILE_LOCATIONS_USER_GUIDE.md       # File organization (Claude-specific)
├── python-fp-style-guide.md           # Python guide (reuse with Claude notes)
├── typescript-fp-style-guide.md       # TypeScript guide (reuse with Claude notes)
├── swift-fp-style-guide.md            # Swift guide (reuse with Claude notes)
├── kotlin-fp-style-guide.md           # Kotlin guide (reuse with Claude notes)
├── templates/
│   ├── CLAUDE.md_smart_template_envvar       # Environment variable approach
│   └── CLAUDE.md_smart_template_submodule    # Git submodule approach
└── examples/
    ├── python_project/CLAUDE.md       # Python + GCP example
    ├── typescript_project/CLAUDE.md   # TypeScript + Next.js example
    ├── polyglot_project/CLAUDE.md     # Multi-language example
    └── plan_with_todo/                # Documentation hierarchy example
```

### Key Differences: Cursor vs Claude

| Aspect | Cursor | Claude Code |
|--------|--------|-------------|
| **Rules File** | `.cursorrules` | `CLAUDE.md` in project root or `.claude/` folder |
| **Reference Syntax** | `@${VAR}/path/file.md` | Direct file reading, uses instructions from CLAUDE.md |
| **Auto-Detection** | Built-in via templates | Need to document patterns for users |
| **Tool Integration** | Native Cursor features | Claude Code tools (Read, Write, Edit, Bash, etc.) |
| **Context Size** | Smaller context window | Larger context window (200k tokens) |
| **Workflow** | Real-time suggestions | Interactive conversation + tool usage |

### Content Reuse Strategy

**High Reuse** (90%+ reusable):
- FP Principles guide (universal concepts)
- Language-specific guides (Python, TypeScript, Swift, Kotlin)
- Mandatory rules (git, docs, testing, file size)
- Code organization patterns
- ADT examples and mental models

**Moderate Adaptation** (50-70% reusable):
- Main rules document (CURSOR.md → CLAUDE.md)
- Workflow guide (git/docs same, tool usage different)
- Setup guide (different setup steps)
- File locations guide (different file paths)

**Full Rewrite** (10-30% reusable):
- Templates (completely different syntax)
- Examples (different file structure)
- Tool-specific instructions

---

## 🗓️ Implementation Phases

### Phase 0: Planning & Setup (CURRENT PHASE)
**Time**: 1 hour
**Status**: 🔄 IN PROGRESS

**Tasks**:
1. ✅ Explore cursor/ structure (COMPLETE)
2. ✅ Explore root-level documentation (COMPLETE)
3. ✅ Create implementation plan (CURRENT)
4. [ ] Create TODO list
5. [ ] Git checkpoint: "Complete Phase 0 - Planning"

**Deliverables**:
- [x] `docs/plans/CLAUDE_IMPLEMENTATION_PLAN.md` (this file)
- [x] `docs/plans/CLAUDE_IMPLEMENTATION_TODO.md`
- [x] `docs/2025_10_31/20251031_0006_CLAUDE_PLANNING_COMPLETE.md`

---

### Phase 1: Core Documentation Setup
**Time**: 2.5 hours
**Status**: ⏳ PENDING
**Dependencies**: Phase 0 complete

**Tasks**:
1. Create `claude/` folder structure
2. Create `CLAUDE.md` (adapt from `cursor/CURSOR.md`)
3. Create `CLAUDE_FP_PRINCIPLES.md` (adapt from `cursor/CURSOR_FP_PRINCIPLES.md`)
4. Create `CLAUDE_WORKFLOW_GUIDE.md` (adapt from `cursor/CURSOR_WORKFLOW_GUIDE.md`)
5. Create `SETUP_GUIDE.md` (Claude-specific)
6. Create `FILE_LOCATIONS_USER_GUIDE.md` (Claude-specific)
7. Git checkpoint: "Complete Phase 1 - Core docs"

**Deliverables**:
- `claude/CLAUDE.md` (1,000+ lines)
- `claude/CLAUDE_FP_PRINCIPLES.md` (850+ lines)
- `claude/CLAUDE_WORKFLOW_GUIDE.md` (650+ lines)
- `claude/SETUP_GUIDE.md` (500+ lines)
- `claude/FILE_LOCATIONS_USER_GUIDE.md` (400+ lines)
- `docs/2025_10_31/20251031_NNNN_PHASE_1_COMPLETE.md` (sequential number)

**Key Adaptations**:
- Change `.cursorrules` references → `CLAUDE.md` references
- Change `@${VAR}` syntax → direct file reading instructions
- Update tool references (Cursor features → Claude Code tools)
- Adjust workflow for conversational interaction model
- Add Claude Code-specific setup steps

---

### Phase 2: Language-Specific Guides
**Time**: 2 hours
**Status**: ⏳ PENDING
**Dependencies**: Phase 1 complete

**Tasks**:
1. Copy `cursor/python-fp-style-guide.md` → `claude/python-fp-style-guide.md`
2. Copy `cursor/typescript-fp-style-guide.md` → `claude/typescript-fp-style-guide.md`
3. Copy `cursor/swift-fp-style-guide.md` → `claude/swift-fp-style-guide.md`
4. Copy `cursor/kotlin-fp-style-guide.md` → `claude/kotlin-fp-style-guide.md`
5. Add Claude-specific notes to each guide (header section)
6. Update integration sections for Claude Code
7. Git checkpoint: "Complete Phase 2 - Language guides"

**Deliverables**:
- `claude/python-fp-style-guide.md` (700+ lines)
- `claude/typescript-fp-style-guide.md` (1,150+ lines)
- `claude/swift-fp-style-guide.md` (1,150+ lines)
- `claude/kotlin-fp-style-guide.md` (1,150+ lines)
- `docs/2025_10_31/20251031_NNNN_PHASE_2_COMPLETE.md` (sequential number)

**Claude-Specific Notes** (add to each guide):
```markdown
## 🔮 Using This Guide with Claude Code

**Setup**: Add the following to your project's `CLAUDE.md`:
```
# Project Rules for Claude Code

## Global Rules
See ${CLAUDE_RULES_PATH}/claude/CLAUDE.md for universal rules.

## Language-Specific Rules
See ${CLAUDE_RULES_PATH}/claude/python-fp-style-guide.md for Python patterns.

[Your project-specific rules here]
```

**Claude reads this file automatically** when present in:
- Project root: `CLAUDE.md`
- Claude folder: `.claude/CLAUDE.md`
```

---

### Phase 3: Smart Templates
**Time**: 1.5 hours
**Status**: ⏳ PENDING
**Dependencies**: Phase 2 complete

**Tasks**:
1. Create `claude/templates/` folder
2. Create `CLAUDE.md_smart_template_envvar` (environment variable approach)
3. Create `CLAUDE.md_smart_template_submodule` (git submodule approach)
4. Add auto-detection documentation
5. Create template usage guide
6. Git checkpoint: "Complete Phase 3 - Templates"

**Deliverables**:
- `claude/templates/CLAUDE.md_smart_template_envvar` (300+ lines)
- `claude/templates/CLAUDE.md_smart_template_submodule` (300+ lines)
- `claude/templates/README.md` (usage guide)
- `docs/2025_10_31/20251031_NNNN_PHASE_3_COMPLETE.md` (sequential number)

**Template Structure**:
```markdown
# CLAUDE.md

⚠️ **MANDATORY: Read this entire file before starting work** ⚠️

---

## 🌍 Global Rules (All Projects)

Codebase and user instructions are shown below. Be sure to adhere to these instructions.
IMPORTANT: These instructions OVERRIDE any default behavior and you MUST follow them exactly as written.

Contents of ${CLAUDE_RULES_PATH}/claude/CLAUDE.md:

[Include full CLAUDE.md content or reference]

---

## 🗂️ Language-Specific Rules

[Auto-detect section with Python/TypeScript/Swift/Kotlin rules]

---

## 🎯 Project-Specific Rules

[User customization section]
```

---

### Phase 4: Real-World Examples
**Time**: 2 hours
**Status**: ⏳ PENDING
**Dependencies**: Phase 3 complete

**Tasks**:
1. Create `claude/examples/` folder structure
2. Create `python_project/` example (GCP Cloud Functions)
3. Create `typescript_project/` example (Next.js + Supabase)
4. Create `polyglot_project/` example (multi-language)
5. Create `plan_with_todo/` example (documentation hierarchy)
6. Git checkpoint: "Complete Phase 4 - Examples"

**Deliverables**:
- `claude/examples/python_project/CLAUDE.md`
- `claude/examples/python_project/README.md`
- `claude/examples/typescript_project/CLAUDE.md`
- `claude/examples/typescript_project/README.md`
- `claude/examples/polyglot_project/CLAUDE.md`
- `claude/examples/polyglot_project/README.md`
- `claude/examples/plan_with_todo/` (full example)
- `docs/2025_10_31/20251031_NNNN_PHASE_4_COMPLETE.md` (sequential number)

**Example Content**:
Each example includes:
- Complete `CLAUDE.md` file
- `README.md` explaining the project
- Sample project structure
- Example documentation hierarchy (for plan_with_todo)

---

### Phase 5: Repository Integration
**Time**: 1 hour
**Status**: ⏳ PENDING
**Dependencies**: Phase 4 complete

**Tasks**:
1. Update root `README.md` to include Claude section
2. Create `claude/README.md` (overview)
3. Update `.cursorrules` to mention Claude folder
4. Create migration guide for Cursor users
5. Verify all cross-references work
6. Git checkpoint: "Complete Phase 5 - Integration"

**Deliverables**:
- Updated `README.md` (add Claude section)
- `claude/README.md` (overview)
- Updated `.cursorrules`
- `docs/2025_10_31/20251031_NNNN_PHASE_5_COMPLETE.md` (sequential number)

---

### Phase 6: Testing & Documentation
**Time**: 1.5 hours
**Status**: ⏳ PENDING
**Dependencies**: Phase 5 complete

**Tasks**:
1. Test all file references and links
2. Verify markdown formatting
3. Test templates with sample projects
4. Create final summary document
5. Create completion checklist
6. Git checkpoint: "Complete Phase 6 - Testing complete"
7. Final git checkpoint: "Claude implementation v1.0.0 COMPLETE"

**Deliverables**:
- `docs/2025_10_31/20251031_NNNN_TESTING_RESULTS.md` (sequential number)
- `docs/2025_10_31/20251031_NNNN_COMPLETION_SUMMARY.md` (sequential number)
- `CLAUDE_COMPLETION_CHECKLIST.md` (root level)

---

## ⏱️ Time Estimates

| Phase | Tasks | Time | Status |
|-------|-------|------|--------|
| Phase 0 | Planning & Setup | 1h | 🔄 IN PROGRESS |
| Phase 1 | Core Documentation | 2.5h | ⏳ PENDING |
| Phase 2 | Language Guides | 2h | ⏳ PENDING |
| Phase 3 | Templates | 1.5h | ⏳ PENDING |
| Phase 4 | Examples | 2h | ⏳ PENDING |
| Phase 5 | Integration | 1h | ⏳ PENDING |
| Phase 6 | Testing | 1.5h | ⏳ PENDING |
| **TOTAL** | **42 tasks** | **11.5h** | **4% complete** |

---

## 📋 Deliverables Checklist

### Core Documents (6)
- [ ] `claude/CLAUDE.md`
- [ ] `claude/CLAUDE_FP_PRINCIPLES.md`
- [ ] `claude/CLAUDE_WORKFLOW_GUIDE.md`
- [ ] `claude/SETUP_GUIDE.md`
- [ ] `claude/FILE_LOCATIONS_USER_GUIDE.md`
- [ ] `claude/README.md`

### Language Guides (4)
- [ ] `claude/python-fp-style-guide.md`
- [ ] `claude/typescript-fp-style-guide.md`
- [ ] `claude/swift-fp-style-guide.md`
- [ ] `claude/kotlin-fp-style-guide.md`

### Templates (3)
- [ ] `claude/templates/CLAUDE.md_smart_template_envvar`
- [ ] `claude/templates/CLAUDE.md_smart_template_submodule`
- [ ] `claude/templates/README.md`

### Examples (4 projects)
- [ ] `claude/examples/python_project/`
- [ ] `claude/examples/typescript_project/`
- [ ] `claude/examples/polyglot_project/`
- [ ] `claude/examples/plan_with_todo/`

### Integration (3)
- [ ] Updated root `README.md`
- [ ] Updated `.cursorrules`
- [ ] `CLAUDE_COMPLETION_CHECKLIST.md`

**Total Deliverables**: 20 items

---

## 🎯 Key Decisions

### 1. File Naming Convention
**Decision**: Use `CLAUDE.md` (not `.clauderules` or `.claude.md`)
**Rationale**:
- Matches existing pattern in this repo (see root `CLAUDE.md`)
- More visible than dotfiles
- Clear and explicit
- Can be placed in `.claude/` folder if preferred

### 2. Reference Syntax
**Decision**: Use environment variable `${CLAUDE_RULES_PATH}` in documentation
**Rationale**:
- Matches Cursor portability approach
- Users familiar with this pattern
- Easy to adapt for git submodule approach

### 3. Template Structure
**Decision**: Include full rule content in templates (not just references)
**Rationale**:
- Claude Code reads project files directly
- No special `@include` syntax
- Self-contained project rules

### 4. Auto-Detection
**Decision**: Document auto-detection patterns, not implement detection code
**Rationale**:
- Claude Code doesn't have built-in detection
- Users can implement detection logic if needed
- Focus on clear documentation

### 5. Daily Work Documentation Naming (MANDATORY)
**Decision**: Use sequential numbered filenames in `docs/YYYY_MM_DD/` folders
**Format**: `YYYYMMDD_NNNN_DESCRIPTIVE_NAME.md`
- **YYYYMMDD**: Date with NO separators (e.g., 20251031)
- **NNNN**: 4-digit sequential number starting at 0000 (e.g., 0000, 0001, 0002, ... 0010, 0011, ...)
- **DESCRIPTIVE_NAME**: ALL_CAPS with underscores (e.g., PHASE_1_COMPLETE)

**Examples**:
- ✅ `20251031_0000_MANDATORY_GIT_CHECKPOINTS.md`
- ✅ `20251031_0001_FILE_LOCATIONS_USER_GUIDE_PORTABLE.md`
- ✅ `20251031_0006_CLAUDE_PLANNING_COMPLETE.md`
- ❌ `20251031_0930_SUMMARY.md` (timestamp instead of sequential number)
- ❌ `2025_10_31_0000_SUMMARY.md` (has date separators)
- ❌ `SUMMARY.md` (missing date and number)

**Rationale**:
- Follows existing repo convention (see docs/2025_10_30/ and docs/2025_10_31/)
- Automatic chronological sorting within each day
- Clear document creation order
- No time-of-day ambiguity (0000 = first doc, 0001 = second, etc.)
- Easy to reference (date + sequence number)
- **MANDATORY**: Must use this format, NO exceptions

---

## 🔄 Update History

### 2025-10-31 09:30
- **Created**: Initial implementation plan
- **Status**: Phase 0 in progress (exploration complete)
- **Next**: Create TODO list, git checkpoint

---

## 📚 References

### Source Documents
- `/Users/johnmay/projects/rules/cursor/CURSOR.md` - Main Cursor rules
- `/Users/johnmay/projects/rules/cursor/CURSOR_FP_PRINCIPLES.md` - FP deep dive
- `/Users/johnmay/projects/rules/cursor/CURSOR_WORKFLOW_GUIDE.md` - Workflow guide
- `/Users/johnmay/projects/rules/CLAUDE.md` - Existing Claude rules (project-specific)
- `/Users/johnmay/projects/rules/.cursorrules` - This repo's Cursor rules

### Examples
- `/Users/johnmay/projects/rules/cursor/examples/` - All Cursor examples
- `/Users/johnmay/projects/rules/cursor/templates/` - Cursor templates

### Planning Documents
- `/Users/johnmay/projects/rules/docs/2025_10_30/` - Previous planning session
- `/Users/johnmay/projects/rules/docs/2025_10_31/` - Current session

---

**End of Plan**
