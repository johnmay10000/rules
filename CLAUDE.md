# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

---

## ⚠️ MANDATORY REQUIREMENTS - READ FIRST ⚠️

These requirements are **NON-NEGOTIABLE** and must be followed **AT ALL TIMES**:

### 1. GIT CHECKPOINTS (MANDATORY)

**CRITICAL**: Git commits are MANDATORY at these checkpoints:

**Checkpoint Triggers (MUST commit after)**:
1. ✅ **Completing a phase** - Every phase completion (Phase 0, Phase 1, etc.)
2. ✅ **Creating core documents** - CLAUDE.md, SETUP_GUIDE.md, major deliverables
3. ✅ **Completing logical units** - Set of related templates, set of examples
4. ✅ **Before context switch** - Switching between different work areas
5. ✅ **Bug fixes** - Any fix that resolves an issue
6. ✅ **Documentation updates** - Significant doc changes
7. ✅ **Every 30-60 minutes** - Maximum time between commits during active work

**Commit Message Format (REQUIRED)**:
```
<Brief summary line (50-72 chars)>

<Detailed description of what was done>

<Why it was done (rationale)>

<What deliverables/files were affected>

<Any relevant context or decisions>

Status: <Current state>

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: Claude <noreply@anthropic.com>
```

**Good Checkpoint Example**:
```
Complete Phase 0: Claude implementation planning

Created comprehensive planning documents for Claude Code rules system:

- CLAUDE_IMPLEMENTATION_PLAN.md (500+ lines)
  - 6 implementation phases (0-6)
  - 42 tasks with time estimates (11.5h total)
  - 20 deliverables defined
  - Key decisions documented

Planning identified:
- High reuse content (90%+ for FP guides)
- Moderate adaptation needed (50-70% for main docs)

Ready to proceed to Phase 1: Core Documentation Setup

Status: Phase 0 complete ✅

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: Claude <noreply@anthropic.com>
```

**Anti-Patterns (DO NOT DO)**:
- ❌ Too vague: "Update files"
- ❌ No context: "WIP"
- ❌ Multiple unrelated changes in one commit
- ❌ Waiting 2+ hours without commit

**Checkpoint Strategy**:
- Each core document → separate commit
- Each template set → separate commit
- Each example set → separate commit
- Each phase → separate commit
- Documentation updates → separate commit

---

### 2. DOCUMENTATION NAMING (MANDATORY)

**CRITICAL**: Daily work documentation MUST follow this exact format:

**Format**: `YYYYMMDD_NNNN_DESCRIPTIVE_NAME.md`

**Components**:
- **YYYYMMDD**: Date with NO separators (e.g., `20251031`)
- **NNNN**: 4-digit sequential number starting at 0000 (e.g., `0000`, `0001`, `0002`, ...)
- **DESCRIPTIVE_NAME**: ALL_CAPS with underscores (e.g., `PHASE_1_COMPLETE`)

**Examples**:
- ✅ `20251031_0000_MANDATORY_GIT_CHECKPOINTS.md`
- ✅ `20251031_0006_CLAUDE_PLANNING_COMPLETE.md`
- ✅ `20251031_0007_MANDATORY_NAMING_CONVENTION.md`
- ❌ `20251031_0930_SUMMARY.md` (timestamp instead of sequential)
- ❌ `2025_10_31_0000_SUMMARY.md` (has date separators)
- ❌ `SUMMARY.md` (missing date and number)

**Location**: `docs/YYYY_MM_DD/` (dated folders)
- `docs/2025_10_31/20251031_0000_*.md` (first doc of the day)
- `docs/2025_10_31/20251031_0001_*.md` (second doc of the day)
- Each day starts fresh at 0000

**Why This Matters**:
- Automatic chronological sorting within each day
- Clear document creation order (0000 = first, 0001 = second, etc.)
- No time-of-day ambiguity
- Easy to reference by date + sequence number
- **MANDATORY**: No exceptions allowed

**How to Determine Next Number**:
1. List files in current day's folder: `ls docs/2025_10_31/`
2. Find highest number (e.g., `20251031_0007_...`)
3. Increment by 1 (e.g., next is `0008`)
4. Use that number for new document

---

### 3. FILE ORGANIZATION (MANDATORY)

**Three-Tier Documentation Hierarchy**:

**TIER 1: Strategic** - `ARCHITECTURE_PLAN.md` (project root)
- High-level architecture and major components
- Updated rarely (major changes only)
- NO timestamp, NO sequence number

**TIER 2: Tactical** - `docs/plans/` folder
- Feature-specific plans: `FEATURE_NAME_PLAN.md`
- Paired TODO lists: `FEATURE_NAME_TODO.md`
- Updated regularly as work progresses
- NO timestamp, NO sequence number

**TIER 3: Execution** - `docs/YYYY_MM_DD/` folders
- Daily work logs, decisions, summaries
- Format: `YYYYMMDD_NNNN_DESCRIPTIVE_NAME.md` (MANDATORY)
- Updated multiple times per day
- **MUST use sequential numbers**

**Other Folders**:
- `cursor/` - Cursor-specific rules and templates (DO NOT MODIFY)
- `claude/` - Claude-specific rules and templates (TO BE CREATED)
- `templates/` - Reusable templates
- `examples/` - Real-world usage examples

---

### 4. TODO LIST MANAGEMENT (MANDATORY)

**Using TodoWrite Tool**:
- ✅ Use TodoWrite tool to track all tasks
- ✅ Mark tasks as `in_progress` when starting
- ✅ Mark tasks as `completed` when finished
- ✅ Update actual time spent on each task
- ✅ ONLY ONE task `in_progress` at a time

**Paired Plan and TODO**:
- Every plan document (`FEATURE_NAME_PLAN.md`) MUST have paired TODO (`FEATURE_NAME_TODO.md`)
- TODO lists updated after EACH task completion
- Add timestamped update history entries
- Update progress percentages

**Update Requirements**:
- Mark tasks complete [x] immediately after finishing
- Add actual time spent: `(Est: 2h, Actual: 1.5h)`
- Update progress percentages
- Add update history entry with timestamp

---

### 5. TESTING & VERIFICATION (MANDATORY)

**Before Every Git Commit**:
1. ✅ Read all created/modified files to verify content
2. ✅ Check for linter errors (if applicable)
3. ✅ Verify all references/links work
4. ✅ Ensure markdown formatting is correct
5. ✅ Test any templates created

**For Code Repositories** (not this repo):
- ✅ Run all relevant tests
- ✅ Ensure 100% tests passing
- ✅ Check code follows style guidelines
- ✅ Verify no breaking changes

---

## 🎯 Project-Specific Rules (This Repository)

### What This Repository Is

This is a **portable, standalone repository for rules and guidelines** to direct AI tools like Claude Code and Cursor.

**Structure**:
- `cursor/` - Complete Cursor AI rules system (DO NOT MODIFY)
- `claude/` - Claude Code rules system (TO BE CREATED)
- `docs/` - Planning and daily work documentation
- Root files - README, guides, examples

### Current Work: Claude Implementation

**Status**: Phase 0 complete ✅ (Planning and naming convention enforcement)

**Next**: Phase 1 - Core Documentation Setup (2.5 hours, 7 tasks)

**Plan Documents**:
- [CLAUDE_IMPLEMENTATION_PLAN.md](docs/plans/CLAUDE_IMPLEMENTATION_PLAN.md) - Full implementation plan
- [CLAUDE_IMPLEMENTATION_TODO.md](docs/plans/CLAUDE_IMPLEMENTATION_TODO.md) - Task breakdown

**Goal**: Create `claude/` folder structure mirroring `cursor/` organization, adapted for Claude Code specifics.

---

## 🔄 Standard Workflow

### Task Implementation Flow

1. **Read TODO list** → Identify next task
2. **Start task** → Use TodoWrite to mark `in_progress`
3. **Do work** → Create/modify files
4. **Verify** → Read files, check quality, verify links
5. **Complete task** → Use TodoWrite to mark `completed`, add actual time
6. **Git checkpoint** → Commit with descriptive message
7. **Repeat** → Next task

### Multi-File Work

For tasks creating multiple related files:
- Complete all related files first
- Verify all files work together
- Single commit for the logical unit
- But don't wait too long (30-60 min max)

### Phase Completion

1. Complete all tasks in phase
2. Update TODO list (mark phase complete)
3. Create phase summary document (in `docs/YYYY_MM_DD/`)
4. Git checkpoint with "Phase X complete" message
5. Brief pause/review before next phase

---

## 📁 Folder Structure (This Repo)

```
/Users/johnmay/projects/rules/
├── CLAUDE.md                          # This file (Claude-specific rules)
├── README.md                          # Repository overview
├── .cursorrules                       # Cursor-specific rules
├── cursor/                            # Cursor rules system (DO NOT MODIFY)
│   ├── CURSOR.md                      # Main Cursor rules
│   ├── CURSOR_FP_PRINCIPLES.md        # FP deep dive
│   ├── CURSOR_WORKFLOW_GUIDE.md       # Workflow guide
│   ├── templates/                     # Cursor templates
│   └── examples/                      # Cursor examples
├── claude/                            # Claude rules system (TO BE CREATED)
│   ├── CLAUDE.md                      # Main Claude rules (planned)
│   ├── CLAUDE_FP_PRINCIPLES.md        # FP deep dive (planned)
│   ├── templates/                     # Claude templates (planned)
│   └── examples/                      # Claude examples (planned)
├── docs/
│   ├── plans/                         # Tier 2: Tactical plans
│   │   ├── CLAUDE_IMPLEMENTATION_PLAN.md
│   │   └── CLAUDE_IMPLEMENTATION_TODO.md
│   ├── 2025_10_30/                    # Tier 3: Daily work (Oct 30)
│   └── 2025_10_31/                    # Tier 3: Daily work (Oct 31)
└── examples/                          # Shared examples
```

---

## 🚨 DO NOT MODIFY

**Protected Folders/Files**:
- ❌ `cursor/` - All files (Cursor-specific, already complete)
- ❌ `cursor/CURSOR.md` - Main Cursor rules
- ❌ `cursor/templates/` - Cursor templates
- ❌ `cursor/examples/` - Cursor examples

**Only Modify**:
- ✅ `claude/` - Claude-specific files (create new)
- ✅ `docs/plans/` - Planning documents
- ✅ `docs/YYYY_MM_DD/` - Daily work documents
- ✅ `CLAUDE.md` - This file (if needed)
- ✅ Root files like `README.md` (for Claude integration)

---

## 🎯 Current Task Context

**Active Plan**: [CLAUDE_IMPLEMENTATION_PLAN.md](docs/plans/CLAUDE_IMPLEMENTATION_PLAN.md)
**Active TODO**: [CLAUDE_IMPLEMENTATION_TODO.md](docs/plans/CLAUDE_IMPLEMENTATION_TODO.md)

**Completed Phases**:
- ✅ Phase 0: Planning & Setup (1h) - Planning documents + naming convention

**Current Phase**: Phase 1 - Core Documentation Setup (2.5h, 7 tasks)
1. Create `claude/` folder structure
2. Create `CLAUDE.md` main rules (adapt from `cursor/CURSOR.md`)
3. Create `CLAUDE_FP_PRINCIPLES.md` (adapt from `cursor/CURSOR_FP_PRINCIPLES.md`)
4. Create `CLAUDE_WORKFLOW_GUIDE.md` (adapt from `cursor/CURSOR_WORKFLOW_GUIDE.md`)
5. Create `SETUP_GUIDE.md` (Claude-specific)
6. Create `FILE_LOCATIONS_USER_GUIDE.md` (Claude-specific)
7. Git checkpoint: "Complete Phase 1"

**Progress**: 4% complete (2/42 tasks done)

---

## 🔑 Key Decisions Made

### 1. Naming Convention
- **Decision**: Use sequential numbers (`YYYYMMDD_NNNN_`) NOT timestamps
- **Rationale**: Matches existing repo pattern, clear document order
- **Status**: MANDATORY and enforced

### 2. Folder Isolation
- **Decision**: Separate `cursor/` and `claude/` folders
- **Rationale**: Clean separation, no conflicts, easy to maintain
- **Status**: cursor/ complete, claude/ to be created

### 3. Content Reuse
- **High reuse** (90%+): FP principles, language guides
- **Moderate adaptation** (50-70%): Main rules, workflow guides
- **Full rewrite** (10-30%): Templates, tool-specific instructions

---

## 📚 Reference Documents

**Planning**:
- [CLAUDE_IMPLEMENTATION_PLAN.md](docs/plans/CLAUDE_IMPLEMENTATION_PLAN.md) - Complete implementation plan
- [CLAUDE_IMPLEMENTATION_TODO.md](docs/plans/CLAUDE_IMPLEMENTATION_TODO.md) - Task breakdown

**Naming Convention**:
- [20251031_0007_MANDATORY_NAMING_CONVENTION.md](docs/2025_10_31/20251031_0007_MANDATORY_NAMING_CONVENTION.md) - Comprehensive guide

**Completed Work**:
- [20251031_0006_CLAUDE_PLANNING_COMPLETE.md](docs/2025_10_31/20251031_0006_CLAUDE_PLANNING_COMPLETE.md) - Phase 0 summary

---

## 🤖 About This File

This `CLAUDE.md` file ensures Claude Code applies consistent rules when working on the global rules repository itself. It follows the same patterns we're creating for other projects.

**Last Updated**: 2025-10-31
**Status**: Active
**Applies To**: This repository only (project-specific rules for Claude Code)
**Parallel File**: `.cursorrules` (for Cursor AI)

---

## 💡 Quick Tips for Claude Code

1. **Always check TodoWrite** before starting work
2. **One task at a time** (mark in_progress, complete, then next)
3. **Git checkpoint every 30-60 min** or after major milestones
4. **Sequential filenames** for daily work (check existing files first)
5. **Read before modifying** - verify content before changes
6. **DO NOT modify cursor/ folder** - it's complete and protected
7. **Follow the plan** - refer to CLAUDE_IMPLEMENTATION_PLAN.md
8. **Update TODO lists** immediately after completing tasks

---

**Ready to work!** Check the TODO list and start Phase 1 when approved.
