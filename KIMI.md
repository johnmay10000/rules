# KIMI.md

This file provides guidance to Kimi CLI when working with code in this repository.

---

## ⚠️ MANDATORY REQUIREMENTS - READ FIRST ⚠️

These requirements are **NON-NEGOTIABLE** and must be followed **AT ALL TIMES**:

### 1. GIT CHECKPOINTS (MANDATORY)

**CRITICAL**: Git commits are MANDATORY at these checkpoints:

**Checkpoint Triggers (MUST commit after)**:
1. ✅ **Completing a phase** - Every phase completion (Phase 0, Phase 1, etc.)
2. ✅ **Creating core documents** - KIMI.md, SETUP_GUIDE.md, major deliverables
3. ✅ **Completing logical units** - Set of related templates, set of examples
4. ✅ **Before context switch** - Switching between different work areas
5. ✅ **Bug fixes** - Any fix that resolves an issue
6. ✅ **Documentation updates** - Significant doc changes
7. ✅ **Every 30-60 minutes** - Maximum time between commits during active work

**Commit Message Format (REQUIRED)** (Kimi-specific version):
```
<Brief summary line (50-72 chars)>

<Detailed description of what was done>

<Why it was done (rationale)>

<What deliverables/files were affected>

<Any relevant context or decisions>

Status: <Current state>

🤖 Generated with [Kimi](https://kimi.ai)
```

**Good Checkpoint Example**:
```
Complete Phase 0: Kimi implementation planning

Created comprehensive planning documents for Kimi CLI rules system:

- KIMI_IMPLEMENTATION_PLAN.md (500+ lines)
  - 6 implementation phases (0-6)
  - 42 tasks with time estimates (11.5h total)
  - 20 deliverables defined
  - Key decisions documented

Planning identified:
- High reuse content (90%+ for FP guides)
- Moderate adaptation needed (50-70% for main docs)

Ready to proceed to Phase 1: Core Documentation Setup

Status: Phase 0 complete ✅

🤖 Generated with [Kimi](https://kimi.ai)
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
- **YYYYMMDD**: Date with NO separators (e.g., `20251114`)
- **NNNN**: 4-digit sequential number starting at 0000 (e.g., `0000`, `0001`, `0002`, ...)
- **DESCRIPTIVE_NAME**: ALL_CAPS with underscores (e.g., `PHASE_1_COMPLETE`)

**Examples**:
- ✅ `20251114_0000_MANDATORY_GIT_CHECKPOINTS.md`
- ✅ `20251114_0006_KIMI_PLANNING_COMPLETE.md`
- ✅ `20251114_0007_MANDATORY_NAMING_CONVENTION.md`
- ❌ `20251114_0930_SUMMARY.md` (timestamp instead of sequential)
- ❌ `2025_11_14_0000_SUMMARY.md` (has date separators)
- ❌ `SUMMARY.md` (missing date and number)

**Location**: `docs/YYYY_MM_DD/` (dated folders)
- `docs/2025_11_14/20251114_0000_*.md` (first doc of the day)
- `docs/2025_11_14/20251114_0001_*.md` (second doc of the day)
- Each day starts fresh at 0000

**Why This Matters**:
- Automatic chronological sorting within each day
- Clear document creation order (0000 = first, 0001 = second, etc.)
- No time-of-day ambiguity
- Easy to reference by date + sequence number
- **MANDATORY**: No exceptions allowed

**How to Determine Next Number**:
1. List files in current day's folder: `ls docs/2025_11_14/`
2. Find highest number (e.g., `20251114_0007_...`)
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
- `claude/` - Claude Code rules and templates (DO NOT MODIFY)
- `kimi/` - Kimi-specific rules and templates (YOUR AREA)
- `templates/` - Reusable templates
- `examples/` - Real-world usage examples

---

### 4. TODO LIST MANAGEMENT (MANDATORY)

**Using SetTodoList Tool**:
- ✅ Use SetTodoList tool to track all tasks
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

This is a **portable, standalone repository for rules and guidelines** to direct AI tools like Cursor AI, Claude Code, and Kimi CLI.

**Structure**:
- `cursor/` - Complete Cursor AI rules system (DO NOT MODIFY)
- `claude/` - Complete Claude Code rules system (DO NOT MODIFY)
- `kimi/` - Kimi CLI rules system (YOUR AREA - TO BE CREATED)
- `docs/` - Planning and daily work documentation
- Root files - README, guides, examples

### Current Work: Kimi Implementation

**Status**: Phase 0 in progress (Planning and naming convention enforcement)

**Next**: Phase 1 - Core Documentation Setup (2.5 hours, 7 tasks)

**Plan Documents**:
- [KIMI_IMPLEMENTATION_PLAN.md](docs/plans/KIMI_IMPLEMENTATION_PLAN.md) - Full implementation plan (to create)
- [KIMI_IMPLEMENTATION_TODO.md](docs/plans/KIMI_IMPLEMENTATION_TODO.md) - Task breakdown (to create)

**Goal**: Create `kimi/` folder structure mirroring `cursor/` organization, adapted for Kimi CLI specifics.

**Kimi-Specific Considerations**:
- Kimi uses tool-based architecture (Bash, ReadFile, WriteFile, SetTodoList, etc.)
- Follows similar patterns to Claude Code but with Kimi-specific tool usage
- Emphasize Kimi's strengths: parallel tool calls, task delegation via Subagents
- Balance between Cursor's template-heavy approach and Claude's command-heavy approach

---

## 🔄 Standard Workflow

### Task Implementation Flow

1. **Read TODO list** → Identify next task
2. **Start task** → Use SetTodoList to mark task as `in_progress`
3. **Do work** → Create/modify files
4. **Verify** → Read files, check quality, verify links
5. **Complete task** → Use SetTodoList to mark as `completed`, add actual time
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
├── KIMI.md                            # This file (Kimi-specific rules)
├── README.md                          # Repository overview
├── .cursorrules                       # Cursor-specific rules
├── CLAUDE.md                          # Claude-specific rules
├── cursor/                            # Cursor rules system (DO NOT MODIFY)
│   ├── CURSOR.md                      # Main Cursor rules
│   ├── CURSOR_FP_PRINCIPLES.md        # FP deep dive
│   ├── CURSOR_WORKFLOW_GUIDE.md       # Workflow guide
│   ├── templates/                     # Cursor templates
│   └── examples/                      # Cursor examples
├── claude/                            # Claude rules system (DO NOT MODIFY)
│   ├── CLAUDE.md                      # Main Claude rules
│   ├── CLAUDE_FP_PRINCIPLES.md        # FP deep dive
│   ├── CLAUDE_WORKFLOW_GUIDE.md       # Workflow guide
│   ├── templates/                     # Claude templates
│   └── examples/                      # Claude examples
├── kimi/                              # Kimi rules system (TO BE CREATED)
│   ├── KIMI.md                        # Main Kimi rules (planned)
│   ├── KIMI_FP_PRINCIPLES.md          # FP deep dive (planned)
│   ├── KIMI_WORKFLOW_GUIDE.md         # Workflow guide (planned)
│   ├── templates/                     # Kimi templates (planned)
│   └── examples/                      # Kimi examples (planned)
├── docs/
│   ├── plans/                         # Tier 2: Tactical plans
│   │   ├── KIMI_IMPLEMENTATION_PLAN.md (to create)
│   │   └── KIMI_IMPLEMENTATION_TODO.md (to create)
│   ├── 2025_10_30/                    # Tier 3: Daily work (Oct 30)
│   ├── 2025_10_31/                    # Tier 3: Daily work (Oct 31)
│   └── 2025_11_14/                    # Tier 3: Daily work (Nov 14)
└── examples/                          # Shared examples
```

---

## 🚨 DO NOT MODIFY

**Protected Folders/Files**:
- ❌ `cursor/` - All files (Cursor-specific, already complete)
- ❌ `cursor/CURSOR.md` - Main Cursor rules
- ❌ `cursor/templates/` - Cursor templates
- ❌ `cursor/examples/` - Cursor examples
- ❌ `claude/` - All files (Claude-specific, already complete)
- ❌ `claude/CLAUDE.md` - Main Claude rules
- ❌ `claude/templates/` - Claude templates
- ❌ `claude/examples/` - Claude examples

**Only Modify**:
- ✅ `kimi/` - Kimi-specific files (create new)
- ✅ `docs/plans/` - Planning documents
- ✅ `docs/YYYY_MM_DD/` - Daily work documents
- ✅ `KIMI.md` - This file (if needed)
- ✅ Root files like `README.md` (for Kimi integration)
- ✅ `AGENTS.md` and related agent documentation

---

## 🎯 Current Task Context

**Active Plan**: Planning Phase 0 (Kimi implementation)
**Active Work**: Creating root-level KIMI.md and planning documents

**Completed Phases**:
- None yet (Work in progress as of 2025-11-14)

**Current Phase**: Phase 0 - Planning & Setup (1h)
1. Create root-level KIMI.md (this file)
2. Create daily work document: `20251114_0000_KIMI_GUIDELINES_IMPLEMENTATION.md`
3. Create KIMI_IMPLEMENTATION_PLAN.md
4. Create KIMI_IMPLEMENTATION_TODO.md
5. Update AGENTS.md to include Kimi references
6. Update README.md to include Kimi section
7. Git checkpoint: "Phase 0 complete"

**Progress**: 15% complete (1/7 tasks done)

---

## 🔑 Key Principles for Kimi Implementation

### 1. Kimi-Specific Tool Usage

Kimi CLI uses specific tools that should be emphasized:
- **Task Tool**: For spawning subagents for specific tasks
- **SetTodoList**: For tracking task progress
- **Parallel Tool Calls**: Kimi can make multiple tool calls in one response

### 2. Content Reuse Strategy
- **High reuse** (90%+): FP principles from cursor/CURSOR_FP_PRINCIPLES.md
- **Moderate adaptation** (50-70%): Main rules, workflow guides from cursor/
- **Full rewrite** (10-30%): Tool-specific instructions for Kimi CLI

### 3. Documentation Standards
- Follow existing 3-tier documentation hierarchy
- Use consistent markdown formatting
- Cross-reference related documents
- Maintain parallel structure with cursor/ and claude/ folders

---

## 📚 Reference Documents

**Planning** (to create):
- [KIMI_IMPLEMENTATION_PLAN.md](docs/plans/KIMI_IMPLEMENTATION_PLAN.md) - Complete implementation plan
- [KIMI_IMPLEMENTATION_TODO.md](docs/plans/KIMI_IMPLEMENTATION_TODO.md) - Task breakdown

**AI Agent Guide**:
- [AGENTS_OVERVIEW.md](AGENTS_OVERVIEW.md) - Project overview and structure
- [AGENTS_WORKFLOWS.md](AGENTS_WORKFLOWS.md) - How-to guides
- [AGENTS_REFERENCE.md](AGENTS_REFERENCE.md) - Functional programming patterns

**Other References**:
- [CLAUDE.md](CLAUDE.md) - Claude-specific rules (similar structure desired)
- [GEMINI.md](GEMINI.md) - Gemini rules (concise format reference)
- [.cursorrules](.cursorrules) - Cursor automated rules reference

---

## 🤖 About This File

This `KIMI.md` file ensures Kimi CLI applies consistent rules when working on the global rules repository itself. It follows the same patterns used for Cursor AI and Claude Code, adapted for Kimi's unique capabilities.

**Last Updated**: 2025-11-14
**Status**: Active (Phase 0 in progress)
**Applies To**: This repository only (project-specific rules for Kimi CLI)
**Parallel Files**: `.cursorrules` (Cursor AI), `CLAUDE.md` (Claude Code)

---

## 💡 Quick Tips for Kimi CLI

1. **Always check SetTodoList** before starting work
2. **One task at a time** (mark in_progress, complete, then next)
3. **Git checkpoint every 30-60 min** or after major milestones
4. **Sequential filenames** for daily work (check existing files first)
5. **Read before modifying** - verify content before changes
6. **DO NOT modify cursor/ or claude/ folders** - they're complete and protected
7. **Follow the plan** - refer to KIMI_IMPLEMENTATION_PLAN.md once created
8. **Update TODO lists** immediately after completing tasks
9. **Use parallel tool calls** when possible for efficiency
10. **Spawn subagents** for complex or independent subtasks

---

**Ready to work!** Check the TODO list and continue Phase 0 tasks.
