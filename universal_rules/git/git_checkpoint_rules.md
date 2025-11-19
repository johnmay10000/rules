---
title: Git Checkpoint Rules
category: universal_rules
type: git
applies_to: all
version: 1.0.0
last_updated: 2025-11-19
---

# Git Checkpoint Rules

**Status**: MANDATORY - All AI assistants MUST follow these rules

Git checkpoints are small, frequent commits that preserve work progress, enable easy reversion, and maintain clear project history. These rules apply to ALL projects regardless of technology stack or AI assistant used.

---

## Checkpoint Triggers (MANDATORY)

Commit **MUST** occur after:

### 1. Time-Based Checkpoint
- **Every 30-60 minutes** during active work
- **Maximum time between commits**: 1 hour
- **Rationale**: Prevents significant work loss from crashes, errors, or context switches

### 2. Phase Completion
- After completing any phase (Phase 0, Phase 1, etc.)
- After major milestones in multi-phase projects
- **Rationale**: Marks clear progress boundaries

### 3. Core Document Creation
- Creating CURSOR.md, KIMI.md, CLAUDE.md, GEMINI.md
- Creating SETUP_GUIDE.md, ARCHITECTURE_PLAN.md
- Any major deliverable or design document
- **Rationale**: Preserves architectural decisions and project foundations

### 4. Logical Unit Completion
- Completing a set of related templates
- Finishing a set of examples
- Completing a feature or user story
- **Rationale**: Groups related changes for easier review and reversion

### 5. Context Switch
- Before switching between different work areas
- Before changing projects or repositories
- Before starting a different major task
- **Rationale**: Preserves context and enables clean transitions

### 6. Bug Fix Resolution
- After fixing any bug that resolves an issue
- After implementing error handling improvements
- **Rationale**: Isolates fixes for easy cherry-picking and verification

### 7. Documentation Updates
- Significant documentation changes
- Updating README.md, CONTRIBUTING.md, or API docs
- **Rationale**: Keeps documentation in sync with code changes

### 8. End of Work Session
- Daily checkpoint before stopping work
- End-of-day summary commit
- **Rationale**: Preserves daily progress and provides session summary

---

## Commit Message Format (MANDATORY)

**Required Structure**:

```
<Brief summary (50-72 characters)>

<Detailed description of changes>

<Rationale - why the change was made>

<Impact - what files/features affected>

<Context - relevant decisions or background>

Status: <Current state>

🤖 Generated with [AI Assistant Name]
Co-Authored-By: AI <noreply@anthropic.com>
```

### All Elements Required:

1. **Brief summary line** (50-72 characters)
   - Start with action verb (Complete, Create, Update, Fix, etc.)
   - Include key identifier (phase number, feature name)
   - End with status emoji (✅ for complete, 🔄 for in progress)

2. **Detailed description** (bullet points preferred)
   - What specific changes were made
   - List of files created/modified
   - Technical details of implementation

3. **Rationale** (why the change was needed)
   - Problem being solved
   - User requirement or business need
   - Technical debt being addressed

4. **Impact** (what was affected)
   - Files changed
   - Features impacted
   - Dependencies updated

5. **Context** (relevant decisions)
   - Design decisions made
   - Alternatives considered
   - Future implications

6. **Status indicator** (current state)
   - `Status: Complete ✅`
   - `Status: In Progress 🔄`
   - `Status: Blocked 🚫`
   - `Status: Ready for Review 👀`

7. **Co-author attribution** (required)
   - `🤖 Generated with [Kimi](https://kimi.ai)`
   - `🤖 Generated with [Cursor](https://cursor.sh)`
   - `🤖 Generated with [Claude](https://claude.ai)`
   - `🤖 Generated with [Gemini](https://gemini.google.com)`
   - `Co-Authored-By: Kimi <noreply@anthropic.com>`
   - `Co-Authored-By: Claude <noreply@anthropic.com>`

---

## Good Examples

### ✅ Phase Completion
```
Complete Phase 0 - Portability foundation ✅

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

🤖 Generated with [Kimi](https://kimi.ai)
Co-Authored-By: Kimi <noreply@anthropic.com>
```

### ✅ Document Creation
```
Create CURSOR.md core global rule set ✅

Implemented mandatory universal rules:
- Git checkpoint requirements
- Documentation structure (3-tier hierarchy)
- File size limits (250-300 lines)
- Testing requirements (comprehensive, 100% passing)
- FP principles (ADTs, Result types, immutability)

Rationale: Based on analysis of code-style.mdc and CLAUDE.md.
Generalized for all tech stacks.

Status: Core rules document complete ✅

🤖 Generated with [Cursor](https://cursor.sh)
Co-Authored-By: Claude <noreply@anthropic.com>
```

### ✅ Template Set
```
Create all portable project templates ✅

Created 3 smart .cursorrules templates:
- _envvar: Uses ${CURSOR_RULES_PATH} for portability
- _submodule: Uses relative paths for self-contained
- Basic template for simple projects

All include auto-detection logic for Python, TypeScript, GCP, AWS.
Tested on macOS and Linux.

Status: Templates complete ✅

🤖 Generated with [Kimi](https://kimi.ai)
Co-Authored-By: Kimi <noreply@anthropic.com>
```

---

## Bad Examples (DO NOT DO)

### ❌ Too Vague
```
Update files
```
**Problem**: No context, no rationale, no impact description.

### ❌ No Context
```
WIP
```
**Problem**: Meaningless, provides no information for future review.

### ❌ Multiple Unrelated Changes
```
Create CURSOR.md, fix typos, update README, add examples, refactor templates
```
**Problem**: Should be 5 separate commits. Violates single responsibility principle.

### ❌ No Status Indicator
```
Complete Phase 1

Created all deliverables.
```
**Problem**: Missing status indicator makes it unclear if truly complete.

### ❌ Missing Co-Author
```
Complete Phase 2 ✅

Created all deliverables.

Status: Complete ✅
```
**Problem**: Missing AI assistant attribution and co-author line.

---

## Workflow Examples

### Daily Development Flow

**Morning (Start of Session)**:
```bash
# 1. Check current status
git status
git log --oneline -n 5

# 2. Review TODO list (if exists)
cat docs/plans/CURRENT_FEATURE_TODO.md

# 3. Create daily work folder
mkdir -p docs/$(date +%Y_%m_%d)

# 4. Start work
```

**During Work (Every 30-60 minutes)**:
```bash
# 1. Check what changed
git status
git diff

# 2. Read modified files to verify content
# AI assistants can read and verify multiple files

# 3. Stage changes
git add <files>

# 4. Commit with descriptive message (follow format above)
git commit -m "Brief summary (50-72 chars)

Detailed description of changes made

Rationale for the changes

What deliverables/files were affected

Any relevant context or decisions

Status: <Current state>

🤖 Generated with [AI Assistant Name]
Co-Authored-By: AI <noreply@anthropic.com>"

# 5. Update TODO list
# Mark tasks [x], add actual time, update progress
```

**End of Day (Session Checkpoint)**:
```bash
# 1. Create session summary
cat > docs/$(date +%Y_%m_%d)/$(date +%Y%m%d)_NNNN_SESSION_SUMMARY.md << 'EOF'
# Session Summary - YYYY-MM-DD

## Accomplished
- Task 1 completed
- Task 2 in progress

## Decisions Made
- Decision 1
- Decision 2

## Next Session
- Continue with task 2
- Start task 3

## Blockers
- None

## AI Assistant Notes
- Workflow patterns used
EOF

# 2. Git checkpoint for session end
git add docs/
git commit -m "Session checkpoint - work summary

Created session summary document with:
- Completed tasks: X
- Decisions: Y key decisions
- Next steps: Z tasks for next session
- Blockers: None identified

Files: docs/YYYY_MM_DD/YYYYMMDD_NNNN_SESSION_SUMMARY.md

Status: Session complete, ready for next phase

🤖 Generated with [AI Assistant Name]
Co-Authored-By: AI <noreply@anthropic.com>"
```

---

## Benefits of Git Checkpoints

### For Individual Developers
- **Work Preservation**: Never lose more than 1 hour of work
- **Clear History**: Easy to track progress and decisions
- **Safe Reversion**: Can revert specific changes without losing everything
- **Context Preservation**: Commit messages explain why changes were made
- **Stress Reduction**: Frequent commits reduce risk of major loss

### For Teams
- **Transparent Progress**: Team can see daily/hourly progress
- **Easy Code Review**: Small, focused commits are easier to review
- **Knowledge Sharing**: Commit messages document decisions and rationale
- **Conflict Reduction**: Frequent commits reduce merge conflicts
- **Onboarding**: New team members can read commit history to understand project evolution

### For AI Assistants
- **Context Awareness**: Can read recent commits to understand current state
- **Progress Tracking**: Can track which tasks are complete vs. in progress
- **Decision Preservation**: Can reference past decisions in commit messages
- **Workflow Integration**: Can generate appropriate commit messages following the format

---

## Anti-Patterns (DO NOT DO)

### ❌ Waiting Too Long
- Working 2+ hours without committing
- Completing multiple phases without checkpoint
- Creating 10+ files without commit

**Consequence**: Risk of significant work loss, unclear history, difficult reversion.

### ❌ Vague Commit Messages
- "Update files"
- "WIP"
- "More changes"
- "Fix stuff"

**Consequence**: No context for future review, can't understand what changed or why.

### ❌ Mixing Unrelated Changes
- Committing bug fix + new feature + documentation update together
- Combining work from different phases
- Mixing refactoring with new functionality

**Consequence**: Difficult to revert specific changes, hard to review, unclear history.

### ❌ Skipping Status Updates
- Not updating TODO lists after commits
- Not creating session summaries
- Not documenting decisions

**Consequence**: Loss of context, can't track progress, repeated questions about status.

---

## Emergency Procedures

### If You Forget to Commit for 2+ Hours
1. **Stop work immediately**
2. **Create checkpoint commit** with what you have
3. **Document the gap** in commit message
4. **Resume normal checkpoint schedule**
5. **Set a timer** for 45 minutes to remind next checkpoint

### If You Make a Bad Commit
1. **Don't panic** - this is why we checkpoint frequently
2. **Revert if needed**: `git revert <commit-hash>`
3. **Create new commit** with correct changes
4. **Document the revert** in commit message

### If You Commit Too Much at Once
1. **Create a new branch**: `git checkout -b split-commits`
2. **Reset to before the big commit**: `git reset HEAD~1`
3. **Stage and commit in smaller logical units**
4. **Merge or rebase** back to main branch

---

## Tool-Specific Integration

### For Kimi CLI
- Use `kimi git-commit` with appropriate templates
- Leverage SetTodoList tool to track task completion
- Use parallel tool calls to verify multiple files before commit
- Spawn subagents for complex validation before checkpointing

### For Cursor
- Use inline suggestions for commit message generation
- Leverage VS Code Git integration
- Use Cursor's understanding of project context for better messages

### For Claude
- Use Claude's analysis capabilities to generate comprehensive commit messages
- Leverage Claude's planning abilities for phase-based commits

### For Gemini
- Use Gemini's code understanding to identify what changed
- Leverage Gemini's documentation skills for detailed descriptions

---

## Metrics and Monitoring

### Personal Metrics to Track
- **Checkpoint Frequency**: Average time between commits (target: < 60 minutes)
- **Commit Quality**: Percentage of commits following full format (target: 100%)
- **Documentation Coverage**: Percentage of commits with proper documentation (target: 100%)

### Team Metrics to Track
- **Average Commit Size**: Lines changed per commit (target: < 300 lines)
- **Commit Frequency**: Commits per developer per day (target: 8-15)
- **Revert Rate**: Percentage of commits reverted (target: < 5%)

---

## Further Reading

- **Git Best Practices**: `universal_rules/git/commit_conventions.md`
- **Branching Strategy**: `universal_rules/git/branching_strategy.md`
- **Documentation Standards**: `universal_rules/documentation/daily_workflow.md`

---

**Last Updated**: 2025-11-19  
**Maintained By**: Global AI Rules System  
**Status**: Active  
**Applies To**: All AI assistants (Cursor, Kimi, Claude, Gemini) and all human developers