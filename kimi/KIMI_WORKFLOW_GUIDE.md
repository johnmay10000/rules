# KIMI_WORKFLOW_GUIDE.md - Git and Documentation Workflow

**Version**: 1.0.0  
**Last Updated**: 2025-11-14  
**Companion To**: [KIMI.md](KIMI.md)  
**Purpose**: Detailed workflow guidance and templates for Kimi CLI

---

## Overview

This document provides practical guidance for implementing the mandatory workflow rules from [KIMI.md](KIMI.md).

**Covers**:
- Git checkpoint workflow
- Commit message templates
- Documentation hierarchy examples
- Plan and TODO list templates
- Real-world workflow scenarios
- Kimi-specific parallel execution patterns

---

## Table of Contents

1. [Git Checkpoint Workflow](#1-git-checkpoint-workflow)
2. [Commit Message Templates](#2-commit-message-templates)
3. [Documentation Hierarchy In Practice](#3-documentation-hierarchy-in-practice)
4. [Plan Document Templates](#4-plan-document-templates)
5. [TODO List Management](#5-todo-list-management)
6. [Real-World Scenarios](#6-real-world-scenarios)
7. [Kimi-Specific Patterns](#7-kimi-specific-patterns)

---

## 1. Git Checkpoint Workflow

### Daily Development Flow

**Morning** (Start of Session):
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

**During Work** (Every 30-60 minutes):
```bash
# 1. Check what changed
git status
git diff

# 2. Read modified files to verify content
# Kimi can read and verify multiple files in parallel:
kimi read-file file1.md &
kimi read-file file2.md &
kimi read-file file3.md &
wait

# 3. Stage changes
git add <files>

# 4. Commit with descriptive message (follow KIMI.md format)
git commit -m "Brief summary (50-72 chars)

Detailed description of changes made

Rationale for the changes

Files affected: file1.md, file2.md

Status: Task 3 complete ✅

🤖 Generated with [Kimi](https://kimi.ai)

Co-Authored-By: Kimi <noreply@kimi.ai>"

# 5. Update TODO list (using SetTodoList tool)
# Mark tasks [x], add actual time, update progress
```

**End of Day** (Session Checkpoint):
```bash
# 1. Create session summary (use next sequence number)
# Find next number: ls docs/$(date +%Y_%m_%d)/ | tail -1
# If last file is 20251114_0001_*, next is 0002

cat > docs/$(date +%Y_%m_%d)/$(date +%Y%m%d)_0002_SESSION_SUMMARY.md << 'EOF'
# Session Summary - $(date +%Y-%m-%d)

## Accomplished
- Task 1 completed
- Task 2 in progress
- Created 3 implementation guides

## Decisions Made
- Decision 1
- Decision 2

## Next Session
- Continue with task 2
- Start task 3
- Begin templates phase

## Blockers
- None

## Kimi Notes
- Used parallel tool calls for verification
- Spawned subagent for complex validation
EOF

# 2. Git checkpoint for session end
git add docs/
git commit -m "Session checkpoint - work summary

Created session summary document with:
- Completed tasks: 2
- Decisions: 2 key decisions documented
- Next steps: 3 tasks for next session
- Blockers: None identified

Files: docs/2025_11_14/20251114_0002_SESSION_SUMMARY.md

Status: Session complete, ready for Phase 2

🤖 Generated with [Kimi](https://kimi.ai)

Co-Authored-By: Kimi <noreply@kimi.ai>"
```

---

## 2. Commit Message Templates

### Phase Completion Template
```
Complete Phase X: Description of what was done

Detailed description of what was accomplished in this phase:

- Item 1
- Item 2  
- Item 3

Rationale: Why these specific items, what decisions were made

Files affected:
- doc1.md
- doc2.md
- feature/file1.ext

Status: Phase X complete ✅

🤖 Generated with [Kimi](https://kimi.ai)

Co-Authored-By: Kimi <noreply@kimi.ai>
```

### Bug Fix Template
```
Fix: Brief description of the bug

**Problem**: What was broken or not working

**Root Cause**: Why it was happening

**Solution**: How it was fixed

**Verification**: How you confirmed the fix works

Files affected:
- src/buggy/file.ext
- tests/test_file.ext

Status: Bug fixed, tests passing ✅

🤖 Generated with [Kimi](https://kimi.ai)

Co-Authored-By: Kimi <noreply@kimi.ai>
```

### Documentation Template
```
Documentation: What was documented

**Purpose**: Why this documentation was needed

**Contents**: What topics are covered

**Structure**: How it's organized

Files affected:
- docs/2025_11_14/20251114_XXXX_file.md

Status: Documentation complete ✅

🤖 Generated with [Kimi](https://kimi.ai)

Co-Authored-By: Kimi <noreply@kimi.ai>
```

---

## 3. Documentation Hierarchy In Practice

### Three-Tier Structure Template

**Tier 1: Strategic** (Project Root)
```
ARCHITECTURE_PLAN.md

# Architecture Overview

## System Components
- Component 1
- Component 2
- Component 3

## Key Decisions
- Decision 1 with rationale
- Decision 2 with rationale

## Technology Stack
- Language/Framework choices
- Key libraries and why
```

**Tier 2: Tactical** (docs/plans/)
```
FEATURE_NAME_PLAN.md
FEATURE_NAME_TODO.md

KIMI_IMPLEMENTATION_PLAN.md
KIMI_IMPLEMENTATION_TODO.md

USER_PROFILE_PLAN.md
USER_PROFILE_TODO.md

PAYMENT_PROCESSING_PLAN.md
PAYMENT_PROCESSING_TODO.md
```

**Tier 3: Execution** (docs/YYYY_MM_DD/)
```
docs/
├── 2025_11_14/
│   ├── 20251114_0000_CURSOR_MD_IMPLEMENTATION.md
│   ├── 20251114_0001_PHASE_0_COMPLETE.md
│   ├── 20251114_0002_USER_PROFILE_VALIDATION.md
│   └── 20251114_0003_SESSION_SUMMARY.md
│
├── 2025_11_15/
│   ├── 20251115_0000_PAYMENT_INTEGRATION.md
│   ├── 20251115_0001_GCP_SETUP.md
│   └── 20251115_0002_SESSION_SUMMARY.md
```

---

## 4. Plan Document Templates

### Feature/Phase Plan Template
```markdown
# Feature/Phase Name Implementation Plan

**Status**: In Progress/Complete  
**Created**: YYYY-MM-DD  
**Paired With**: [FEATURE_NAME_TODO.md](FEATURE_NAME_TODO.md)

---

## Overview

**Goal**: What this feature/phase accomplishes  
**Scope**: What's included and what's not  
**Success Criteria**: How we know it's done  
**Estimated Time**: X hours

---

## Architecture

### Components
- Component 1: Description
- Component 2: Description
- Component 3: Description

### Data Flow
[Diagram or description of data flow]

### Key Decisions
- Decision 1: Rationale
- Decision 2: Rationale

---

## Implementation Phases

### Phase 0: Planning (1 hour)
- [] Task 1
- [] Task 2
- [] Task 3

### Phase 1: Core Setup (2 hours)
- [] Task 1
- [] Task 2
- [] Task 3

### Phase 2: Implementation (3 hours)
- [] Task 1
- [] Task 2
- [] Task 3

---

## Risk Assessment

**High Risk**:
- Risk 1: Mitigation strategy

**Medium Risk**:
- Risk 2: Mitigation strategy

**Low Risk**:
- Risk 3: Mitigation strategy

---

## References

- Related document 1
- Related document 2
```

---

## 5. TODO List Management

### TODO File Structure
```markdown
# Feature Name TODO List

**Paired With**: [FEATURE_NAME_PLAN.md](FEATURE_NAME_PLAN.md)
**Created**: YYYY-MM-DD  
**Last Updated**: YYYY-MM-DD

---

## Progress Overview

- **Total Tasks**: 15
- **Completed**: 3 ✅
- **In Progress**: 1 🔄
- **Pending**: 11 ⏳
- **Progress**: 20%

**Estimated Time**: 5 hours  
**Time Spent**: 1.5 hours  
**Time Remaining**: 3.5 hours

---

## Phase 1: Core Setup (1 hour)

**Status**: 🔄 IN PROGRESS  
**Progress**: 2/3 tasks (67%)

- [x] Task 1 - Create project structure (0.25h actual) ✅
- [x] Task 2 - Set up dependencies (0.5h actual) ✅
- [] Task 3 - Initialize documentation (in progress)

---

## Update History

**2025-11-14 13:30**: Started task 3, 80% complete  
**2025-11-14 13:15**: Completed task 2 (0.5h actual vs 0.25h est)  
**2025-11-14 12:45**: Completed task 1 (0.25h actual vs 0.5h est)
```

### Using SetTodoList Tool

When updating TODO lists, use the SetTodoList tool:

```json
{
  "todos": [
    {
      "title": "Task 1 - Create project structure",
      "status": "Done"
    },
    {
      "title": "Task 2 - Set up dependencies", 
      "status": "Done"
    },
    {
      "title": "Task 3 - Initialize documentation",
      "status": "In Progress"
    }
  ]
}
```

---

## 6. Real-World Scenarios

### Scenario 1: Multi-File Feature
```bash
# Working on a feature that requires changes to multiple files

# Step 1: Update TODO list (mark task in_progress)
kimi set-todo "Feature X - Multi-file changes" "In Progress"

# Step 2: Make changes to multiple related files
# File 1: docs/plans/FEATURE_PLAN.md
# File 2: docs/plans/FEATURE_TODO.md  
# File 3: src/feature/module.py
# File 4: tests/test_feature.py

# Step 3: Verify all files (Kimi can do this in parallel)
kimi read-file docs/plans/FEATURE_PLAN.md &
kimi read-file docs/plans/FEATURE_TODO.md &
kimi read-file src/feature/module.py &
kimi read-file tests/test_feature.py &
wait

# Step 4: Test the changes
python -m pytest tests/test_feature.py -v
# Or other test commands based on project

# Step 5: Git checkpoint (one commit for logical unit)
git add -A
git commit -m "Complete Feature X - Multi-file implementation

Implemented Feature X across multiple components:

- Updated plan to reflect scope changes
- Completed all 5 implementation tasks
- Added comprehensive tests
- Updated documentation

Files affected:
- docs/plans/FEATURE_PLAN.md
- docs/plans/FEATURE_TODO.md
- src/feature/module.py
- tests/test_feature.py
- README.md (updated usage)

All tests passing: 12 new tests added, 100% coverage

Status: Feature X complete ✅

🤖 Generated with [Kimi](https://kimi.ai)

Co-Authored-By: Kimi <noreply@kimi.ai>"

# Step 6: Update TODO list (mark completed, add time)
kimi set-todo "Feature X - Multi-file changes" "Done" "2.5h"
```

### Scenario 2: Bug Fix
```bash
# Debugging and fixing a production bug

# Step 1: Create bug investigation document
# docs/$(date +%Y_%m_%d)/$(date +%Y%m%d)_XXXX_BUG_INVESTIGATION.md

# Step 2: Identify root cause
# ... debugging process ...

# Step 3: Implement fix
# ... fix implementation ...

# Step 4: Verify fix with tests
# ... test execution ...

# Step 5: Git checkpoint
git commit -m "Fix: Critical payment processing bug

**Problem**: Payment processing failing for amounts > $999.99
Error: 'amount exceeds maximum value'
Root Cause: Integer overflow in amount validation

**Solution**: Changed amount validation from Int to Integer (arbitrary precision)

**Verification**:
- Added regression test for large amounts
- Tested with $9999.99 (previously failed, now passes)
- All existing tests still passing

Files affected:
- src/payment/validation.ts
- tests/payment/validation.test.ts

Status: Bug fixed, all tests passing ✅

🤖 Generated with [Kimi](https://kimi.ai)

Co-Authored-By: Kimi <noreply@kimi.ai>"
```

### Scenario 3: Documentation Sprint
```bash
# Creating multiple documentation files

# Step 1: Create implementation plan
# This is a meta-task about documentation creation

# Step 2: Create multiple related docs in sequence
# Files to create:
# 1. docs/plans/DOCS_SPRINT_PLAN.md
# 2. docs/plans/DOCS_SPRINT_TODO.md
# 3. docs/2025_11_14/20251114_XXXX_USER_GUIDE.md
# 4. docs/2025_11_14/20251114_XXXX_API_REFERENCE.md
# 5. docs/2025_11_14/20251114_XXXX_SETUP_GUIDE.md

# Step 3: Work through each file
# Create file 1 → verify → file 2 → verify → file 3 → verify

# Step 4: After ~45 minutes, checkpoint
# Even though related, don't wait for ALL files
# Checkpoint after logical groups

git commit -m "Documentation: User guide and API reference

Created comprehensive documentation for Feature X:

- User guide with examples and screenshots
- API reference with all endpoints documented
- Setup guide for new developers
- Cross-referenced with architecture plan

Files created:
- docs/2025_11_14/20251114_0003_USER_GUIDE.md
- docs/2025_11_14/20251114_0004_API_REFERENCE.md

2 more docs coming in next commit

Status: Documentation sprint 50% complete

🤖 Generated with [Kimi](https://kimi.ai)

Co-Authored-By: Kimi <noreply@kimi.ai>"

# Step 5: Continue with remaining docs
# Then final checkpoint
```

---

## 7. Kimi-Specific Patterns

### Parallel File Verification

Kimi's strength is parallel tool execution. Use this for efficiency:

```bash
# Instead of sequential:
kimi read-file file1.md
kimi read-file file2.md
kimi read-file file3.md

# Use parallel execution:
kimi read-file file1.md &
kimi read-file file2.md &
kimi read-file file3.md &
wait
```

### Subagent Delegation

For complex validations, spawn subagents:

```bash
# Complex validation that benefits from subagent
echo "Validating complex FP pipeline..."

# Spawn subagent for type checking
kimi task "Validate type signatures" "coder" \
  "Check all function signatures in src/module.ts for:
   - Proper Either types
   - Correct generic parameters
   - Consistent error handling" &

# Spawn subagent for purity check
kimi task "Verify function purity" "coder" \
  "Analyze functions in src/pure/ to verify:
   - No side effects
   - No mutations of inputs
   - Deterministic outputs" &

wait
echo "All validations complete!"
```

### SetTodoList Integration

Always use SetTodoList for task tracking (not manual file editing):

```json
{
  "todos": [
    {
      "title": "Create KIMI_WORKFLOW_GUIDE.md",
      "status": "In Progress"
    },
    {
      "title": "Create FILE_LOCATIONS_USER_GUIDE.md",
      "status": "Pending"
    },
    {
      "title": "Create DATA_STRUCTURE_PATTERNS.md",
      "status": "Pending"
    },
    {
      "title": "Create NAMING_CONVENTION.md",
      "status": "Pending"
    }
  ]
}
```

### Batch File Operations

Kimi can process multiple files in one command:

```bash
# Create multiple related files in one operation
cat > file1.md << 'EOF'
Content for file 1
EOF

cat > file2.md << 'EOF'  
Content for file 2
EOF

cat > file3.md << 'EOF'
Content for file 3
EOF

# Then verify all at once
git add file1.md file2.md file3.md
git commit -m "Create batch of related files

Created 3 related documentation files:

1. File 1: Purpose and description
2. File 2: Purpose and description
3. File 3: Purpose and description

All files follow consistent format and structure.

Files created:
- file1.md
- file2.md
- file3.md

Status: Batch creation complete ✅

🤖 Generated with [Kimi](https://kimi.ai)

Co-Authored-By: Kimi <noreply@kimi.ai>"
```

---

## Summary

This workflow guide provides templates and patterns for using Kimi CLI effectively. Key principles:

1. **Git checkpoints every 30-60 minutes** (mandatory)
2. **Use sequential numbering** for daily work documents
3. **Update TODO lists after each task** completion
4. **Parallel verification** for efficiency
5. **Subagent delegation** for complex validations
6. **Clear commit messages** with all required sections

**Kimi-Specific Best Practices**:
- Leverage parallel tool calls for file operations
- Use subagents for independent validation tasks
- Batch related file changes in single commits
- Always use SetTodoList tool for tracking

**Next Steps**:
1. Review templates before starting work
2. Set up environment variables
3. Create first daily work document
4. Start with small, focused tasks

---

**Last Updated**: 2025-11-14  
**Maintained By**: Kimi CLI Global Rules System  
**Status**: Active
