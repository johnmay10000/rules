# CURSOR_WORKFLOW_GUIDE.md - Git and Documentation Workflow

**Version**: 1.0.0  
**Last Updated**: 2025-10-31  
**Companion To**: [CURSOR.md](CURSOR.md)  
**Purpose**: Detailed workflow guidance and templates

---

## Overview

This document provides practical guidance for implementing the mandatory workflow rules from [CURSOR.md](CURSOR.md).

**Covers**:
- Git checkpoint workflow
- Commit message templates
- Documentation hierarchy examples
- Plan and TODO list templates
- Real-world workflow scenarios

---

## Table of Contents

1. [Git Checkpoint Workflow](#1-git-checkpoint-workflow)
2. [Commit Message Templates](#2-commit-message-templates)
3. [Documentation Hierarchy In Practice](#3-documentation-hierarchy-in-practice)
4. [Plan Document Templates](#4-plan-document-templates)
5. [TODO List Management](#5-todo-list-management)
6. [Real-World Scenarios](#6-real-world-scenarios)

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

# 2. Stage changes
git add <files>

# 3. Commit with descriptive message
git commit -m "Brief summary

Detailed description of changes made

Rationale for the changes

Files affected: <list>

Status: <state>

🤖 Generated with Cursor
Co-Authored-By: Claude <noreply@anthropic.com>"

# 4. Update TODO list (if applicable)
# Mark tasks [x], add actual time, update progress
```

**End of Day** (Session Checkpoint):
```bash
# 1. Create session summary (use next sequence number)
# Find next number: ls docs/$(date +%Y_%m_%d)/ | tail -1
# If last file is 20251031_0001_*, next is 0002

cat > docs/$(date +%Y_%m_%d)/$(date +%Y%m%d)_0002_SESSION_SUMMARY.md << 'EOF'
# Session Summary - $(date +%Y-%m-%d)

## Accomplished
- Task 1 completed
- Task 2 in progress

## Decisions Made
- Decision 1
- Decision 2

## Next Session
- Continue with task 2
- Start task 3
EOF

# 2. Commit session summary
git add docs/
git commit -m "End of session - $(date +%Y-%m-%d)

Session summary created with progress update.

Completed: [list tasks]
In Progress: [list tasks]

Status: End of day checkpoint

🤖 Generated with Cursor
Co-Authored-By: Claude <noreply@anthropic.com>"

# 3. Push to remote (if applicable)
git push
```

---

### Checkpoint Decision Tree

```
Are you working? → Check timer
    ↓
Has 30-60 min passed? → YES → COMMIT NOW
    ↓ NO
Did you complete a phase? → YES → COMMIT NOW
    ↓ NO
Did you fix a bug? → YES → COMMIT NOW
    ↓ NO
Did you create major doc? → YES → COMMIT NOW
    ↓ NO
About to switch tasks? → YES → COMMIT NOW
    ↓ NO
Continue working → Check timer again
```

---

## 2. Commit Message Templates

### Template 1: Feature Implementation

```
Implement [feature name] - [component]

Implementation details:
- Added [specific functionality]
- Created [files/modules]
- Integrated with [existing system]

Rationale:
- [Why this was needed]
- [What problem it solves]

Impact:
- Files added: [list]
- Files modified: [list]
- Dependencies added: [list]

Testing:
- [Tests added/modified]
- All tests passing ✓

Status: Feature [X] complete

🤖 Generated with Cursor
Co-Authored-By: Claude <noreply@anthropic.com>
```

**Example**:
```
Implement user authentication - auth module

Implementation details:
- Added JWT token generation and validation
- Created auth middleware for route protection
- Integrated with existing user service

Rationale:
- Application requires secure user sessions
- JWT provides stateless authentication
- Middleware ensures consistent auth checking

Impact:
- Files added: src/auth/jwt.ts, src/auth/middleware.ts
- Files modified: src/api/users.ts, src/types/auth.ts
- Dependencies added: jsonwebtoken, bcrypt

Testing:
- Added 12 tests for JWT operations
- Added 8 tests for middleware
- All 20 tests passing ✓

Status: Authentication feature complete ✓

🤖 Generated with Cursor
Co-Authored-By: Claude <noreply@anthropic.com>
```

---

### Template 2: Bug Fix

```
Fix [bug description] - [component]

Problem:
- [What was broken]
- [How it manifested]
- [Impact on users/system]

Root Cause:
- [What caused the bug]

Solution:
- [How it was fixed]
- [Why this approach]

Files Changed:
- [list with brief description]

Testing:
- [Regression test added]
- [Existing tests still pass]

Status: Bug #[number] fixed ✓

🤖 Generated with Cursor
Co-Authored-By: Claude <noreply@anthropic.com>
```

---

### Template 3: Documentation Update

```
Update documentation - [topic]

Changes:
- [What was added/modified]
- [What was clarified]

Reason:
- [Why update was needed]
- [What was unclear before]

Files Updated:
- [list]

Cross-References:
- [Links to related docs updated]

Status: Documentation current ✓

🤖 Generated with Cursor
Co-Authored-By: Claude <noreply@anthropic.com>
```

---

### Template 4: Phase Completion

```
Complete Phase [N]: [Phase name] ✅

Delivered:
- [Deliverable 1]
- [Deliverable 2]
- [Deliverable 3]

Accomplishments:
- [Major achievement 1]
- [Major achievement 2]

Time:
- Estimated: [X] hours
- Actual: [Y] hours

Quality Metrics:
- Tests: [X] passing
- Coverage: [Y]%
- Files: [Z] created/modified

Next Phase:
- [Brief description]
- Estimated: [X] hours

Status: Phase [N] complete ✅

🤖 Generated with Cursor
Co-Authored-By: Claude <noreply@anthropic.com>
```

---

## 3. Documentation Hierarchy In Practice

### Scenario: Adding New Feature

**Step 1: Update Tier 1** (if major feature)

```markdown
# ARCHITECTURE_PLAN.md

## Major Components
- ... (existing)
- **NEW**: Payment Processing - Handles subscriptions and billing

## Sub-Plans
- [Existing plans...]
- [Payment Processing Plan](docs/plans/PAYMENT_PROCESSING_PLAN.md)
```

**Step 2: Create Tier 2** (plan + TODO)

```bash
# Create plan
cat > docs/plans/PAYMENT_PROCESSING_PLAN.md
# (Use template below)

# Create paired TODO
cat > docs/plans/PAYMENT_PROCESSING_TODO.md
# (Use template below)
```

**Step 3: Daily Work in Tier 3**

```bash
# Each day, create timestamped docs
docs/2025_10_31/20251031_0900_PAYMENT_STRIPE_INTEGRATION.md
docs/2025_10_31/20251031_1400_PAYMENT_WEBHOOK_HANDLER.md
docs/2025_10_31/20251031_1700_SESSION_SUMMARY.md
```

**Step 4: Update TODO as Work Progresses**

Cursor automatically updates:
```markdown
# docs/plans/PAYMENT_PROCESSING_TODO.md

- [x] Task 1: Stripe integration (Est: 2h, Actual: 2.5h)
- [x] Task 2: Webhook handler (Est: 1h, Actual: 0.8h)
- [ ] Task 3: Testing (Est: 2h, Actual: -)

Last Updated: 2025-10-31 17:00
Progress: 2/3 tasks (67%)
```

---

## 4. Plan Document Templates

### Feature Plan Template

```markdown
# [Feature Name] Implementation Plan

**Status**: ⏳ IN PROGRESS  
**Last Updated**: YYYY-MM-DD HH:MM  
**Estimated Time**: [X] hours  
**Priority**: [P1/P2/P3]

---

## Overview

### What
[Brief description of what will be built]

### Why
[Business justification or problem being solved]

### Success Criteria
- [ ] Criterion 1
- [ ] Criterion 2
- [ ] Criterion 3

---

## Phases

### Phase 1: [Name] ([X] hours)
- [ ] Task 1.1
- [ ] Task 1.2
- [ ] Task 1.3

### Phase 2: [Name] ([Y] hours)
- [ ] Task 2.1
- [ ] Task 2.2

### Phase 3: [Name] ([Z] hours)
- [ ] Task 3.1
- [ ] Task 3.2

---

## Implementation Details

### Code Locations
- **New Files**: `src/features/[feature]/`
- **Modified Files**: `src/api/routes.ts`, `src/types/domain.ts`

### ADT Structures
\`\`\`python
@dataclass(frozen=True)
class [FeatureType]:
    field1: str
    field2: int

[FeatureResult] = Success[[FeatureType]] | Failure[[ErrorType]]
\`\`\`

### Key Functions
- `process_[feature]()` - Main entry point
- `validate_[feature]()` - Validation logic
- `save_[feature]()` - Persistence

### Dependencies
- [Library 1] - [Purpose]
- [Library 2] - [Purpose]

---

## Testing Strategy
- Unit tests: [Number] tests for core logic
- Integration tests: [Number] tests for API
- Coverage target: 80%+

---

## Risks and Mitigations
| Risk | Impact | Mitigation |
|------|--------|------------|
| [Risk 1] | [High/Med/Low] | [Mitigation strategy] |

---

## Update History

### YYYY-MM-DD HH:MM
- Completed Phase 1
- Started Phase 2
- [Decisions/issues]

### YYYY-MM-DD HH:MM
- [Initial plan created]
```

---

## 5. TODO List Management

### Cursor's Responsibilities

**Cursor MUST automatically**:
1. Mark completed tasks with [x]
2. Add actual time spent
3. Update progress percentages
4. Add timestamped update entries

### TODO Template

```markdown
# [Feature Name] TODO List

**Plan Reference**: [[FEATURE_NAME_PLAN.md]](FEATURE_NAME_PLAN.md)  
**Status**: ⏳ IN PROGRESS | ✅ COMPLETE  
**Last Updated**: YYYY-MM-DD HH:MM  
**Progress**: X/Y tasks (Z%)

---

## Phase 1: [Phase Name]

**Status**: ⏳ IN PROGRESS  
**Progress**: X/Y tasks

- [x] Task 1.1: [Description] (Est: 2h, Actual: 1.5h)
  - Implementation note: [Brief note]
  - Files: `src/feature/module1.ts`
  
- [x] Task 1.2: [Description] (Est: 1h, Actual: 1.2h)
  - Implementation note: [Brief note]
  - Files: `src/feature/module2.ts`
  
- [ ] Task 1.3: [Description] (Est: 3h, Actual: -)
  - Blocked by: [Dependency]
  
- [ ] Task 1.4: [Description] (Est: 1h, Actual: -)

**Phase Totals**:  
Est: 7h | Actual: 2.7h | Remaining: ~4.3h

---

## Phase 2: [Phase Name]

**Status**: 🔄 NOT STARTED  
**Progress**: 0/3 tasks

- [ ] Task 2.1: [Description] (Est: 2h, Actual: -)
- [ ] Task 2.2: [Description] (Est: 1h, Actual: -)
- [ ] Task 2.3: [Description] (Est: 2h, Actual: -)

**Phase Totals**:  
Est: 5h | Remaining: 5h

---

## Overall Progress

**Tasks**: X completed / Y total (Z%)  
**Time**: Est [Total]h | Actual [X]h | Remaining ~[Y]h  
**Completion Rate**: [Z]%

**Velocity**: [X] hours/day average

---

## Update History

### YYYY-MM-DD HH:MM
- ✅ Completed: Tasks 1.1, 1.2
- ⏳ Started: Task 1.3
- 📊 Progress: Phase 1 = 50% complete
- ⏱️ Time: 2.7h spent, 4.3h remaining in Phase 1
- 🔍 Notes: Task 1.2 took longer due to [reason]

### YYYY-MM-DD HH:MM
- 📝 Plan created
- 🎯 7 tasks identified across 2 phases
- ⏱️ Total estimate: 12 hours
```

---

## 6. Real-World Scenarios

### Scenario 1: Starting New Feature

**Day 1 - Planning**:
```bash
# 1. Create plan documents
cat > docs/plans/USER_PROFILE_PLAN.md
# [Fill in template]

cat > docs/plans/USER_PROFILE_TODO.md
# [Fill in template]

# 2. Update architecture
# Add link to ARCHITECTURE_PLAN.md

# 3. Commit plan
git add docs/plans/
git commit -m "Create plan for user profile feature"
```

**Day 1 - Implementation**:
```bash
# 4. Create daily folder
mkdir docs/2025_10_31

# 5. Start coding, document decisions
cat > docs/2025_10_31/20251031_0900_PROFILE_SCHEMA_DESIGN.md
# [Document ADT design decisions]

# 6. Implement first task
# Code...

# 7. Checkpoint after 45 min
git add src/ docs/
git commit -m "Implement profile schema - ADTs and types"

# 8. Update TODO
# Mark task 1.1 complete, add actual time
```

---

### Scenario 2: Bug Fix

```bash
# 1. Document investigation
cat > docs/2025_10_31/20251031_1400_BUG_123_INVESTIGATION.md
# [Document symptoms, root cause analysis]

# 2. Fix bug
# Code changes...

# 3. Add regression test
# Test code...

# 4. Commit fix
git add src/ tests/ docs/
git commit -m "Fix validation error in email checker - Bug #123"

# 5. No TODO update needed (not part of a plan)
```

---

### Scenario 3: End of Phase

```bash
# 1. Complete last task
# Code...

# 2. Update TODO list
# Mark all Phase 1 tasks [x]
# Add actual times
# Mark Phase 1 status: ✅ COMPLETE

# 3. Create phase summary
cat > docs/2025_10_31/20251031_1700_PHASE_1_COMPLETE.md
# [Summarize accomplishments, metrics, decisions]

# 4. Commit phase completion
git add docs/
git commit -m "Complete Phase 1: User Profile Schema ✅"

# 5. Update plan document
# Mark Phase 1 [x]
# Add to update history
```

---

## Quick Reference

### Before Every Commit Checklist

- [ ] Changes address a specific checkpoint trigger
- [ ] Commit message follows template
- [ ] Relevant documentation updated
- [ ] TODO list updated (if applicable)
- [ ] Tests pass (if code changed)
- [ ] Files under 300 lines

### When to Create Documents

**Tier 1** (ARCHITECTURE_PLAN.md):
- New project starts
- Major architectural changes

**Tier 2** (docs/plans/):
- New feature (3+ hours)
- Major refactoring
- System integration
- Bug requiring investigation

**Tier 3** (docs/YYYY_MM_DD/):
- Daily work notes
- Design decisions
- Investigation results
- Session summaries

**Naming Convention** (MANDATORY):
- Format: `YYYYMMDD_NNNN_DESCRIPTIVE_NAME.md`
- NNNN = 4-digit sequence (0000, 0001, 0002, ...)
- Start each day at 0000
- Increment for each new document
- Examples:
  - `20251031_0000_SETUP_GUIDE.md`
  - `20251031_0001_PHASE_1_COMPLETE.md`
  - `20251031_0002_SESSION_SUMMARY.md`

---

## Summary

**Git Workflow**:
- Commit every 30-60 min
- Use appropriate template
- Include context and rationale

**Documentation**:
- 3 tiers for different purposes
- Plans for complex work (3+ hours)
- Daily docs for execution details

**TODO Lists**:
- Paired with every plan
- Cursor updates automatically
- Track time and progress

---

**Version**: 1.0.0  
**Last Updated**: 2025-10-31  
**Maintained By**: Global Rules Repository

---

**Follow this workflow for consistent, well-documented progress!**

