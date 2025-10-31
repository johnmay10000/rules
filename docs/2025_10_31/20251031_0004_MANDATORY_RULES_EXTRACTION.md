# Mandatory Rules Extraction

**Created**: 2025-10-31 10:30  
**Phase**: Phase 1 - Foundation  
**Purpose**: Extract and consolidate mandatory rules for CURSOR.md  
**Source**: Rule Taxonomy (20251031_0003_RULE_TAXONOMY.md)

---

## Mandatory Universal Rules (Tier 1)

These rules apply to ALL projects, ALL tech stacks. Non-negotiable.

---

## 1. GIT WORKFLOW (MANDATORY)

### 1.1 Git Checkpoints - When to Commit

**CRITICAL**: Commits are MANDATORY at these checkpoints:

✅ **Every 30-60 minutes** - Maximum time between commits during active work  
✅ **After completing a phase** - Any phase in multi-phase work  
✅ **After creating core documents** - Major deliverables (CURSOR.md, SETUP_GUIDE.md)  
✅ **After completing logical units** - Set of related files (templates, examples)  
✅ **Before context switch** - Switching between different work areas  
✅ **After bug fixes** - Any fix that resolves an issue  
✅ **After documentation updates** - Significant doc changes  
✅ **At end of work session** - Daily checkpoint before stopping

**Rationale**:
- Prevents work loss (crashes, errors)
- Clear project history
- Easy to review progress
- Easy to revert if needed
- Context preservation across sessions

---

### 1.2 Commit Message Format (MANDATORY)

**Required Format**:
```
<Brief summary line (50-72 chars)>

<Detailed description of what was done>

<Why it was done (rationale)>

<What deliverables/files were affected>

<Any relevant context or decisions>

Status: <Current state>

🤖 Generated with Cursor
Co-Authored-By: Claude <noreply@anthropic.com>
```

**Required Elements**:
1. ✅ **Brief summary** (50-72 characters max)
2. ✅ **Detailed description** (bullet points describing changes)
3. ✅ **Rationale** (why the change was made)
4. ✅ **Files affected** (what was changed)
5. ✅ **Context/decisions** (relevant background)
6. ✅ **Status indicator** (current state of work)
7. ✅ **Co-author attribution** (Cursor signature)

**Good Example**:
```
Complete Phase 0: Portability foundation ✅

Created all portability deliverables:
- SETUP_GUIDE.md (machine setup for all platforms)
- .cursorrules_smart_template_envvar (portable via env var)
- .cursorrules_smart_template_submodule (self-contained)
- Updated FILE_LOCATIONS_USER_GUIDE.md

All templates now use portable paths.
Ready to proceed to Phase 1.

Status: Phase 0 complete ✅

🤖 Generated with Cursor
Co-Authored-By: Claude <noreply@anthropic.com>
```

**Bad Examples**:
```
# TOO VAGUE
Update files

# NO CONTEXT  
WIP

# MULTIPLE UNRELATED (should be 3 commits)
Create CURSOR.md, fix typos, update README
```

---

### 1.3 Checkpoint Strategy

**Small, Frequent Commits**:
- Each core document → separate commit
- Each template set → separate commit
- Each example set → separate commit
- Each phase → separate commit
- Each significant update → separate commit

**Benefits**:
1. Easy to review progress
2. Easy to revert if needed
3. Clear history of decisions
4. Demonstrates thoroughness
5. Enables context preservation

---

## 2. DOCUMENTATION STRUCTURE (MANDATORY)

### 2.1 Three-Tier Documentation Hierarchy

**CRITICAL**: All projects MUST follow this structure

#### Tier 1: Strategic - ARCHITECTURE_PLAN.md

**Location**: Project root  
**Purpose**: High-level architecture, major components, technology choices  
**Updated**: Rarely (only on major changes)  
**Format**: `ARCHITECTURE_PLAN.md` (no timestamp)

**Required Content**:
- System architecture overview
- Major components and interactions
- Technology stack and rationale
- Links to Tier 2 plans
- High-level success criteria

**Example Structure**:
```markdown
# Project Architecture Plan

## Overview
High-level system description

## Major Components
- Component 1: Purpose and tech
- Component 2: Purpose and tech

## Technology Choices
- Language: X because Y
- Platform: X because Y

## Sub-Plans
- [Feature A Plan](docs/plans/FEATURE_A_PLAN.md)
- [Feature B Plan](docs/plans/FEATURE_B_PLAN.md)
```

---

#### Tier 2: Tactical - docs/plans/*.md

**Location**: `docs/plans/`  
**Purpose**: Feature/area-specific plans with implementation details  
**Updated**: Regularly as features progress  
**Format**: `FEATURE_NAME_PLAN.md` + `FEATURE_NAME_TODO.md` (paired)

**Required for**:
- New features (3+ hours)
- Architectural changes
- Bug fixes requiring investigation
- System integrations
- Multi-step refactoring

**Plan Document Structure**:
```markdown
# Feature Name Plan

**Status**: ⏳ IN PROGRESS / ✅ COMPLETE  
**Last Updated**: YYYY-MM-DD HH:MM  
**Estimated Time**: X hours

## Overview
What needs to be done and why

## Phases
- [ ] Phase 1: Description (X hours)
- [ ] Phase 2: Description (Y hours)
- [ ] Phase 3: Description (Z hours)

## Implementation Details
- Code locations: where files will be
- ADT structures: what types will be created
- Key functions: main entry points

## Success Criteria
- Criterion 1: How to verify
- Criterion 2: How to verify

## Update History
### YYYY-MM-DD HH:MM
- Completed Phase 1
- Started Phase 2
- Encountered issue X, resolved with Y
```

**Paired TODO List Structure**:
```markdown
# Feature Name TODO List

**Plan Reference**: [FEATURE_NAME_PLAN.md](FEATURE_NAME_PLAN.md)  
**Status**: ⏳ IN PROGRESS  
**Last Updated**: YYYY-MM-DD HH:MM  
**Progress**: X/Y tasks complete (Z%)

## Phase 1: Phase Name

**Status**: ⏳ IN PROGRESS  
**Progress**: X/Y tasks complete

- [x] Task 1.1: Description (Estimated: 2h, Actual: 1.5h)
- [x] Task 1.2: Description (Estimated: 1h, Actual: 1.2h)
- [ ] Task 1.3: Description (Estimated: 3h, Actual: -)
- [ ] Task 1.4: Description (Estimated: 1h, Actual: -)

**Phase Total**: Estimated 7h | Actual 2.7h | Remaining ~4.3h

## Overall Progress

**Total Tasks**: X completed / Y total (Z%)  
**Total Time**: Estimated 10h | Actual 2.7h | Remaining ~7.3h  
**Completion Rate**: XX% complete

## Update History

### YYYY-MM-DD HH:MM
- Completed tasks 1.1, 1.2
- Started task 1.3
- Phase 1: 50% complete
```

**Cursor's Responsibility**: Automatically update TODO lists as work progresses
- Mark tasks complete [x]
- Add actual time spent
- Update progress percentages
- Add update history entries with timestamps

---

#### Tier 3: Execution - docs/YYYY_MM_DD/*.md

**Location**: `docs/2025_10_31/` (dated folders)  
**Purpose**: Daily work logs, decisions, session summaries  
**Updated**: Frequently (multiple times per day)  
**Format**: `YYYYMMDD_HHMM_DESCRIPTIVE_NAME.md` (WITH timestamp)

**Filename Format (STRICT)**:
```
YYYYMMDD_HHMM_DESCRIPTIVE_NAME.md
```

**Examples**:
- ✅ `20251031_0900_CURSOR_MD_IMPLEMENTATION.md`
- ✅ `20251031_1530_BUG_FIX_VALIDATION.md`
- ✅ `20251031_2100_SESSION_SUMMARY.md`
- ❌ `2025_10_31_SUMMARY.md` (has separators - WRONG)
- ❌ `SUMMARY.md` (missing timestamp - WRONG)
- ❌ `20251031_SUMMARY.md` (missing time - WRONG)

**Rationale**:
- Files automatically sort chronologically
- Exact creation date AND time visible in filename
- No ambiguity about which document is latest
- Easy to find work from specific time periods
- Prevents filename collisions on same day

**Content Types**:
- Implementation notes
- Decision logs
- Bug investigation
- Session summaries
- Analysis documents

---

### 2.2 Cross-Referencing Rules

**MANDATORY**: Documents MUST cross-reference appropriately

**Tier 1 → Tier 2**: Links to all sub-plans  
**Tier 2 → Tier 1**: Reference to architecture plan  
**Tier 2 → Tier 3**: Links to daily work documents for detailed notes  
**Tier 3 → Tier 2**: Reference to plan being worked on

**Example**:
```markdown
# In ARCHITECTURE_PLAN.md
## Sub-Plans
- [Portability Setup](docs/plans/PORTABILITY_PLAN.md)

# In docs/plans/PORTABILITY_PLAN.md
**Architecture**: See [ARCHITECTURE_PLAN.md](../../ARCHITECTURE_PLAN.md)
**Daily Work**: See [docs/2025_10_31/](../2025_10_31/)

# In docs/2025_10_31/20251031_0900_WORK.md
**Plan**: [Portability Plan](../plans/PORTABILITY_PLAN.md)
```

---

## 3. TESTING REQUIREMENTS (MANDATORY)

### 3.1 Core Testing Rule

**CRITICAL**: ALL production code MUST have comprehensive tests that pass before committing

### 3.2 "Comprehensive" Defined

**Minimum Test Coverage**:

1. ✅ **Happy Path** - Normal expected behavior
   - Valid inputs produce correct outputs
   - Main flow executes successfully
   - Example: User registration with valid data

2. ✅ **Error Cases** - All failure modes explicitly tested
   - Invalid inputs handled correctly
   - Expected errors return proper error types
   - Example: User registration with invalid email

3. ✅ **Edge Cases** - Boundary conditions
   - Empty inputs
   - Null/undefined values
   - Maximum/minimum values
   - Large inputs
   - Example: Registration with 1-char or 1000-char name

4. ✅ **Regression Tests** - Past bugs stay fixed
   - Tests for previously fixed bugs
   - Ensures fixes don't regress
   - Example: Test for bug #123 that was fixed

### 3.3 Minimum Coverage Targets

**Functions with Business Logic**: 3+ tests minimum
- 1 happy path
- 1 error case
- 1 edge case

**Business Logic Modules**: 80%+ coverage
- Core business rules fully tested
- All branches covered
- All error paths tested

**Utility Functions**: 2+ tests minimum
- 1 happy path
- 1 error case

**Integration Points**: 100% error paths
- All external system failures tested
- Network errors handled
- Database failures handled

### 3.4 Special Requirements

**FP Migration Testing** (CRITICAL):
- When migrating old code to FP style
- Old code and new code must have equivalent test coverage
- Can't remove tests during migration
- Must add tests for new error handling paths

**Test Quality**:
- No test skipping without documented reason
- All tests must pass before commit (no broken tests allowed)
- Tests must be deterministic (same input → same result)
- No flaky tests (tests that randomly fail)

### 3.5 Language-Specific Test Tools

**Python**:
- `pytest` - Test framework
- `uv run pytest` - Run tests (mandatory to use `uv`)
- `pytest-cov` - Coverage reports

**TypeScript**:
- `jest` OR `vitest` - Test framework
- `@testing-library/*` - UI testing (if applicable)

**Swift**:
- XCTest - Built-in test framework

**Kotlin**:
- JUnit - Core testing
- Kotest - FP-friendly testing

---

## 4. FILE SIZE LIMITS (MANDATORY)

### 4.1 Universal File Length Rule

**CRITICAL**: ALL code files MUST be 250-300 lines maximum

**Target**: 250-300 lines per file  
**Absolute Maximum**: 350 lines (with justification)

**Applies To**:
- Python files (`.py`)
- TypeScript files (`.ts`, `.tsx`)
- Swift files (`.swift`)
- Kotlin files (`.kt`)
- Any other code files

### 4.2 How to Comply

**When file exceeds 300 lines**:

1. ✅ **Split into multiple files** if necessary
2. ✅ **Create folder for module** - organize related files
3. ✅ **Organize by feature** - group by functionality
4. ✅ **Organize by responsibility** - separation of concerns

**CRITICAL**: Do NOT modify functionality to reduce file size!
- Don't remove features
- Don't consolidate different concerns
- Don't compromise code quality
- Just split logically

### 4.3 Acceptable Exceptions

**With Justification** (document in file):
- Generated code (must mark as generated)
- Single complex algorithm that can't be split (rare)
- Configuration files (if unavoidable)

**Must Document**:
```python
# WARNING: This file exceeds 300 lines (currently 350)
# Reason: Complex ML training algorithm that can't be meaningfully split
# without losing coherence. Attempted split in commit abc123 was reverted
# due to decreased readability and increased coupling.
# Approved by: [Name] on [Date]
```

### 4.4 Rationale

**Why 250-300 lines**:
- ✅ Maintains readability (fits on ~2 screens)
- ✅ Forces focused modules (single responsibility)
- ✅ Easier code review (reviewers can understand quickly)
- ✅ Better testing (smaller modules easier to test)
- ✅ Reduces merge conflicts (smaller files = fewer conflicts)
- ✅ Encourages composition (build from small parts)

---

## Summary

### Mandatory Rules Count

**Total Mandatory Rules**: 20

**Category Breakdown**:
- Git Workflow: 7 rules
- Documentation Structure: 8 rules
- Testing Requirements: 3 rules
- File Size Limits: 2 rules

### Enforcement

**ALL Tier 1 rules are**:
- ✅ Non-negotiable
- ✅ Apply to ALL projects
- ✅ Apply to ALL tech stacks
- ✅ No exceptions without documented justification
- ✅ Cursor must enforce automatically

### Integration into CURSOR.md

These rules will form the **core** of CURSOR.md:
- Section 1: Git Workflow
- Section 2: Documentation Structure
- Section 3: Testing Requirements
- Section 4: File Size Limits

**Then**:
- Section 5: Recommended Patterns (Tier 2 from taxonomy)
- Section 6: Language-Specific Rules (defer to guides)
- Section 7: Platform-Specific Rules (defer to guides)

---

**Extraction Complete**: Ready for CURSOR.md integration

**Status**: Mandatory rules clearly defined, ready to generalize across all tech stacks

