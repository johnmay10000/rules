# Global Rule Set Implementation Plan

**Created**: 2025-10-30 00:00  
**Status**: 🔄 PLANNING  
**Objective**: Create a generalized, tech-agnostic global rule set for Cursor based on existing FP guides and best practices

---

## Executive Summary

This plan outlines the creation of a **universal development rule set** that can be applied to any technology stack when using Cursor. The rule set will consolidate best practices from:

- Existing FP style guides (Python, TypeScript, Swift, Kotlin)
- Code style guidelines (code-style.mdc)
- Claude integration patterns (CLAUDE.md)
- How-to-use guides (how-to-use-fp-style-guides.md)

**Key Goals**:
1. ✅ **Tech-agnostic**: Works with any language/framework
2. ✅ **Cursor-optimized**: Designed specifically for Cursor usage
3. ✅ **Mandatory enforcement**: Critical rules that must be followed
4. ✅ **Comprehensive**: Covers development workflow, not just code style
5. ✅ **Actionable**: Clear, specific, enforceable guidelines

---

## Problem Statement

**Current State**:
- Multiple language-specific FP guides (Python, TypeScript, Swift, Kotlin)
- Project-specific rules (CLAUDE.md from another project)
- Code style guidelines mixed with deployment specifics
- No unified rule set for Cursor across all tech stacks
- Git checkpoint rules exist but not consistently defined
- Documentation folder structure not standardized globally

**Gaps**:
1. No universal rule set that works across all languages
2. No Cursor-specific integration guide (only Claude Code references)
3. Git checkpoint rules scattered across documents
4. Documentation structure rules not mandatory globally
5. Functional programming principles mixed with platform-specific details
6. No clear hierarchy of rules (mandatory vs recommended)

**Desired State**:
- Single, authoritative global rule set
- Clear mandatory vs recommended distinction
- Tech-agnostic where possible, with language-specific sections
- Cursor-optimized integration
- Standardized development workflow (git, docs, planning)

---

## Architecture Overview

### Three-Tier Rule System

```
┌─────────────────────────────────────────────────────┐
│  TIER 1: MANDATORY UNIVERSAL RULES                  │
│  - Git checkpoints (every 30-60 min)               │
│  - Documentation structure (docs/YYYY_MM_DD)       │
│  - Plan documents for complex work                 │
│  - File size limits (250-300 lines)                │
│  - Comprehensive testing                           │
└─────────────────────────────────────────────────────┘
                        ↓
┌─────────────────────────────────────────────────────┐
│  TIER 2: PROGRAMMING PARADIGM RULES                 │
│  - Functional Programming (FP) principles          │
│  - Type safety and ADTs                            │
│  - Pure functions and immutability                 │
│  - Pattern matching over conditionals              │
│  - Result types for error handling                 │
└─────────────────────────────────────────────────────┘
                        ↓
┌─────────────────────────────────────────────────────┐
│  TIER 3: LANGUAGE-SPECIFIC IMPLEMENTATIONS          │
│  - Python: returns, toolz, mypy                    │
│  - TypeScript: fp-ts, Effect                       │
│  - Swift: Result, Bow                              │
│  - Kotlin: Arrow                                   │
│  - (Other languages as needed)                     │
└─────────────────────────────────────────────────────┘
```

### Document Structure

```
CURSOR.md                    # Main global rule set
├── Tier 1: Mandatory Universal Rules
├── Tier 2: Programming Paradigm Rules  
├── Tier 3: Language-Specific Quick Reference
└── Cursor Integration Guide

CURSOR_FP_PRINCIPLES.md      # Deep dive into FP concepts
├── Universal FP patterns
├── Language implementations
└── Migration guides

CURSOR_WORKFLOW_GUIDE.md     # Development workflow
├── Git checkpoint strategy
├── Documentation structure
├── Plan document templates
└── Todo list management
```

---

## Implementation Phases

### Phase 1: Foundation (2 hours)

**Goal**: Extract and consolidate universal rules

**Tasks**:
1. ✅ Create docs/2025_10_30/ folder structure
2. Analyze existing documents for universal patterns
3. Extract mandatory rules (git, docs, planning)
4. Extract programming paradigm rules (FP principles)
5. Create rule hierarchy (mandatory vs recommended)
6. Define Cursor-specific integration points

**Deliverable**: Rule taxonomy document

### Phase 2: Core Document Creation (3 hours)

**Goal**: Create CURSOR.md main rule set

**Sections to include**:
1. **Mandatory Universal Rules** (TIER 1)
   - Git checkpoints (mandatory, every 30-60 min)
   - Documentation structure (mandatory, docs/YYYY_MM_DD)
   - Timestamped filenames (mandatory, YYYYMMDD_HHMM format)
   - Plan documents (mandatory for complex work)
   - File size limits (250-300 lines)
   - Comprehensive testing
   - Architecture TODO tracking

2. **Programming Paradigm Rules** (TIER 2)
   - Functional programming principles
   - Type safety and ADTs
   - Pure functions and immutability
   - Pattern matching over conditionals
   - Result/Either types for error handling
   - No defaults/fallbacks
   - Function composition

3. **Language-Specific Quick Reference** (TIER 3)
   - Python implementation patterns
   - TypeScript implementation patterns
   - Swift implementation patterns
   - Kotlin implementation patterns
   - Generic guidance for other languages

4. **Cursor Integration**
   - How to use rules in Cursor
   - Project setup with .cursorrules
   - Custom instructions format
   - Reference patterns (@filename)

**Deliverable**: CURSOR.md (main rule set)

### Phase 3: Supporting Documents (2 hours)

**Goal**: Create companion guides

**Documents**:
1. **CURSOR_FP_PRINCIPLES.md**
   - Deep dive into functional programming
   - Universal patterns across languages
   - Why FP matters (benefits)
   - Migration guide from imperative to FP

2. **CURSOR_WORKFLOW_GUIDE.md**
   - Git checkpoint best practices
   - Documentation structure templates
   - Plan document templates
   - Todo list management
   - Complex work management

3. **CURSOR_LANGUAGE_GUIDES/** (folder)
   - Extract language-specific sections
   - Create standalone guides per language
   - Link back to main CURSOR.md

**Deliverable**: 3 supporting documents + language guides folder

### Phase 4: Integration & Templates (1.5 hours)

**Goal**: Create ready-to-use templates

**Templates**:
1. `.cursorrules` template for new projects
2. Plan document template (PLAN.md)
3. Daily work summary template
4. Architecture TODO template
5. Git commit message template

**Examples**:
- Example .cursorrules for Python project
- Example .cursorrules for TypeScript project
- Example .cursorrules for polyglot project

**Deliverable**: templates/ folder with 5 templates + 3 examples

### Phase 5: Documentation & Testing (1.5 hours)

**Goal**: Finalize and validate

**Tasks**:
1. Update README.md to reference new global rule set
2. Create migration guide from existing guides to CURSOR.md
3. Create validation checklist
4. Test with sample project scenarios
5. Create troubleshooting section
6. Final review and polish

**Deliverable**: Complete, tested, documented rule set

---

## Success Criteria

### Must Have (Phase 1-2)
- ✅ CURSOR.md covers all mandatory rules
- ✅ Clear tier hierarchy (mandatory/recommended)
- ✅ Tech-agnostic where possible
- ✅ Cursor-specific integration guide
- ✅ Git checkpoints mandatory and documented
- ✅ Documentation structure mandatory (docs/YYYY_MM_DD)

### Should Have (Phase 3-4)
- ✅ Supporting documents for deep dives
- ✅ Templates ready to use
- ✅ Language-specific quick reference
- ✅ Examples for common scenarios

### Nice to Have (Phase 5)
- ✅ Migration guide from existing setup
- ✅ Troubleshooting section
- ✅ Visual diagrams
- ✅ Video walkthrough script

---

## Key Decisions

### 1. Mandatory Rules (Non-Negotiable)

**Git Checkpoints**:
- ✅ MANDATORY for all projects, all languages
- ✅ Frequency: Every 30-60 minutes of active work
- ✅ After any bug fix, feature implementation, or doc update
- ✅ Commit message format standardized

**Documentation Structure**:
- ✅ MANDATORY: docs/YYYY_MM_DD/ folder structure
- ✅ MANDATORY: Timestamped filenames (YYYYMMDD_HHMM_DESCRIPTION.md)
- ✅ Automatic chronological sorting
- ✅ Clear audit trail

**Plan Documents**:
- ✅ MANDATORY for complex work (3+ hours or multi-step)
- ✅ Living documents in docs/plans/
- ✅ Standard structure: Overview, Phases, Implementation, Success Criteria

**File Size Limits**:
- ✅ MANDATORY: 250-300 lines maximum per file
- ✅ Split into modules if necessary
- ✅ Do not modify functionality to reduce size

### 2. Programming Paradigm (Strongly Recommended)

**Functional Programming**:
- ✅ Pure functions (no side effects)
- ✅ Immutable data structures
- ✅ Pattern matching over conditionals
- ✅ ADTs for type safety
- ✅ Result/Either types for errors
- ✅ No defaults/fallbacks

**Rationale**: Applicable to most modern languages, improves maintainability

### 3. Language-Specific (Implementation Details)

- Python: returns, toolz, mypy
- TypeScript: fp-ts, Effect
- Swift: Result, Bow
- Kotlin: Arrow
- Others: Provide general guidance, reference libraries

---

## Document Outline: CURSOR.md

```markdown
# CURSOR.md - Universal Development Rules

## ⚠️ MANDATORY REQUIREMENTS (Tier 1)

### 1. Git Checkpoints (MANDATORY)
- Frequency and triggers
- Commit message format
- What to include

### 2. Documentation Structure (MANDATORY)
- docs/YYYY_MM_DD/ format
- Timestamped filenames
- Organization patterns

### 3. Plan Documents (MANDATORY)
- When required
- Standard structure
- Living document approach

### 4. File Size Limits (MANDATORY)
- 250-300 line maximum
- How to split
- Module organization

### 5. Testing (MANDATORY)
- Comprehensive test coverage
- Test-driven development
- Integration testing

## 🎯 PROGRAMMING PARADIGM (Tier 2)

### Functional Programming Principles
- Pure functions
- Immutability
- Pattern matching
- ADTs and type safety
- Result types
- No defaults/fallbacks

### Universal Patterns
- Error handling
- Function composition
- Data transformation
- State management

## 🔧 LANGUAGE-SPECIFIC QUICK REFERENCE (Tier 3)

### Python
- Libraries, tools, patterns

### TypeScript
- Libraries, tools, patterns

### Swift
- Libraries, tools, patterns

### Kotlin
- Libraries, tools, patterns

### Other Languages
- General guidance

## 📱 CURSOR INTEGRATION

### Project Setup
- .cursorrules file
- Custom instructions
- Reference patterns

### Using Rules in Cursor
- How Cursor reads rules
- Override patterns
- Project-specific additions

## 📚 ADDITIONAL RESOURCES

### Supporting Documents
- CURSOR_FP_PRINCIPLES.md
- CURSOR_WORKFLOW_GUIDE.md
- Language-specific deep dives

### Templates
- Plan documents
- Git commit messages
- Documentation structure

### Troubleshooting
- Common issues
- Solutions
```

---

## Timeline

| Phase | Duration | Start | End | Deliverable |
|-------|----------|-------|-----|-------------|
| Phase 1 | 2h | Day 1 | Day 1 | Rule taxonomy |
| Phase 2 | 3h | Day 1 | Day 1 | CURSOR.md |
| Phase 3 | 2h | Day 2 | Day 2 | Supporting docs |
| Phase 4 | 1.5h | Day 2 | Day 2 | Templates |
| Phase 5 | 1.5h | Day 2 | Day 2 | Final validation |
| **Total** | **10h** | - | - | Complete rule set |

---

## Risk Assessment

| Risk | Impact | Mitigation |
|------|--------|------------|
| Rules too specific | High | Keep Tier 1 tech-agnostic, move specifics to Tier 3 |
| Rules too generic | Medium | Provide concrete examples for each rule |
| Conflicts with existing guides | Low | Create migration guide, mark legacy docs |
| Adoption resistance | Medium | Clear benefits section, gradual adoption path |
| Maintenance overhead | Medium | Living document approach, update history section |

---

## Next Steps

1. ✅ Review this plan with stakeholders
2. ✅ Get approval for approach and timeline
3. ✅ Create todo list for tracking (20251030_0001_TODO_LIST.md)
4. Begin Phase 1: Foundation work
5. Git checkpoint after each phase completion

---

## Questions for Review

1. ✅ Is the three-tier architecture appropriate?
2. ✅ Should git checkpoints be truly mandatory for all projects?
3. ✅ Should file size limits (250-300 lines) apply to all languages?
4. ✅ Are there additional mandatory rules to include?
5. ✅ Should we include cloud-specific rules (AWS, GCP, Azure) in separate sections?
6. ✅ How should we handle non-FP paradigms (OOP, procedural)?

---

**Note**: This is a living plan document and will be updated as implementation progresses.

