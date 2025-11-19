---
title: Git Commit Conventions
category: universal_rules
type: git
applies_to: all
version: 1.0.0
last_updated: 2025-11-19
---

# Git Commit Conventions

Universal conventions for git commits across all projects and AI tools (Cursor, Kimi, Claude, Gemini).

---

## Commit Message Format

### Required Structure

Every commit message MUST follow this exact structure:

```
<Brief summary (50-72 characters)>

<Detailed description of changes>

<Rationale - why the change was made>

<Impact - what files/features affected>

<Context - relevant decisions or background>

Status: <Current state>

🤖 Generated with [Tool Name]
Co-Authored-By: [Tool] <noreply@[tool].com>
```

### All Elements Required

1. **Brief summary line** (50-72 characters)
   - Starts with capital letter
   - No period at the end
   - Imperative mood ("Add feature" not "Added feature")
   - Clear and descriptive

2. **Detailed description** (bullet points preferred)
   - What was changed in detail
   - Use markdown bullet points
   - Be specific about changes

3. **Rationale** 
   - Why this change was needed
   - Business or technical reasoning
   - Problem being solved

4. **Impact**
   - What files, features, or systems are affected
   - Scope of the change
   - Potential side effects

5. **Context**
   - Relevant decisions made
   - Background information
   - Links to issues or discussions

6. **Status indicator**
   - `Status: Task X complete ✅`
   - `Status: In progress 🔄`
   - `Status: Ready for review 👀`
   - `Status: Blocked 🚫`

7. **Co-author attribution**
   - Required for AI-assisted commits
   - Format: `Co-Authored-By: [Tool] <noreply@[tool].com>`

---

## Good Examples

### ✅ Phase Completion

```
Complete Phase 0 - Portability foundation

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

🤖 Generated with Cursor
Co-Authored-By: Claude <noreply@anthropic.com>
```

### ✅ Document Creation

```
Create Python FP style guide with Result patterns

Implemented comprehensive Python functional programming guide:
- Error handling with Result/Either types
- Monadic composition examples
- Railway-oriented programming patterns
- Currying and partial application
- Immutable data structures with dataclasses
- Complete training loop example

Rationale: Python ML projects need consistent FP patterns
for error handling and composability.

Impact: All Python projects can now reference this guide.
Reduces code review time for FP patterns.

Status: Core rules document complete ✅

🤖 Generated with Kimi
Co-Authored-By: Kimi <noreply@kimi.ai>
```

### ✅ Bug Fix

```
Fix race condition in user authentication

Fixed concurrent access issue in auth module:
- Added Mutex around shared state
- Implemented proper error handling for lock acquisition
- Added timeout to prevent deadlock
- Updated tests to cover concurrent scenarios

Rationale: Production logs showed intermittent auth failures
under high load due to data race.

Impact: Fixes #1234. Affects auth module only.
No breaking changes to API.

Status: Bug fix complete, ready for QA ✅

🤖 Generated with Cursor
Co-Authored-By: Claude <noreply@anthropic.com>
```

### ✅ Template Set

```
Create all portable project templates

Created 3 smart .cursorrules templates:
- _envvar: Uses ${CURSOR_RULES_PATH} for portability
- _submodule: Uses relative paths for self-contained
- Basic template for simple projects

All include auto-detection logic for Python, TypeScript, GCP, AWS.
Tested on macOS and Linux.

Rationale: Different projects have different portability needs.
One size does not fit all.

Impact: Users can choose appropriate template for their setup.
Reduces setup friction.

Status: Templates complete ✅

🤖 Generated with Cursor
Co-Authored-By: Claude <noreply@anthropic.com>
```

---

## Anti-Patterns (DO NOT DO)

### ❌ Too Vague

```
Update files

Fixed some stuff.

Status: Done

🤖 Generated with Cursor
Co-Authored-By: Claude <noreply@anthropic.com>
```

**Problems**:
- No context about what was changed
- No rationale
- No impact assessment
- Cannot revert safely
- No learning value for team

---

### ❌ No Context

```
WIP

Status: In progress

🤖 Generated with Kimi
Co-Authored-By: Kimi <noreply@kimi.ai>
```

**Problems**:
- "WIP" is not descriptive
- No information about what's being worked on
- Breaks the checkpoint system
- Makes git history useless

---

### ❌ Multiple Unrelated Changes

```
Create CURSOR.md, fix typos, update README, add examples, refactor templates

Did a bunch of stuff.

Status: Done

🤖 Generated with Cursor
Co-Authored-By: Claude <noreply@anthropic.com>
```

**Problems**:
- Should be 5 separate commits
- Cannot revert one change without reverting all
- Hard to review
- Breaks logical grouping
- Makes bisecting impossible

---

### ❌ No Status Indicator

```
Add new feature X

Implemented feature X.

Rationale: Needed for project.

Impact: Affects module Y.

🤖 Generated with Cursor
Co-Authored-By: Claude <noreply@anthropic.com>
```

**Problems**:
- Missing status indicator
- Cannot track progress at a glance
- Breaks automation that relies on status format

---

## Checkpoint Triggers

### When to Commit (Mandatory)

Commit **MUST** occur after:

1. **Every 30-60 minutes** - Maximum time between commits during active work
2. **Completing a phase** - Every phase completion (Phase 0, Phase 1, etc.)
3. **Creating core documents** - Major deliverables, guides, plans
4. **Completing logical units** - Set of related templates, examples, or files
5. **Before context switch** - Switching between different work areas or projects
6. **After bug fixes** - Any fix that resolves an issue
7. **After documentation updates** - Significant documentation changes
8. **At end of work session** - Daily checkpoint before stopping work

### Why Small, Frequent Commits?

**Benefits**:
1. **Prevents work loss** - Crashes, errors, or accidents don't lose hours of work
2. **Easy to review** - Small changes are easier to understand and review
3. **Easy to revert** - Can revert specific changes without losing everything
4. **Clear history** - Git log tells a coherent story of development
5. **Bisect-friendly** - `git bisect` can pinpoint issues quickly
6. **Context preservation** - Each commit preserves context and decisions
7. **Demonstrates thoroughness** - Shows methodical, disciplined approach

---

## Commit Verification Checklist

Before committing, verify:

- [ ] Summary line is 50-72 characters
- [ ] Summary starts with capital letter, no period
- [ ] Summary uses imperative mood ("Add" not "Added")
- [ ] Detailed description explains what was changed
- [ ] Rationale explains why the change was needed
- [ ] Impact describes what files/features are affected
- [ ] Context includes relevant decisions or background
- [ ] Status indicator is present and accurate
- [ ] Co-author attribution is included
- [ ] All modified files have been reviewed
- [ ] Changes are logically grouped (not unrelated)
- [ ] Commit is within 30-60 minute window (or is a phase/document completion)

---

## Branching Strategy

See [branching_strategy.md](./branching_strategy.md) for:
- Main branch protection rules
- Feature branch naming conventions
- Pull request requirements
- Merge strategies

---

## Related Documents

- [git_checkpoint_rules.md](./git_checkpoint_rules.md) - Detailed checkpoint workflow
- [branching_strategy.md](./branching_strategy.md) - Branch management
- [../project_structure/directory_layout.md](../project_structure/directory_layout.md) - Where to place files

---

**Last Updated**: 2025-11-19  
**Maintained By**: Global AI Rules System  
**Status**: Active  
**Applies To**: All AI assistants (Cursor, Kimi, Claude, Gemini)