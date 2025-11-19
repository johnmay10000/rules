---
title: Daily Workflow Guide
category: universal_rules
type: documentation
applies_to: all
version: 1.0.0
last_updated: 2025-11-19
---

# Daily Workflow Guide

**Status**: MANDATORY - All work must follow this daily workflow structure

This document defines the universal daily workflow for all projects, applicable across all AI assistants (Cursor, Kimi, Claude, Gemini) and programming languages.

---

## Overview

The daily workflow provides a consistent, repeatable structure for organizing work, tracking progress, and maintaining clear documentation. It ensures that:
- Work is discoverable and organized by date
- Progress is tracked systematically
- Context is preserved across sessions
- Git checkpoints align with work documentation
- Team members can understand what was done and when

---

## Daily Work Folder Structure

### Required Directory Layout

```
project/
└── docs/
    └── YYYY_MM_DD/              # Daily work folder (e.g., 2025_11_19)
        ├── YYYYMMDD_0000_*.md   # First file of the day
        ├── YYYYMMDD_0001_*.md   # Second file
        ├── YYYYMMDD_0002_*.md   # Third file
        └── ...
```

### Folder Naming Convention

- **Format**: `docs/YYYY_MM_DD/`
- **Year**: 4 digits (e.g., `2025`)
- **Month**: 2 digits, zero-padded (e.g., `11` for November)
- **Day**: 2 digits, zero-padded (e.g., `19`)

**Examples**:
- `docs/2025_11_19/` (November 19, 2025)
- `docs/2025_01_05/` (January 5, 2025)
- `docs/2025_12_31/` (December 31, 2025)

---

## File Naming Convention

### Format: `YYYYMMDD_NNNN_DESCRIPTIVE_NAME.md`

**Components**:
1. **Date**: `YYYYMMDD` (e.g., `20251119`)
2. **Sequence**: `NNNN` (4-digit number, starting from `0000`)
3. **Description**: `DESCRIPTIVE_NAME` (kebab-case, uppercase)
4. **Extension**: `.md` (Markdown)

**Rules**:
- Sequence numbers start at `0000` for each new day
- Increment by 1 for each new file (`0000`, `0001`, `0002`, etc.)
- Description must be descriptive and in kebab-case
- Use uppercase for acronyms and abbreviations
- No spaces, use underscores between components only

### Examples

**Good Examples**:
```
20251119_0000_MORNING_PLANNING.md
20251119_0001_SESSION_SUMMARY.md
20251119_0002_BUG_FIX_INVESTIGATION.md
20251119_0003_PHASE_3_COMPLETE.md
20251119_0004_MCP_SERVER_IMPLEMENTATION.md
20251119_0005_END_OF_DAY_CHECKPOINT.md
```

**Bad Examples**:
```
20251119_0000_file1.md                    # Not descriptive
20251119_1_bug.md                         # Wrong sequence format
20251119_0000_my file.md                 # Spaces not allowed
20251119_0000_myFile.md                   # Not kebab-case
2025_11_19_0000_planning.md              # Wrong date format
```

---

## Daily Workflow Routine

### Morning (Start of Work Session)

**Time**: When you begin work for the day

**Actions**:
1. **Check repository status**
   ```bash
   git status
   git log --oneline -n 5
   ```

2. **Review previous day's work**
   ```bash
   ls -la docs/$(date -d "yesterday" +%Y_%m_%d)/
   cat docs/$(date -d "yesterday" +%Y_%m_%d)/*_SESSION_SUMMARY.md
   ```

3. **Review TODO lists**
   ```bash
   cat docs/plans/CURRENT_FEATURE_TODO.md
   ```

4. **Create daily work folder**
   ```bash
   mkdir -p docs/$(date +%Y_%m_%d)
   ```

5. **Create morning planning document**
   ```bash
   cat > docs/$(date +%Y_%m_%d)/$(date +%Y%m%d)_0000_MORNING_PLANNING.md << 'EOF'
   # Morning Planning - YYYY-MM-DD

   ## Today's Goals
   - [ ] Goal 1
   - [ ] Goal 2
   - [ ] Goal 3

   ## Priority Order
   1. Highest priority task
   2. Second priority task
   3. Third priority task

   ## Blockers from Yesterday
   - Blocker 1 (if any)
   - Blocker 2 (if any)

   ## Resources Needed
   - Access to X system
   - Documentation for Y feature

   ## Notes
   - Any relevant context or reminders
   EOF
   ```

6. **Git checkpoint for morning start**
   ```bash
   git add docs/
   git commit -m "Start day YYYY-MM-DD - Morning planning

   Created morning planning document with:
   - Today's goals: 3 tasks
   - Priority order defined
   - Blockers identified: None
   - Resources needed: Documented

   Status: Morning planning complete 🔄

   🤖 Generated with [AI Assistant]
   Co-Authored-By: AI <noreply@anthropic.com>"
   ```

---

### During Work Session (Checkpoint Cycle)

**Frequency**: Every 30-60 minutes, or after completing a logical unit

**Actions**:
1. **Assess what was accomplished**
   - Review code changes
   - Review documentation changes
   - Identify completed tasks

2. **Create checkpoint document** (if significant progress)
   ```bash
   # Determine next sequence number
   ls docs/$(date +%Y_%m_%d)/
   # If last file is 20251119_0003_*, next is 0004
   
   cat > docs/$(date +%Y_%m_%d)/$(date +%Y%m%d)_0004_CHECKPOINT.md << 'EOF'
   # Checkpoint - YYYY-MM-DD HH:MM

   ## Completed Since Last Checkpoint
   - [x] Task A completed
   - [x] Task B completed
   - [ ] Task C in progress

   ## Time Spent
   - Task A: 0.5 hours
   - Task B: 0.75 hours
   - Total: 1.25 hours

   ## Key Decisions Made
   - Decision 1: Chose approach X over Y because...

   ## Issues Encountered
   - Issue 1: Description
   - Resolution: How it was resolved (or "Still investigating")

   ## Next Steps
   - Continue with Task C
   - Start Task D

   ## Git Checkpoint
   - Commit hash: [to be filled after git commit]
   - Files changed: list of files

   EOF
   ```

3. **Git checkpoint** (MANDATORY every 30-60 minutes)
   ```bash
   # Review changes
   git status
   git diff
   
   # Stage changes
   git add <files>
   
   # Commit with descriptive message
   git commit -m "Brief summary (50-72 chars)

   Detailed description of changes made

   Rationale for the changes

   Files affected: file1.md, file2.md

   Status: Checkpoint - Task X complete 🔄

   🤖 Generated with [AI Assistant]
   Co-Authored-By: AI <noreply@anthropic.com>"
   
   # Update checkpoint document with commit hash
   # Edit the checkpoint file to add commit hash
   ```

---

### End of Work Session (Daily Wrap-up)

**Time**: When you stop work for the day

**Actions**:
1. **Review all work completed**
   ```bash
   git log --oneline --since="9:00 AM"
   ls docs/$(date +%Y_%m_%d)/
   ```

2. **Create end-of-day summary document**
   ```bash
   # Determine next sequence number
   cat > docs/$(date +%Y_%m_%d)/$(date +%Y%m%d)_NNNN_SESSION_SUMMARY.md << 'EOF'
   # Session Summary - YYYY-MM-DD

   ## Work Session: [Morning/Afternoon/Full Day]

   ## Accomplished Today
   - [x] Task 1 completed
   - [x] Task 2 completed
   - [x] Task 3 completed
   - [ ] Task 4 in progress (80% complete)

   ## Time Tracking
   | Task | Estimated | Actual | Variance |
   |------|-----------|--------|----------|
   | Task 1 | 1.0h | 1.5h | +0.5h |
   | Task 2 | 2.0h | 1.75h | -0.25h |
   | Task 3 | 0.5h | 0.5h | 0h |
   | **Total** | **3.5h** | **3.75h** | **+0.25h** |

   ## Key Decisions Made Today
   1. Decision 1: What was decided and why
   2. Decision 2: What was decided and why

   ## Blockers Encountered
   - Blocker 1: Description
     - Status: Resolved / Still blocked
     - Action needed: What needs to happen
   - Blocker 2: Description
     - Status: Still blocked
     - Action needed: What needs to happen

   ## What Went Well
   - Positive aspect 1
   - Positive aspect 2

   ## What Could Be Improved
   - Improvement area 1
   - Improvement area 2

   ## Next Session Plan
   ### Tomorrow's Priorities
   1. Complete Task 4 (remaining 20%)
   2. Start Task 5
   3. Review and address blockers

   ### Upcoming Tasks
   - Task 6
   - Task 7

   ## Git Summary
   - Total commits today: N
   - Key commits:
     - commit-hash: Brief description
     - commit-hash: Brief description

   ## Files Created/Modified Today
   - docs/2025_11_19/20251119_0000_MORNING_PLANNING.md
   - docs/2025_11_19/20251119_0001_CHECKPOINT.md
   - src/module/file.py
   - tests/test_module.py

   ## AI Assistant Notes
   - Tools used: SetTodoList, parallel execution, subagents
   - Patterns applied: Railway-oriented programming, immutable updates
   - Issues encountered: None significant

   ## Overall Status
   Status: Day complete - 3 of 4 tasks finished ✅
   Confidence: High / Medium / Low

   Mood: 😊 / 😐 / 😔
   Energy level: High / Medium / Low

   EOF
   ```

3. **Final git checkpoint for the day**
   ```bash
   # Stage all remaining changes
   git add .
   
   # Commit end-of-day summary
   git commit -m "End of day YYYY-MM-DD - Session summary

   Completed work session with:
   - Tasks completed: 3 of 4
   - Total time: 3.75 hours
   - Key decisions: 2 major decisions documented
   - Blockers: 1 blocker identified and documented
   - Files created: 2 docs, 3 src files, 2 test files

   Status: Day complete ✅

   🤖 Generated with [AI Assistant]
   Co-Authored-By: AI <noreply@anthropic.com>"
   
   # Push to remote (if applicable)
   git push origin main
   ```

4. **Update master TODO list**
   ```bash
   # Edit docs/plans/CURRENT_FEATURE_TODO.md
   # Mark today's completed tasks with [x]
   # Update actual time spent
   # Add notes about blockers
   ```

5. **Plan next day (optional but recommended)**
   ```bash
   cat > docs/$(date -d "tomorrow" +%Y_%m_%d)/$(date -d "tomorrow" +%Y%m%d)_0000_MORNING_PLANNING.md << 'EOF'
   # Morning Planning - YYYY-MM-DD (Tomorrow)
   
   ## Goals for Tomorrow
   - [ ] Goal 1 (from today's blockers)
   - [ ] Goal 2
   - [ ] Goal 3
   
   ## Priority Order
   1. Address blocker from today
   2. Continue in-progress task
   3. Start next planned task
   
   ## Notes from Today
   - Carry over: Task X (80% complete)
   - Blocker to address: Y
   EOF
   ```

---

## File Content Templates

### Morning Planning Template

```markdown
# Morning Planning - YYYY-MM-DD

## Today's Goals
- [ ] Primary goal for the day
- [ ] Secondary goal
- [ ] Tertiary goal

## Priority Order
1. Most important task
2. Second most important
3. Third most important

## Blockers from Yesterday
- [ ] Blocker 1 (if any)
- [ ] Blocker 2 (if any)

## Resources Needed
- Access to X system
- Documentation for Y feature
- Review of Z design

## Time Allocation
- Task 1: 2 hours
- Task 2: 3 hours
- Task 3: 1 hour
- Buffer: 2 hours

## Notes
- Any relevant context, reminders, or observations
```

### Checkpoint Template

```markdown
# Checkpoint - YYYY-MM-DD HH:MM

## Completed Since Last Checkpoint
- [x] Task A completed
- [x] Task B completed
- [ ] Task C in progress

## Time Spent
- Task A: 0.5 hours
- Task B: 0.75 hours
- Total: 1.25 hours

## Key Decisions Made
- Decision 1: What was decided and why
- Decision 2: What was decided and why

## Issues Encountered
- Issue 1: Description
  - Resolution: How it was resolved (or "Still investigating")
- Issue 2: Description
  - Resolution: How it was resolved

## Next Steps
- Continue with Task C
- Start Task D
- Address Issue 1 if not resolved

## Git Checkpoint
- Commit: abc123def456
- Files changed: file1.py, file2.md, file3.ts

## AI Assistant Notes
- Tools used: SetTodoList, parallel execution
- Patterns applied: Railway-oriented programming
```

### Session Summary Template

```markdown
# Session Summary - YYYY-MM-DD

## Work Session: [Morning/Afternoon/Full Day]

## Accomplished Today
- [x] Task 1 completed
- [x] Task 2 completed
- [x] Task 3 completed
- [ ] Task 4 in progress (80% complete)

## Time Tracking
| Task | Estimated | Actual | Variance |
|------|-----------|--------|----------|
| Task 1 | 1.0h | 1.5h | +0.5h |
| Task 2 | 2.0h | 1.75h | -0.25h |
| Task 3 | 0.5h | 0.5h | 0h |
| **Total** | **3.5h** | **3.75h** | **+0.25h** |

## Key Decisions Made Today
1. Decision 1: What was decided and why
2. Decision 2: What was decided and why

## Blockers Encountered
- Blocker 1: Description
  - Status: Resolved / Still blocked
  - Action needed: What needs to happen
- Blocker 2: Description
  - Status: Still blocked
  - Action needed: What needs to happen

## What Went Well
- Positive aspect 1
- Positive aspect 2

## What Could Be Improved
- Improvement area 1
- Improvement area 2

## Next Session Plan
### Tomorrow's Priorities
1. Complete Task 4 (remaining 20%)
2. Start Task 5
3. Review and address blockers

### Upcoming Tasks
- Task 6
- Task 7

## Git Summary
- Total commits today: N
- Key commits:
  - abc123: Brief description
  - def456: Brief description

## Files Created/Modified Today
- docs/2025_11_19/20251119_0000_MORNING_PLANNING.md
- docs/2025_11_19/20251119_0001_CHECKPOINT.md
- src/module/file.py
- tests/test_module.py

## AI Assistant Notes
- Tools used: SetTodoList, parallel execution, subagents
- Patterns applied: Railway-oriented programming, immutable updates
- Issues encountered: None significant

## Overall Status
Status: Day complete - 3 of 4 tasks finished ✅
Confidence: High / Medium / Low

Mood: 😊 / 😐 / 😔
Energy level: High / Medium / Low
```

---

## Integration with Git Checkpoints

### Every Git Commit Should Reference Daily Work

When creating a git commit, reference the corresponding daily work file:

```bash
git commit -m "Complete user authentication module ✅

Implemented JWT-based authentication:
- Added login endpoint (/api/login)
- Added token validation middleware
- Added password hashing with bcrypt
- Created AuthService with unit tests

Rationale: Required for user management feature
Addresses requirement from morning planning doc.

Files affected:
- src/auth/service.py
- src/auth/middleware.py
- tests/test_auth.py
- docs/2025_11_19/20251119_0003_AUTH_IMPLEMENTATION.md

Status: Task complete ✅

🤖 Generated with [AI Assistant]
Co-Authored-By: AI <noreply@anthropic.com>"
```

### Linking Commits to Daily Work

In your daily work files, always record the git commit hash:

```markdown
## Git Checkpoint
- Commit: abc123def456789
- Branch: feature/user-auth
- Files changed: 3 files (+150 -20 lines)
```

This creates bidirectional traceability:
- **Commit → Daily Work**: Commit message references the planning doc
- **Daily Work → Commit**: Planning doc records the commit hash

---

## Best Practices

### ✅ DO:

- **Create daily folder every day** - Even if no work done (can be empty)
- **Use sequence numbers consistently** - Always increment by 1
- **Be descriptive in filenames** - Anyone should understand the content from the name
- **Cross-reference everything** - Link to plans, link commits to docs
- **Review previous day** - Always check yesterday's summary before starting
- **Track time honestly** - Don't underestimate or overestimate
- **Document blockers immediately** - Don't wait until end of day
- **Use templates** - Copy-paste templates for consistency

### ❌ DON'T:

- **Skip days** - Every work day must have a folder
- **Reuse sequence numbers** - Each file must have unique sequence
- **Use vague filenames** - `20251119_0000_STUFF.md` is not acceptable
- **Mix unrelated content** - One file per logical unit/topic
- **Forget git checkpoints** - Must commit every 30-60 minutes
- **Work without a plan** - Always start with morning planning
- **Ignore blockers** - Document and escalate blockers immediately
- **Leave without summary** - Always create end-of-day summary

---

## Weekly and Monthly Review

### Weekly Review (Every Friday)

Create a weekly summary document:

```bash
cat > docs/$(date +%Y_%m_%d)/$(date +%Y%m%d)_NNNN_WEEKLY_REVIEW.md << 'EOF'
# Weekly Review - Week of YYYY-MM-DD

## Summary
- Days worked: X
- Total commits: Y
- Tasks completed: Z

## Major Accomplishments
1. Accomplishment 1
2. Accomplishment 2

## Challenges
- Challenge 1 and how it was addressed

## Next Week's Priorities
1. Priority 1
2. Priority 2

## Metrics
- Average commits per day: X
- Average time per task: Y hours
- Blockers encountered: Z
EOF
```

### Monthly Review (Last day of month)

Create a monthly summary in `docs/plans/`:

```bash
cat > docs/plans/$(date +%Y_%m)_MONTHLY_REVIEW.md << 'EOF'
# Monthly Review - Month YYYY-MM

## Executive Summary
- Major projects completed
- Overall progress toward goals
- Key learnings

## Detailed Statistics
- Total commits: X
- Tasks completed: Y
- Hours logged: Z
- Coverage improvement: A%

## Goal Progress
- Goal 1: XX% complete
- Goal 2: XX% complete

## Next Month's Goals
1. Goal 1
2. Goal 2
EOF
```

---

## Tool-Specific Integration

### For Kimi CLI

Kimi provides enhanced daily workflow support:
- **Automatic file creation**: `kimi create-daily-file --type checkpoint`
- **SetTodoList integration**: Automatically update TODO lists
- **Parallel processing**: Work on multiple tasks simultaneously
- **Subagent workflow**: Delegate subtasks to subagents

**Kimi-Specific Commands**:
```bash
# Create morning planning document
kimi daily-workflow --action start-day --date today

# Create checkpoint
kimi daily-workflow --action checkpoint --time now

# End day summary
kimi daily-workflow --action end-day --date today

# Review yesterday
kimi daily-workflow --action review --date yesterday
```

### For Cursor

Cursor provides IDE-integrated daily workflow:
- **File templates**: Automatic daily work file templates
- **Git integration**: Inline commit message generation
- **Timeline view**: Visual timeline of daily work
- **Search across days**: Find content across all daily work files

**Cursor-Specific Features**:
- Use `.cursorrules` to enforce daily workflow
- Custom snippets for daily work templates
- Integrated git checkpoint reminders

### For Claude

Claude excels at daily planning and summarization:
- **Planning assistance**: Help break down daily goals
- **Summary generation**: Generate end-of-day summaries from git history
- **Blocker analysis**: Identify and categorize blockers
- **Time estimation**: Help estimate task durations

### For Gemini

Gemini provides comprehensive workflow guidance:
- **Best practices**: Detailed daily workflow patterns
- **Template generation**: Create custom templates
- **Retrospective analysis**: Analyze patterns across days/weeks
- **Productivity optimization**: Suggest workflow improvements

---

## Automation

### Shell Alias for Quick File Creation

Add to your `~/.bashrc` or `~/.zshrc`:

```bash
# Create daily work file
daily_file() {
  local desc="$1"
  local today=$(date +%Y_%m_%d)
  local date_num=$(date +%Y%m%d)
  local dir="docs/$today"
  
  mkdir -p "$dir"
  
  # Find next sequence number
  local last_file=$(ls -1 "$dir"/*.md 2>/dev/null | tail -1)
  local next_seq="0000"
  
  if [ -n "$last_file" ]; then
    local last_seq=$(basename "$last_file" | cut -d'_' -f2)
    next_seq=$(printf "%04d" $((10#$last_seq + 1)))
  fi
  
  local filename="${dir}/${date_num}_${next_seq}_${desc}.md"
  touch "$filename"
  echo "Created: $filename"
  
  # Open in editor
  ${EDITOR:-code} "$filename"
}

# Usage: daily_file "MORNING_PLANNING"
# Usage: daily_file "CHECKPOINT"
# Usage: daily_file "SESSION_SUMMARY"
```

---

## Troubleshooting

### Problem: Forgot to create daily folder

**Solution**:
```bash
# Create folder retroactively
mkdir -p docs/2025_11_19

# Create files with correct timestamps
touch -t 202511190800 docs/2025_11_19/20251119_0000_MORNING_PLANNING.md
```

### Problem: Wrong sequence number

**Solution**:
```bash
# Rename file with correct sequence
mv docs/2025_11_19/20251119_0005_WRONG.md docs/2025_11_19/20251119_0006_CORRECT.md
```

### Problem: Mixed up days (worked past midnight)

**Solution**:
```bash
# Use the date when you STARTED the session
# If work spanned midnight, use the earlier date
# Document in the file that work spanned midnight
```

---

## Example: Complete Daily Work Folder

```
docs/2025_11_19/
├── 20251119_0000_MORNING_PLANNING.md
├── 20251119_0001_MCP_SERVER_SETUP.md
├── 20251119_0002_API_IMPLEMENTATION.md
├── 20251119_0003_BUG_FIX_INVESTIGATION.md
├── 20251119_0004_CHECKPOINT.md
├── 20251119_0005_SESSION_SUMMARY.md
└── 20251119_0006_TOMORROW_PLANNING.md  (optional)
```

---

## Related Documents

- **File Organization**: `file_organization.md` - Where to place different types of files
- **Naming Conventions**: `naming_conventions.md` - How to name files and folders
- **Git Checkpoint Rules**: `../git/git_checkpoint_rules.md` - When and how to commit
- **Project Structure**: `../project_structure/directory_layout.md` - Overall directory structure

---

**Last Updated**: 2025-11-19  
**Maintained By**: Global AI Rules System  
**Status**: Active  
**Applies To**: All AI assistants (Cursor, Kimi, Claude, Gemini) and all human developers