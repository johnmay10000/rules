# File Locations User Guide - Where to Put Global Rules for Cursor

**Created**: 2025-10-30 00:30  
**Priority**: 🔥 CRITICAL - Users Need This First  
**Purpose**: Define where all rule files should be located for Cursor to find them

---

## Overview

Cursor can load rules from multiple locations with different scopes:
1. **Global Rules** - Apply to ALL projects (stored in rules repository)
2. **Project Rules** - Apply to ONE project (stored in project root)
3. **Precedence** - Project rules override global rules

This guide tells you WHERE to put each file.

---

## Quick Answer

### For Global Rules (All Projects)

**Location**: `/Users/johnmay/projects/rules/` (this repository)

**Files**:
```
/Users/johnmay/projects/rules/
├── CURSOR.md                           # Main global rules
├── CURSOR_FP_PRINCIPLES.md            # FP deep dive
├── CURSOR_WORKFLOW_GUIDE.md           # Workflow guide
├── CURSOR_CLOUD_GCP.md                # GCP platform guide
├── CURSOR_CLOUD_AWS.md                # AWS platform guide
├── python-fp-style-guide.md           # Python FP guide
├── typescript-fp-style-guide.md       # TypeScript FP guide
├── swift-fp-style-guide.md            # Swift FP guide
├── kotlin-fp-style-guide.md           # Kotlin FP guide
└── templates/
    ├── .cursorrules_smart_template    # Smart template (auto-detect)
    ├── ARCHITECTURE_PLAN_TEMPLATE.md  # Architecture plan template
    └── ...
```

**How Cursor Uses Them**: Reference via `@filename` in project `.cursorrules`

### For Project Rules (One Project)

**Location**: Project root (e.g., `/Users/johnmay/projects/my-app/`)

**Files**:
```
/Users/johnmay/projects/my-app/
├── .cursorrules                        # Project-specific rules (references global)
├── ARCHITECTURE_PLAN.md               # Project roadmap
└── docs/
    ├── plans/                          # Sub-plans for features
    │   ├── FEATURE_X_PLAN.md
    │   └── FEATURE_X_TODO.md
    └── YYYY_MM_DD/                     # Daily work logs
        └── YYYYMMDD_HHMM_*.md
```

---

## Detailed File Locations

### 1. Global Rule Documents

**Purpose**: Universal rules that apply to all projects

**Location**: `/Users/johnmay/projects/rules/`

| File | Purpose | When Cursor Uses It |
|------|---------|---------------------|
| `CURSOR.md` | Main global rule set | Referenced via `@CURSOR.md` |
| `CURSOR_FP_PRINCIPLES.md` | FP deep dive | Referenced via `@CURSOR_FP_PRINCIPLES.md` |
| `CURSOR_WORKFLOW_GUIDE.md` | Git, docs, workflow | Referenced via `@CURSOR_WORKFLOW_GUIDE.md` |
| `CURSOR_CLOUD_GCP.md` | GCP-specific rules | Auto-detected OR referenced |
| `CURSOR_CLOUD_AWS.md` | AWS-specific rules | Auto-detected OR referenced |

**How to Reference** (in project `.cursorrules`):
```markdown
# .cursorrules (in project)

@/Users/johnmay/projects/rules/CURSOR.md
@/Users/johnmay/projects/rules/python-fp-style-guide.md
```

---

### 2. Language-Specific Guides

**Purpose**: FP patterns for specific languages

**Location**: `/Users/johnmay/projects/rules/`

| File | Language | When Cursor Uses It |
|------|----------|---------------------|
| `python-fp-style-guide.md` | Python | Auto-detected OR referenced |
| `typescript-fp-style-guide.md` | TypeScript | Auto-detected OR referenced |
| `swift-fp-style-guide.md` | Swift | Auto-detected OR referenced |
| `kotlin-fp-style-guide.md` | Kotlin | Auto-detected OR referenced |

**Auto-Detection** (with smart template):
```markdown
# .cursorrules (in Python project)

## Auto-Detect
IF requirements.txt exists:
  → Apply @/Users/johnmay/projects/rules/python-fp-style-guide.md
```

**Manual Reference**:
```markdown
# .cursorrules (in Python project)

@/Users/johnmay/projects/rules/CURSOR.md
@/Users/johnmay/projects/rules/python-fp-style-guide.md
```

---

### 3. Templates

**Purpose**: Starting point for new projects

**Location**: `/Users/johnmay/projects/rules/templates/`

| Template | Purpose | How to Use |
|----------|---------|------------|
| `.cursorrules_smart_template` | Auto-detecting .cursorrules | Copy to project root as `.cursorrules` |
| `ARCHITECTURE_PLAN_TEMPLATE.md` | Project roadmap | Copy to project root as `ARCHITECTURE_PLAN.md` |
| `SUB_PLAN_TEMPLATE.md` | Feature plan | Copy to `docs/plans/FEATURE_NAME_PLAN.md` |
| `SUB_PLAN_TODO_TEMPLATE.md` | Progress tracking | Copy to `docs/plans/FEATURE_NAME_TODO.md` |

**Usage Example**:
```bash
# Starting new project
cd /Users/johnmay/projects/my-new-app

# Copy smart template
cp /Users/johnmay/projects/rules/templates/.cursorrules_smart_template .cursorrules

# Copy architecture plan template
cp /Users/johnmay/projects/rules/templates/ARCHITECTURE_PLAN_TEMPLATE.md ARCHITECTURE_PLAN.md

# Cursor now auto-detects and applies rules!
```

---

### 4. Project-Specific Files

**Purpose**: Rules and docs for THIS project only

**Location**: Project root (varies per project)

#### Root Level
```
/Users/johnmay/projects/my-app/
├── .cursorrules                # Project rules (references global)
├── ARCHITECTURE_PLAN.md       # Project roadmap (living doc)
└── README.md                  # Project description
```

#### Documentation Hierarchy
```
/Users/johnmay/projects/my-app/docs/
├── plans/                      # Sub-plans (living docs, NOT timestamped)
│   ├── FEATURE_X_PLAN.md      # Feature plan
│   ├── FEATURE_X_TODO.md      # Progress tracking (Cursor updates)
│   ├── DEPLOYMENT_PLAN.md     # Deployment plan
│   └── DEPLOYMENT_TODO.md     # Deployment progress
└── YYYY_MM_DD/                 # Daily snapshots (timestamped, immutable)
    ├── 20251030_0900_FEATURE_IMPLEMENTATION.md
    ├── 20251030_1400_BUG_FIX_SUMMARY.md
    └── 20251030_1700_DAILY_WORK_SUMMARY.md
```

---

## How Cursor Finds Rules

### Discovery Order (Precedence)

1. **Project `.cursorrules`** (highest priority)
   - Location: `/Users/johnmay/projects/my-app/.cursorrules`
   - Scope: This project only
   - Can override global rules

2. **Referenced Global Rules** (via `@path`)
   - Location: `/Users/johnmay/projects/rules/*.md`
   - Scope: All projects that reference them
   - Loaded via `@` syntax

3. **Auto-Detected Rules** (smart template)
   - Triggered by: File existence (package.json, requirements.txt, etc.)
   - Applied: Automatically by smart .cursorrules template
   - Scope: All projects using smart template

### Example: Python + GCP Project

**Project Structure**:
```
/Users/johnmay/projects/my-python-gcp-app/
├── .cursorrules              # Uses smart template
├── requirements.txt          # ← Triggers Python detection
├── gc/                       # ← Triggers GCP detection
│   └── my_function/
└── workflows/                # ← Triggers Cloud Workflows detection
```

**What Cursor Loads** (automatically):
1. `.cursorrules` (project-specific)
2. `@CURSOR.md` (universal rules - via reference)
3. `@python-fp-style-guide.md` (auto-detected from requirements.txt)
4. `@CURSOR_CLOUD_GCP.md` (auto-detected from gc/ folder)

**User Does**: Nothing! Just has `.cursorrules` with smart template.

---

## Setup Instructions

### One-Time Global Setup

**Step 1**: Clone or create rules repository
```bash
# If not exists
mkdir -p /Users/johnmay/projects/rules
cd /Users/johnmay/projects/rules

# Pull latest rules (once implemented)
git clone <rules-repo-url> .
```

**Step 2**: Verify global files exist
```bash
ls -la /Users/johnmay/projects/rules/

# Should see:
# CURSOR.md
# python-fp-style-guide.md
# typescript-fp-style-guide.md
# etc.
```

**Done!** Global rules are now available to all projects.

---

### Per-Project Setup

**Step 1**: Copy smart .cursorrules template
```bash
cd /Users/johnmay/projects/my-new-project

cp /Users/johnmay/projects/rules/templates/.cursorrules_smart_template .cursorrules
```

**Step 2**: Copy ARCHITECTURE_PLAN template (optional but recommended)
```bash
cp /Users/johnmay/projects/rules/templates/ARCHITECTURE_PLAN_TEMPLATE.md ARCHITECTURE_PLAN.md
```

**Step 3**: Create docs structure
```bash
mkdir -p docs/plans
mkdir -p docs/$(date +%Y_%m_%d)
```

**Step 4**: Start coding!
```bash
# Cursor automatically:
# - Detects your tech stack
# - Loads appropriate global rules
# - Applies language-specific guidelines
# - Enforces mandatory rules
```

**That's it!** No manual configuration needed.

---

## Reference Syntax in .cursorrules

### Absolute Path (Recommended)
```markdown
@/Users/johnmay/projects/rules/CURSOR.md
@/Users/johnmay/projects/rules/python-fp-style-guide.md
```

### Relative Path (if rules in parent)
```markdown
@../rules/CURSOR.md
@../rules/python-fp-style-guide.md
```

### Smart Template (Auto-Detection)
```markdown
# In .cursorrules

## Auto-Detection Logic
IF requirements.txt exists:
  @/Users/johnmay/projects/rules/python-fp-style-guide.md

IF package.json exists:
  @/Users/johnmay/projects/rules/typescript-fp-style-guide.md

IF gc/ folder exists:
  @/Users/johnmay/projects/rules/CURSOR_CLOUD_GCP.md
```

---

## File Locations Summary Table

| File Type | Location | Scope | Example Path |
|-----------|----------|-------|--------------|
| **Global Rules** | Rules repo | All projects | `/Users/johnmay/projects/rules/CURSOR.md` |
| **Language Guides** | Rules repo | All projects using language | `/Users/johnmay/projects/rules/python-fp-style-guide.md` |
| **Platform Guides** | Rules repo | All projects on platform | `/Users/johnmay/projects/rules/CURSOR_CLOUD_GCP.md` |
| **Templates** | Rules repo | Starting point | `/Users/johnmay/projects/rules/templates/*.md` |
| **Project .cursorrules** | Project root | This project | `/Users/johnmay/projects/my-app/.cursorrules` |
| **ARCHITECTURE_PLAN** | Project root | This project | `/Users/johnmay/projects/my-app/ARCHITECTURE_PLAN.md` |
| **Sub-plans** | Project docs/plans/ | Features | `/Users/johnmay/projects/my-app/docs/plans/*.md` |
| **Daily logs** | Project docs/DATE/ | Daily work | `/Users/johnmay/projects/my-app/docs/2025_10_30/*.md` |

---

## Common Scenarios

### Scenario 1: New Python Project

**Setup**:
```bash
cd ~/projects/my-python-app
cp ~/projects/rules/templates/.cursorrules_smart_template .cursorrules
touch requirements.txt
```

**What Cursor Loads**:
- Universal rules (CURSOR.md)
- Python FP guide (python-fp-style-guide.md)
- Mandatory rules (git, docs, testing, file size)

**User Action Required**: None! Auto-detected.

---

### Scenario 2: Existing Project (Add Rules)

**Setup**:
```bash
cd ~/projects/existing-app
cp ~/projects/rules/templates/.cursorrules_smart_template .cursorrules
```

**What Changes**:
- Cursor now enforces rules
- Old code: monitor and migrate incrementally
- New code: must follow rules immediately

**Migration Strategy**: Per Decision 4 (incremental with testing)

---

### Scenario 3: Multi-Language Project

**Setup**:
```bash
cd ~/projects/polyglot-app
cp ~/projects/rules/templates/.cursorrules_smart_template .cursorrules

# Has both Python and TypeScript
ls
# requirements.txt  ← Python detected
# package.json      ← TypeScript detected
```

**What Cursor Loads**:
- Universal rules (CURSOR.md)
- Python FP guide (for .py files)
- TypeScript FP guide (for .ts files)
- Applies appropriate rules per file type

---

### Scenario 4: Override Global Rules

**Setup** (in project `.cursorrules`):
```markdown
@/Users/johnmay/projects/rules/CURSOR.md

## Project-Specific Overrides
- File size limit: 400 lines (instead of 250-300)
  Reason: Legacy codebase, migrating gradually
  
- Testing: Integration tests only (for now)
  Reason: Adding unit tests incrementally
```

**Result**: Project overrides take precedence, but other rules still apply.

---

## Troubleshooting

### Problem: Cursor Not Loading Rules

**Check 1**: Verify .cursorrules exists in project root
```bash
ls -la .cursorrules
```

**Check 2**: Verify global files exist
```bash
ls -la /Users/johnmay/projects/rules/CURSOR.md
```

**Check 3**: Verify @ syntax is correct
```markdown
# CORRECT
@/Users/johnmay/projects/rules/CURSOR.md

# WRONG
/Users/johnmay/projects/rules/CURSOR.md  # Missing @
```

**Check 4**: Check Cursor settings
- Open Cursor settings
- Search for "rules"
- Verify .cursorrules is enabled

---

### Problem: Auto-Detection Not Working

**Check 1**: Verify smart template is being used
```bash
head -5 .cursorrules
# Should see "Auto-Detection" section
```

**Check 2**: Verify detection files exist
```bash
# Python
ls requirements.txt

# TypeScript
ls package.json

# GCP
ls -d gc/
```

**Check 3**: Check detection logic in .cursorrules
```markdown
IF requirements.txt exists:
  @/Users/johnmay/projects/rules/python-fp-style-guide.md
```

---

## Migration from Old Setup

### If You Had Custom Skills

**Old Location** (Claude.ai custom skills):
```
/mnt/skills/user/python-fp/SKILL.md
/mnt/skills/user/typescript-fp/SKILL.md
```

**New Location** (Cursor global rules):
```
/Users/johnmay/projects/rules/python-fp-style-guide.md
/Users/johnmay/projects/rules/typescript-fp-style-guide.md
```

**Migration Steps**:
1. Copy content from SKILL.md files to new location
2. Update `.cursorrules` to reference new location
3. Remove old custom skills references

---

### If You Had Project Docs

**Old Location** (various):
```
/project/CLAUDE.md
/project/CODE_STYLE.md
/project/docs/README.md
```

**New Location** (standardized):
```
/project/.cursorrules                # References global rules
/project/ARCHITECTURE_PLAN.md        # Project roadmap
/project/docs/plans/FEATURE_PLAN.md  # Feature plans
/project/docs/2025_10_30/*.md        # Daily work logs
```

**Migration Steps**:
1. Create `.cursorrules` (copy smart template)
2. Create `ARCHITECTURE_PLAN.md` (copy template)
3. Organize existing docs into new structure
4. Archive old docs or convert to new format

---

## Best Practices

### 1. Keep Global Rules in Sync
```bash
# Update global rules repository periodically
cd /Users/johnmay/projects/rules
git pull
```

### 2. Don't Duplicate Rules
```markdown
# BAD: Copy rules into project
# .cursorrules
[All rules copied here...]

# GOOD: Reference global rules
# .cursorrules
@/Users/johnmay/projects/rules/CURSOR.md
```

### 3. Use Smart Template
```bash
# Always start projects with smart template
cp /Users/johnmay/projects/rules/templates/.cursorrules_smart_template .cursorrules
```

### 4. Document Project Overrides
```markdown
# .cursorrules

@/Users/johnmay/projects/rules/CURSOR.md

## Project Overrides
- [Override 1]: Reason and justification
- [Override 2]: Reason and justification
```

---

## Summary

### Global Files Location
```
/Users/johnmay/projects/rules/
├── CURSOR.md                      ← Main rules
├── python-fp-style-guide.md       ← Language-specific
├── CURSOR_CLOUD_GCP.md            ← Platform-specific
└── templates/                     ← Starting templates
```

### Project Files Location
```
/Users/johnmay/projects/my-app/
├── .cursorrules                   ← References global
├── ARCHITECTURE_PLAN.md           ← Project roadmap
└── docs/
    ├── plans/                     ← Feature plans
    └── YYYY_MM_DD/                ← Daily logs
```

### How to Reference
```markdown
# In project .cursorrules
@/Users/johnmay/projects/rules/CURSOR.md
@/Users/johnmay/projects/rules/python-fp-style-guide.md
```

### Quick Start
```bash
# New project
cd ~/projects/new-app
cp ~/projects/rules/templates/.cursorrules_smart_template .cursorrules
# Done! Cursor auto-detects everything.
```

---

**Status**: User guide specification complete, ready to add to plan

**Next**: Add this guide to deliverables in global rule set plan

