# File Locations User Guide - Portable Version

**Created**: 2025-10-31 (Updated from 2025-10-30)  
**Priority**: 🔥 CRITICAL - Essential for Setup  
**Status**: ✅ PORTABLE - Works on Any Machine  
**Purpose**: Define where rule files should be located for Cursor to find them

---

## 🌍 PORTABILITY FIRST

**Critical Update**: This guide now uses PORTABLE paths that work on any machine.

### Two Portable Approaches

1. **Environment Variable** (Recommended)
   - Set `$CURSOR_RULES_PATH` once per machine
   - Reference: `@${CURSOR_RULES_PATH}/CURSOR.md`
   - Works on: macOS, Linux, Windows

2. **Git Submodule** (Self-Contained)
   - Add rules as `.cursor-rules/` submodule
   - Reference: `@.cursor-rules/CURSOR.md`
   - Works on: Any machine, no setup needed

**Setup Instructions**: See [SETUP_GUIDE.md](../../SETUP_GUIDE.md)

---

## Table of Contents

- [Quick Answer](#quick-answer)
- [Portability Approaches](#portability-approaches)
- [Global Rules Location](#global-rules-location)
- [Project Rules Location](#project-rules-location)
- [Reference Syntax](#reference-syntax)
- [Auto-Detection](#auto-detection)
- [File Organization](#file-organization)
- [Precedence Rules](#precedence-rules)
- [Examples](#examples)
- [Troubleshooting](#troubleshooting)

---

## Quick Answer

### For Global Rules (All Projects)

#### Method 1: Environment Variable (Recommended)

**Location**: Wherever you cloned the rules repo  
**Setup**: Set `$CURSOR_RULES_PATH` environment variable  
**Reference**: `@${CURSOR_RULES_PATH}/filename.md`

**Example**:
```bash
# Your machine
export CURSOR_RULES_PATH="$HOME/projects/rules"

# In project .cursorrules
@${CURSOR_RULES_PATH}/CURSOR.md
```

#### Method 2: Git Submodule (Self-Contained)

**Location**: `.cursor-rules/` inside each project  
**Setup**: `git submodule add <rules-repo-url> .cursor-rules`  
**Reference**: `@.cursor-rules/filename.md`

**Example**:
```bash
# Add to project
git submodule add https://github.com/yourusername/rules.git .cursor-rules

# In project .cursorrules
@.cursor-rules/CURSOR.md
```

---

### For Project Rules (One Project)

**Location**: Project root and `docs/` folder  
**Setup**: None (just create files)  
**Reference**: Direct in `.cursorrules` or separate files

**Example**:
```
your-project/
├── .cursorrules              # References global + project rules
├── ARCHITECTURE_PLAN.md      # Project architecture
└── docs/
    ├── plans/                # Feature plans
    └── 2025_10_31/           # Daily work
```

---

## Portability Approaches

### Approach 1: Environment Variable ($CURSOR_RULES_PATH)

**Best For**: Personal projects, multiple projects sharing rules

**Setup** (one-time per machine):
```bash
# macOS/Linux
echo 'export CURSOR_RULES_PATH="$HOME/projects/rules"' >> ~/.zshrc
source ~/.zshrc

# Windows (PowerShell)
[System.Environment]::SetEnvironmentVariable('CURSOR_RULES_PATH', "$HOME\projects\rules", 'User')
```

**Reference in `.cursorrules`**:
```markdown
# Environment variable approach (PORTABLE)
@${CURSOR_RULES_PATH}/CURSOR.md
@${CURSOR_RULES_PATH}/python-fp-style-guide.md
@${CURSOR_RULES_PATH}/CURSOR_CLOUD_GCP.md
```

**Pros**:
- ✅ Single source of truth
- ✅ Easy to update (git pull once, all projects updated)
- ✅ You choose where to put rules
- ✅ Works on any machine after setup

**Cons**:
- ⚠️ Requires one-time machine setup
- ⚠️ Each machine needs environment variable set

---

### Approach 2: Git Submodule (.cursor-rules/)

**Best For**: Shared projects, team collaboration, distribution

**Setup** (one-time per project):
```bash
cd your-project
git submodule add https://github.com/yourusername/rules.git .cursor-rules
git submodule update --init --recursive
```

**Reference in `.cursorrules`**:
```markdown
# Git submodule approach (SELF-CONTAINED)
@.cursor-rules/CURSOR.md
@.cursor-rules/python-fp-style-guide.md
@.cursor-rules/CURSOR_CLOUD_GCP.md
```

**Pros**:
- ✅ Self-contained (rules inside project)
- ✅ No machine setup needed
- ✅ Works immediately after git clone
- ✅ Version-controlled per project

**Cons**:
- ⚠️ Rules duplicated per project
- ⚠️ Must update submodule to get latest rules
- ⚠️ Slightly more complex git workflow

---

## Global Rules Location

### File Structure (Rules Repository)

```
rules/                                  # Rules repository
├── CURSOR.md                          # Core mandatory rules ⭐
├── CURSOR_FP_PRINCIPLES.md            # FP deep dive
├── CURSOR_WORKFLOW_GUIDE.md           # Workflow guide
├── CURSOR_CLOUD_GCP.md                # GCP platform guide
├── CURSOR_CLOUD_AWS.md                # AWS platform guide
├── SETUP_GUIDE.md                     # Machine setup ⭐
├── python-fp-style-guide.md           # Python FP guide
├── typescript-fp-style-guide.md       # TypeScript FP guide
├── kotlin-fp-style-guide.md           # Kotlin FP guide
├── swift-fp-style-guide.md            # Swift FP guide
├── rust-fp-style-guide.md             # Rust FP guide ⭐ NEW
├── templates/
│   ├── .cursorrules_smart_template_envvar      # Env var template ⭐
│   ├── .cursorrules_smart_template_submodule   # Submodule template ⭐
│   ├── ARCHITECTURE_PLAN_TEMPLATE.md
│   ├── SUB_PLAN_TEMPLATE.md
│   ├── SUB_PLAN_TODO_TEMPLATE.md
│   └── ...
├── examples/
│   ├── python_cursorrules_example
│   ├── typescript_cursorrules_example
│   └── ...
└── docs/
    ├── plans/
    └── YYYY_MM_DD/
```

### Where YOU Put It

**Option A**: Environment Variable Approach
```bash
# You choose the location!
~/projects/rules/                # Option 1
~/.cursor-rules-global/          # Option 2
~/Documents/cursor-rules/        # Option 3
```

**Option B**: Submodule Approach
```bash
# Inside each project
your-project/.cursor-rules/      # Submodule location
```

---

## Project Rules Location

### File Structure (Your Project)

```
your-project/                           # Your project root
├── .cursorrules                        # ⭐ Project config (references global rules)
├── .cursor-rules/                      # (Optional) Git submodule with rules
├── ARCHITECTURE_PLAN.md                # ⭐ Project architecture (Tier 1)
├── docs/
│   ├── plans/                          # ⭐ Feature plans (Tier 2)
│   │   ├── FEATURE_X_PLAN.md
│   │   └── FEATURE_X_TODO.md
│   └── 2025_10_31/                     # ⭐ Daily work (Tier 3)
│       └── 20251031_0900_*.md
├── src/                                # Your code
├── tests/                              # Your tests
└── README.md
```

### Three-Tier Documentation

**Tier 1: Strategic** (`ARCHITECTURE_PLAN.md`)
- Location: Project root
- Purpose: High-level architecture
- Updated: Rarely (major changes only)

**Tier 2: Tactical** (`docs/plans/*.md`)
- Location: `docs/plans/`
- Purpose: Feature/area plans with paired TODOs
- Updated: Regularly as features progress

**Tier 3: Execution** (`docs/YYYY_MM_DD/*.md`)
- Location: `docs/2025_10_31/` (dated folders)
- Purpose: Daily work, decisions, summaries
- Updated: Daily/frequently

---

## Reference Syntax

### In `.cursorrules` File

#### Environment Variable References

```markdown
# .cursorrules (portable via environment variable)

## Global Rules
@${CURSOR_RULES_PATH}/CURSOR.md
@${CURSOR_RULES_PATH}/CURSOR_WORKFLOW_GUIDE.md

## Language-Specific
@${CURSOR_RULES_PATH}/python-fp-style-guide.md

## Platform-Specific
@${CURSOR_RULES_PATH}/CURSOR_CLOUD_GCP.md
```

#### Submodule References

```markdown
# .cursorrules (self-contained via submodule)

## Global Rules
@.cursor-rules/CURSOR.md
@.cursor-rules/CURSOR_WORKFLOW_GUIDE.md

## Language-Specific
@.cursor-rules/python-fp-style-guide.md

## Platform-Specific
@.cursor-rules/CURSOR_CLOUD_GCP.md
```

#### Project-Specific Rules (Inline)

```markdown
# .cursorrules

## Global Rules
@${CURSOR_RULES_PATH}/CURSOR.md

## Project-Specific Overrides
# These override or extend global rules

### Custom File Size Limit
- analytics/ module: Max 500 lines (exception to 250-300 rule)

### Custom Data Library
- Use pandas for legacy compatibility (exception to Polars-only rule)
```

---

## Auto-Detection

### How Smart Templates Work

The smart templates automatically detect your project type and suggest rules to load.

**Detection Logic**:

1. **Python Project**: If `requirements.txt`, `pyproject.toml`, or `*.py` files exist
   → Load `python-fp-style-guide.md`

2. **TypeScript Project**: If `package.json` with `"typescript"` dependency exists
   → Load `typescript-fp-style-guide.md`

3. **Kotlin Project**: If `build.gradle.kts` or `*.kt` files exist
   → Load `kotlin-fp-style-guide.md`

4. **Swift Project**: If `Package.swift` or `*.swift` files exist
   → Load `swift-fp-style-guide.md`

5. **Rust Project**: If `Cargo.toml` or `*.rs` files exist
   → Load `rust-fp-style-guide.md`

6. **GCP Project**: If `gc/` folder or `workflows/` folder exists
   → Load `CURSOR_CLOUD_GCP.md`

4. **AWS Project**: If `lambda/` folder or `serverless.yml` exists
   → Load `CURSOR_CLOUD_AWS.md`

### Using Smart Templates

**Environment Variable Version**:
```bash
cp ${CURSOR_RULES_PATH}/templates/.cursorrules_smart_template_envvar .cursorrules
```

**Submodule Version**:
```bash
cp .cursor-rules/templates/.cursorrules_smart_template_submodule .cursorrules
```

Then uncomment the sections for your tech stack.

---

## File Organization

### Global Rules Repository

**Core Documents** (in root):
- `CURSOR.md` - Mandatory for all projects
- `SETUP_GUIDE.md` - Machine setup instructions
- `*-fp-style-guide.md` - Language-specific guides

**Templates** (`templates/`):
- `.cursorrules_smart_template_*` - Project templates
- `*_TEMPLATE.md` - Document templates

**Examples** (`examples/`):
- Real-world usage examples
- Platform-specific examples

**Documentation** (`docs/`):
- `plans/` - Strategic plans for rules repo itself
- `YYYY_MM_DD/` - Work logs for rules repo development

---

### Project Organization

**Root Level**:
- `.cursorrules` - Cursor configuration (references global rules)
- `ARCHITECTURE_PLAN.md` - Project architecture (Tier 1)
- `README.md` - Project overview

**docs/plans/** (Tier 2 - Tactical):
- `FEATURE_X_PLAN.md` - Feature plan
- `FEATURE_X_TODO.md` - Paired TODO list (updated by Cursor)

**docs/YYYY_MM_DD/** (Tier 3 - Execution):
- `YYYYMMDD_HHMM_DESCRIPTIVE_NAME.md` - Timestamped work docs
- Daily work logs, decision logs, session summaries

**src/** and **tests/**:
- Your actual code and tests

---

## Precedence Rules

### Override Hierarchy

1. **Highest**: Inline project-specific rules (in `.cursorrules`)
2. **Medium**: Project-level rule files (in project root)
3. **Lowest**: Global rules (from rules repository)

### Example

```markdown
# .cursorrules

## Global Rules (baseline)
@${CURSOR_RULES_PATH}/CURSOR.md
# Says: "Max file size 250-300 lines"

## Project-Specific (overrides global)
### Custom File Size
- analytics/ module: Max 500 lines (EXCEPTION TO GLOBAL RULE)
# This project-specific rule overrides the global 250-300 line limit
# but ONLY for the analytics/ module
```

**Result**:
- Most code: 250-300 lines (global rule)
- `analytics/*`: 500 lines (project override)

---

## Examples

### Example 1: Python + GCP Project (Environment Variable)

**Setup**:
```bash
# One-time machine setup
export CURSOR_RULES_PATH="$HOME/projects/rules"
```

**Project `.cursorrules`**:
```markdown
# .cursorrules

## Global Rules
@${CURSOR_RULES_PATH}/CURSOR.md
@${CURSOR_RULES_PATH}/CURSOR_WORKFLOW_GUIDE.md

## Python FP
@${CURSOR_RULES_PATH}/python-fp-style-guide.md

## GCP Cloud Functions
@${CURSOR_RULES_PATH}/CURSOR_CLOUD_GCP.md

## Project-Specific
### Data Processing
- Use Polars for all data manipulation
- Minimum 85% test coverage for business logic
```

---

### Example 2: TypeScript + AWS Project (Submodule)

**Setup**:
```bash
# One-time project setup
git submodule add https://github.com/yourusername/rules.git .cursor-rules
```

**Project `.cursorrules`**:
```markdown
# .cursorrules

## Global Rules
@.cursor-rules/CURSOR.md
@.cursor-rules/CURSOR_WORKFLOW_GUIDE.md

## TypeScript FP
@.cursor-rules/typescript-fp-style-guide.md

## AWS Lambda
@.cursor-rules/CURSOR_CLOUD_AWS.md

## Project-Specific
### API Requirements
- All endpoints must have OpenAPI spec
- Response time < 200ms for 95th percentile
```

---

### Example 3: Polyglot Project (Python + TypeScript)

**Project `.cursorrules`**:
```markdown
# .cursorrules

## Global Rules
@${CURSOR_RULES_PATH}/CURSOR.md

## Backend (Python)
@${CURSOR_RULES_PATH}/python-fp-style-guide.md

## Frontend (TypeScript)
@${CURSOR_RULES_PATH}/typescript-fp-style-guide.md

## Project-Specific
### Monorepo Structure
- backend/ - Python FastAPI service
- frontend/ - React + TypeScript
- shared/ - Shared types and validation
```

---

## Troubleshooting

### Rules Not Loading

**Symptom**: Cursor doesn't apply rules

**Check**:
1. **Environment variable set?**
   ```bash
   echo $CURSOR_RULES_PATH
   # Should output path, not empty
   ```

2. **Files exist?**
   ```bash
   ls ${CURSOR_RULES_PATH}/CURSOR.md
   # Should list file, not "No such file"
   ```

3. **Correct syntax?**
   ```markdown
   # CORRECT
   @${CURSOR_RULES_PATH}/CURSOR.md
   
   # WRONG (space after @)
   @ ${CURSOR_RULES_PATH}/CURSOR.md
   ```

4. **Restart Cursor**
   - Rules load on startup
   - Restart after changing `.cursorrules`

---

### Path Not Found

**Symptom**: "File not found" errors

**Solutions**:

**Environment Variable**:
```bash
# Check variable
echo $CURSOR_RULES_PATH

# If empty, set it
export CURSOR_RULES_PATH="$HOME/projects/rules"

# Add to shell config
echo 'export CURSOR_RULES_PATH="$HOME/projects/rules"' >> ~/.zshrc
source ~/.zshrc
```

**Submodule**:
```bash
# Check submodule exists
ls .cursor-rules/

# If empty, initialize
git submodule update --init --recursive
```

---

### Wrong Machine/Path

**Symptom**: Hard-coded path doesn't exist on new machine

**Problem**:
```markdown
# BAD (machine-specific)
@/Users/johnmay/projects/rules/CURSOR.md
```

**Solution**:
```markdown
# GOOD (portable)
@${CURSOR_RULES_PATH}/CURSOR.md
```

Then set `$CURSOR_RULES_PATH` on each machine to the appropriate local path.

---

## Platform-Specific Notes

### macOS

**Default Shell**: Zsh (Catalina+)  
**Config File**: `~/.zshrc`  
**Setup**:
```bash
echo 'export CURSOR_RULES_PATH="$HOME/projects/rules"' >> ~/.zshrc
source ~/.zshrc
```

---

### Linux

**Default Shell**: Bash (most distros)  
**Config File**: `~/.bashrc`  
**Setup**:
```bash
echo 'export CURSOR_RULES_PATH="$HOME/projects/rules"' >> ~/.bashrc
source ~/.bashrc
```

---

### Windows

**Recommended**: PowerShell  
**Setup**:
```powershell
[System.Environment]::SetEnvironmentVariable(
    'CURSOR_RULES_PATH',
    "$HOME\projects\rules",
    'User'
)
```

**Note**: Use forward slashes in paths for cross-platform compatibility:
```markdown
# GOOD (works everywhere)
@${CURSOR_RULES_PATH}/CURSOR.md

# WORKS BUT LESS PORTABLE
@${CURSOR_RULES_PATH}\CURSOR.md
```

---

## Summary

### Global Rules

**Location**: 
- Env Var: Wherever you cloned rules repo
- Submodule: `.cursor-rules/` in each project

**Reference**:
- Env Var: `@${CURSOR_RULES_PATH}/filename.md`
- Submodule: `@.cursor-rules/filename.md`

**Files**:
- `CURSOR.md` (core rules)
- `python-fp-style-guide.md`, `typescript-fp-style-guide.md`, `kotlin-fp-style-guide.md`, `swift-fp-style-guide.md`, `rust-fp-style-guide.md` (5 language guides)
- `CURSOR_CLOUD_GCP.md`, `CURSOR_CLOUD_AWS.md` (platform guides)

---

### Project Rules

**Location**: Project root and `docs/`

**Files**:
- `.cursorrules` (config)
- `ARCHITECTURE_PLAN.md` (Tier 1)
- `docs/plans/*.md` (Tier 2)
- `docs/YYYY_MM_DD/*.md` (Tier 3)

---

### Key Takeaways

✅ **Use Environment Variables or Submodules** - Not hard-coded paths  
✅ **Set $CURSOR_RULES_PATH** - Once per machine (env var approach)  
✅ **Use Smart Templates** - Auto-detect project type  
✅ **Follow 3-Tier Docs** - Strategic, Tactical, Execution  
✅ **Project Overrides Global** - Customize as needed  

---

**Portability**: ✅ Works on macOS, Linux, Windows  
**Determinism**: ✅ Same output across machines  
**Setup Time**: 5 minutes (env var) or 2 minutes (submodule)  

**Ready to use!** 🎉

