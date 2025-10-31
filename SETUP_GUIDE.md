# Global Rules Setup Guide

**One-Time Machine Setup** | **5 Minutes** | **All Platforms**

This guide shows you how to set up your machine to use the global Cursor rules repository across all your projects.

---

## Quick Start (TL;DR)

### Option 1: Environment Variable (Recommended)

```bash
# macOS/Linux - Add to ~/.zshrc or ~/.bashrc
export CURSOR_RULES_PATH="$HOME/projects/rules"
source ~/.zshrc  # or source ~/.bashrc

# Verify
echo $CURSOR_RULES_PATH
```

### Option 2: Git Submodule (Self-Contained)

```bash
# In each project
git submodule add <rules-repo-url> .cursor-rules
```

**Done!** Skip to [Using in Projects](#using-in-projects)

---

## Table of Contents

- [Overview](#overview)
- [Setup Methods](#setup-methods)
  - [Method 1: Environment Variable](#method-1-environment-variable-recommended)
  - [Method 2: Git Submodule](#method-2-git-submodule-self-contained)
  - [Method 3: Symbolic Link](#method-3-symbolic-link-fallback)
- [Using in Projects](#using-in-projects)
- [Verification](#verification)
- [Updating Rules](#updating-rules)
- [Troubleshooting](#troubleshooting)
- [Platform-Specific Notes](#platform-specific-notes)

---

## Overview

### Why This Setup?

The global rules repository contains reusable guidelines for:
- Functional programming principles
- Code style and architecture
- Testing requirements
- Git workflow and documentation
- Platform-specific rules (GCP, AWS)

**Problem**: Hard-coded paths won't work across machines  
**Solution**: Portable references using environment variables or git submodules

### Which Method Should I Use?

| Method | Best For | Pros | Cons |
|--------|----------|------|------|
| **Environment Variable** | Multiple personal projects | Single source of truth, easy updates | One-time setup per machine |
| **Git Submodule** | Shared/team projects | Self-contained, no setup needed | Duplication, manual updates |
| **Symbolic Link** | Specific use cases | Standard location | Platform-dependent |

**Recommendation**: Use **Environment Variable** for personal work, **Git Submodule** for shared projects.

---

## Setup Methods

## Method 1: Environment Variable (Recommended)

### Step 1: Clone the Rules Repository

Choose where to store the rules (your preference):

```bash
# Option A: Projects folder
git clone https://github.com/yourusername/rules.git ~/projects/rules

# Option B: Hidden folder
git clone https://github.com/yourusername/rules.git ~/.cursor-rules-global

# Option C: Documents folder
git clone https://github.com/yourusername/rules.git ~/Documents/cursor-rules
```

### Step 2: Set Environment Variable

#### macOS/Linux (Zsh)

```bash
# Add to ~/.zshrc
echo 'export CURSOR_RULES_PATH="$HOME/projects/rules"' >> ~/.zshrc

# Reload shell
source ~/.zshrc
```

#### macOS/Linux (Bash)

```bash
# Add to ~/.bashrc
echo 'export CURSOR_RULES_PATH="$HOME/projects/rules"' >> ~/.bashrc

# Reload shell
source ~/.bashrc
```

#### Windows (PowerShell)

```powershell
# Option A: Session variable (temporary)
$env:CURSOR_RULES_PATH = "$HOME\projects\rules"

# Option B: Permanent variable (recommended)
[System.Environment]::SetEnvironmentVariable(
    'CURSOR_RULES_PATH',
    "$HOME\projects\rules",
    'User'
)

# Reload PowerShell to verify
```

#### Windows (Command Prompt)

```cmd
# Set permanent variable
setx CURSOR_RULES_PATH "%USERPROFILE%\projects\rules"

# Restart Command Prompt to verify
```

### Step 3: Verify Setup

```bash
# Check environment variable
echo $CURSOR_RULES_PATH
# Output: /Users/yourname/projects/rules (or your path)

# Check files exist
ls $CURSOR_RULES_PATH/CURSOR.md
# Output: /Users/yourname/projects/rules/CURSOR.md

# Check in new shell (important!)
# Open a new terminal window and run:
echo $CURSOR_RULES_PATH
# Should still show your path
```

### Step 4: Use in Projects

Create `.cursorrules` in your project:

```markdown
# .cursorrules

## Global Rules
@${CURSOR_RULES_PATH}/CURSOR.md

## Language-Specific Rules (Auto-detected)
# Python
@${CURSOR_RULES_PATH}/python-fp-style-guide.md

# TypeScript
@${CURSOR_RULES_PATH}/typescript-fp-style-guide.md

## Platform-Specific Rules (Auto-detected)
# GCP
@${CURSOR_RULES_PATH}/CURSOR_CLOUD_GCP.md

# AWS
@${CURSOR_RULES_PATH}/CURSOR_CLOUD_AWS.md
```

**Done!** This `.cursorrules` file now works on ANY machine where `$CURSOR_RULES_PATH` is set.

---

## Method 2: Git Submodule (Self-Contained)

### Step 1: Add Rules as Submodule

In your project:

```bash
cd /path/to/your/project

# Add rules as submodule
git submodule add https://github.com/yourusername/rules.git .cursor-rules

# Initialize and update
git submodule update --init --recursive
```

### Step 2: Verify Structure

```bash
# Check structure
ls -la
# Should see:
# .cursor-rules/  (submodule directory)

# Check rules are present
ls .cursor-rules/
# Should see: CURSOR.md, python-fp-style-guide.md, etc.
```

### Step 3: Use in Project

Create `.cursorrules` in your project:

```markdown
# .cursorrules

## Global Rules
@.cursor-rules/CURSOR.md

## Language-Specific Rules
# Python
@.cursor-rules/python-fp-style-guide.md

# TypeScript
@.cursor-rules/typescript-fp-style-guide.md

## Platform-Specific Rules
# GCP
@.cursor-rules/CURSOR_CLOUD_GCP.md

# AWS
@.cursor-rules/CURSOR_CLOUD_AWS.md
```

### Step 4: Commit Submodule

```bash
git add .gitmodules .cursor-rules .cursorrules
git commit -m "Add Cursor rules as submodule"
```

**Done!** Anyone who clones your project gets the rules automatically.

### For Collaborators

When cloning a project with submodules:

```bash
# Option A: Clone with submodules
git clone --recurse-submodules <your-repo-url>

# Option B: Clone then init submodules
git clone <your-repo-url>
cd <your-repo>
git submodule update --init --recursive
```

---

## Method 3: Symbolic Link (Fallback)

### Step 1: Clone Rules Repository

```bash
# Clone to your preferred location
git clone https://github.com/yourusername/rules.git ~/projects/rules
```

### Step 2: Create Symbolic Link

#### macOS/Linux

```bash
# Create standard location link
ln -s ~/projects/rules ~/.cursor-rules
```

#### Windows (PowerShell - Run as Administrator)

```powershell
# Create symbolic link
New-Item -ItemType SymbolicLink -Path "$HOME\.cursor-rules" -Target "$HOME\projects\rules"
```

### Step 3: Verify Link

```bash
# Check link exists
ls -l ~/.cursor-rules
# Output: ~/.cursor-rules -> /Users/yourname/projects/rules

# Check files accessible
ls ~/.cursor-rules/CURSOR.md
```

### Step 4: Use in Projects

```markdown
# .cursorrules
@~/.cursor-rules/CURSOR.md
@~/.cursor-rules/python-fp-style-guide.md
```

---

## Using in Projects

### Smart Template (Auto-Detection)

Copy the smart template to your project:

```bash
# Environment Variable version
cp ${CURSOR_RULES_PATH}/templates/.cursorrules_smart_template_envvar .cursorrules

# Submodule version
cp .cursor-rules/templates/.cursorrules_smart_template_submodule .cursorrules
```

The smart template automatically detects your project type and loads appropriate rules.

### Manual Selection

Create `.cursorrules` and reference only what you need:

```markdown
# .cursorrules

## Core Rules (All Projects)
@${CURSOR_RULES_PATH}/CURSOR.md

## Python Project
@${CURSOR_RULES_PATH}/python-fp-style-guide.md

## GCP Project
@${CURSOR_RULES_PATH}/CURSOR_CLOUD_GCP.md
```

### Project-Specific Rules

Add project-specific rules after global rules:

```markdown
# .cursorrules

## Global Rules
@${CURSOR_RULES_PATH}/CURSOR.md
@${CURSOR_RULES_PATH}/python-fp-style-guide.md

## Project-Specific Rules
# These override or extend global rules

### Custom Requirements
- Use pandas for data processing (exception to Polars rule)
- Max file size: 500 lines (exception to 250-300 rule)
```

---

## Verification

### Test Environment Variable

```bash
# Check variable is set
echo $CURSOR_RULES_PATH

# Expected output:
# /Users/yourname/projects/rules (or your path)

# If empty, the variable is not set
# Go back to Step 2 and ensure you sourced your shell config
```

### Test File Access

```bash
# Test file exists
cat ${CURSOR_RULES_PATH}/CURSOR.md

# Should display file contents
# If "No such file or directory", check:
# 1. Is $CURSOR_RULES_PATH set correctly?
# 2. Did you clone the rules repo?
# 3. Is the path correct?
```

### Test in New Shell

```bash
# Open a new terminal window
echo $CURSOR_RULES_PATH

# Should still show your path
# If empty, the variable wasn't added to shell config
```

### Test in Cursor

1. Open a project in Cursor
2. Create `.cursorrules` with `@${CURSOR_RULES_PATH}/CURSOR.md`
3. Ask Cursor to show you the active rules
4. Verify rules are loaded

---

## Updating Rules

### Environment Variable Method

```bash
# Navigate to rules repo
cd $CURSOR_RULES_PATH

# Pull latest changes
git pull origin main

# Done! All projects now use updated rules
```

### Git Submodule Method

```bash
# In your project
cd /path/to/your/project

# Update submodule to latest
git submodule update --remote .cursor-rules

# Check what changed
git diff .cursor-rules

# Commit the update
git add .cursor-rules
git commit -m "Update cursor rules to latest version"
```

### Lock to Specific Version (Submodule)

```bash
# In your project
cd .cursor-rules

# Checkout specific tag/commit
git checkout v1.0.0

# Go back to project root
cd ..

# Commit the locked version
git add .cursor-rules
git commit -m "Lock cursor rules to v1.0.0"
```

---

## Troubleshooting

### Environment Variable Not Found

**Symptom**: `echo $CURSOR_RULES_PATH` returns empty

**Solutions**:

1. **Check shell config file**:
   ```bash
   # Zsh users
   cat ~/.zshrc | grep CURSOR_RULES_PATH
   
   # Bash users
   cat ~/.bashrc | grep CURSOR_RULES_PATH
   ```

2. **Add if missing**:
   ```bash
   echo 'export CURSOR_RULES_PATH="$HOME/projects/rules"' >> ~/.zshrc
   source ~/.zshrc
   ```

3. **Check shell type**:
   ```bash
   echo $SHELL
   # Output: /bin/zsh or /bin/bash
   # Make sure you edited the right file
   ```

4. **Try absolute path**:
   ```bash
   export CURSOR_RULES_PATH="/Users/yourname/projects/rules"
   ```

---

### File Not Found Errors

**Symptom**: `cat ${CURSOR_RULES_PATH}/CURSOR.md` returns "No such file or directory"

**Solutions**:

1. **Check path exists**:
   ```bash
   ls $CURSOR_RULES_PATH
   ```

2. **Check spelling**:
   ```bash
   # Make sure file name is correct
   ls ${CURSOR_RULES_PATH}/CURSOR.md
   ```

3. **Clone if missing**:
   ```bash
   git clone <rules-repo-url> $CURSOR_RULES_PATH
   ```

---

### Submodule Not Initialized

**Symptom**: `.cursor-rules/` folder is empty

**Solutions**:

```bash
# Initialize submodules
git submodule update --init --recursive

# If that fails, re-add submodule
git submodule add --force <rules-repo-url> .cursor-rules
```

---

### Windows Path Issues

**Symptom**: Paths with backslashes don't work

**Solutions**:

1. **Use forward slashes** (works in PowerShell):
   ```powershell
   $env:CURSOR_RULES_PATH = "$HOME/projects/rules"
   ```

2. **Or escape backslashes**:
   ```powershell
   $env:CURSOR_RULES_PATH = "$HOME\\projects\\rules"
   ```

---

### Cursor Not Loading Rules

**Symptom**: Cursor doesn't apply rules from `.cursorrules`

**Solutions**:

1. **Check `.cursorrules` syntax**:
   ```markdown
   # Correct
   @${CURSOR_RULES_PATH}/CURSOR.md
   
   # Incorrect (no space after @)
   @ ${CURSOR_RULES_PATH}/CURSOR.md
   ```

2. **Restart Cursor**:
   - Close and reopen Cursor
   - Rules are loaded on startup

3. **Check file permissions**:
   ```bash
   ls -l ${CURSOR_RULES_PATH}/CURSOR.md
   # Should be readable (r--)
   ```

---

## Platform-Specific Notes

### macOS

- **Default shell** (Catalina+): Zsh (`~/.zshrc`)
- **Old macOS**: Bash (`~/.bashrc`)
- **Check**: `echo $SHELL`

**Recommended path**: `~/projects/rules`

### Linux

- **Default shell**: Usually Bash (`~/.bashrc`)
- **Some distros**: Zsh (`~/.zshrc`)
- **Check**: `echo $SHELL`

**Recommended path**: `~/projects/rules`

### Windows

- **PowerShell**: Recommended for modern Windows
- **Command Prompt**: Works but less features
- **Git Bash**: Use Linux instructions

**Recommended path**: `%USERPROFILE%\projects\rules`

**Note**: Use forward slashes in paths for cross-platform compatibility

---

## Advanced Configuration

### Multiple Rules Repositories

```bash
# Main rules
export CURSOR_RULES_PATH="$HOME/projects/rules"

# Work-specific rules
export CURSOR_WORK_RULES="$HOME/work/cursor-rules"

# In .cursorrules
@${CURSOR_RULES_PATH}/CURSOR.md
@${CURSOR_WORK_RULES}/work-specific-rules.md
```

### Per-Project Override

```bash
# .envrc (if using direnv)
export CURSOR_RULES_PATH="$PWD/.cursor-rules-override"
```

### Shell Auto-Completion

```bash
# Add to ~/.zshrc
_cursor_rules_complete() {
  if [[ -n "$CURSOR_RULES_PATH" ]]; then
    _files -W $CURSOR_RULES_PATH
  fi
}
```

---

## Next Steps

### After Setup

1. ✅ **Verify setup** - Run verification commands
2. ✅ **Create `.cursorrules`** - In your project
3. ✅ **Test in Cursor** - Open project and verify rules load
4. ✅ **Review rules** - Read `CURSOR.md` and language guides

### Learn More

- **File Locations Guide**: Where to put global and project rules
- **CURSOR.md**: Core mandatory rules for all projects
- **Language Guides**: Python, TypeScript, Swift, Kotlin FP guides
- **Platform Guides**: GCP and AWS specific rules

---

## Quick Reference Card

### Environment Variable Setup (Copy/Paste)

```bash
# 1. Clone rules
git clone <rules-repo-url> ~/projects/rules

# 2. Set variable (Zsh)
echo 'export CURSOR_RULES_PATH="$HOME/projects/rules"' >> ~/.zshrc
source ~/.zshrc

# 3. Verify
echo $CURSOR_RULES_PATH

# 4. Use in project
cat > .cursorrules << 'EOF'
@${CURSOR_RULES_PATH}/CURSOR.md
@${CURSOR_RULES_PATH}/python-fp-style-guide.md
EOF
```

### Git Submodule Setup (Copy/Paste)

```bash
# 1. Add submodule
git submodule add <rules-repo-url> .cursor-rules

# 2. Create .cursorrules
cat > .cursorrules << 'EOF'
@.cursor-rules/CURSOR.md
@.cursor-rules/python-fp-style-guide.md
EOF

# 3. Commit
git add .gitmodules .cursor-rules .cursorrules
git commit -m "Add Cursor rules"
```

---

## Support

**Issues**: Open an issue in the rules repository  
**Questions**: See FAQ in `CURSOR.md`  
**Updates**: `git pull` in rules repository

---

**Setup Time**: 5 minutes  
**Maintenance**: Minimal (git pull when needed)  
**Portability**: Works on all machines ✅

**You're all set!** 🎉

