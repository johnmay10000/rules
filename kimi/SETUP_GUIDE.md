# Kimi Setup Guide

**One-Time Machine Setup** | **5 Minutes** | **All Platforms**

This guide shows you how to set up your machine to use the global Kimi rules repository across all your projects.

---

## Quick Start (TL;DR)

### Option 1: Environment Variable (Recommended)

```bash
# macOS/Linux - Add to ~/.zshrc or ~/.bashrc
export KIMI_RULES_PATH="$HOME/projects/rules"
source ~/.zshrc  # or source ~/.bashrc

# Verify
echo $KIMI_RULES_PATH
```

### Option 2: Git Submodule (Self-Contained)

```bash
# In each project
git submodule add <rules-repo-url> .kimi-rules
```

**Done!** Skip to [Using in Projects](#using-in-projects)

---

## Using in Projects

### Method 1: Project Scope Rules

Create `.kimirules` in your project root:

```bash
# Link to Kimi main rules
ln -s "$KIMI_RULES_PATH/kimi/KIMI.md" .kimirules

# Or copy if you want to customize
cp "$KIMI_RULES_PATH/kimi/KIMI.md" .kimirules
```

### Method 2: Language-Specific Rules

For Python projects:
```bash
ln -s "$KIMI_RULES_PATH/kimi/python-fp-style-guide.md" .python-rules
```

For Rust projects:
```bash
ln -s "$KIMI_RULES_PATH/kimi/rust-fp-style-guide.md" .rust-rules
```

### Method 3: Comprehensive Setup

```bash
export KIMI_RULES_PATH="$HOME/projects/rules"
export KIMI_PYTHON_RULES="$KIMI_RULES_PATH/kimi/python-fp-style-guide.md"
export KIMI_RUST_RULES="$KIMI_RULES_PATH/kimi/rust-fp-style-guide.md"
export KIMI_TYPESCRIPT_RULES="$KIMI_RULES_PATH/kimi/typescript-fp-style-guide.md"
```

---

## Project Structure Examples

### Python Project

```
my-python-project/
├── .kimirules -> $KIMI_RULES_PATH/kimi/KIMI.md
├── .python-rules -> $KIMI_RULES_PATH/kimi/python-fp-style-guide.md
├── docs/
│   └── 2025_11_14/  # Daily work logs
├── src/
└── tests/
```

### Rust Project

```
my-rust-project/
├── .kimirules -> $KIMI_RULES_PATH/kimi/KIMI.md
├── .rust-rules -> $KIMI_RULES_PATH/kimi/rust-fp-style-guide.md
├── docs/
│   └── 2025_11_14/
├── src/
└── Cargo.toml
```

---

## Verification

Check your setup:

```bash
# Verify environment variable
echo $KIMI_RULES_PATH
# Should output: /Users/johnmay/projects/rules

# Verify symlinks work
ls -la .kimirules
# Should show: .kimirules -> /Users/johnmay/projects/rules/kimi/KIMI.md

# Test Kimi can read the rules
kimi read-file .kimirules
```

---

## Integration with Kimi CLI

When Kimi CLI starts, it will automatically:

1. Check for `.kimirules` file in project root
2. Read language-specific rules based on project type
3. Apply FP principles and mandatory requirements
4. Initialize SetTodoList for task tracking
5. Set up Git checkpoint reminders

---

**Last Updated**: 2025-11-14
**Status**: Active
