# File Locations User Guide - Portable Version for Kimi

**Created**: 2025-11-14  
**Priority**: 🔥 CRITICAL - Essential for Setup  
**Status**: ✅ PORTABLE - Works on Any Machine  
**Purpose**: Define where rule files should be located for Kimi CLI to find them

---

## 🌍 PORTABILITY FIRST

**Critical**: This guide uses PORTABLE paths that work on any machine.

### Two Portable Approaches

1. **Environment Variable** (Recommended)
   - Set `$KIMI_RULES_PATH` once per machine
   - Reference: `@${KIMI_RULES_PATH}/KIMI.md`
   - Works on: macOS, Linux, Windows

2. **Git Submodule** (Self-Contained)
   - Add rules as `.kimi-rules/` submodule
   - Reference: `@.kimi-rules/KIMI.md`
   - Works on: Any machine, no setup needed

**Setup Instructions**: See [SETUP_GUIDE.md](SETUP_GUIDE.md)

---

## Quick Answer

### For Global Rules (All Projects)

#### Method 1: Environment Variable (Recommended)

```bash
# In your project root, create .kimirules file:
ln -s "${KIMI_RULES_PATH}/kimi/KIMI.md" .kimirules

# For language-specific rules:
ln -s "${KIMI_RULES_PATH}/kimi/python-fp-style-guide.md" .python-rules
ln -s "${KIMI_RULES_PATH}/kimi/typescript-fp-style-guide.md" .typescript-rules
ln -s "${KIMI_RULES_PATH}/kimi/rust-fp-style-guide.md" .rust-rules
ln -s "${KIMI_RULES_PATH}/kimi/swift-fp-style-guide.md" .swift-rules
ln -s "${KIMI_RULES_PATH}/kimi/kotlin-fp-style-guide.md" .kotlin-rules
ln -s "${KIMI_RULES_PATH}/kimi/haskell-fp-style-guide.md" .haskell-rules
```

#### Method 2: Git Submodule

```bash
# In each project:
git submodule add <rules-repo-url> .kimi-rules
ln -s .kimi-rules/kimi/KIMI.md .kimirules
```

---

## Reference Syntax

### Environment Variable Approach

```markdown
# In your .kimirules file:

## Global Rules
!@${KIMI_RULES_PATH}/kimi/KIMI.md

## Language-Specific (auto-detected)
!@${KIMI_RULES_PATH}/kimi/python-fp-style-guide.md
!@${KIMI_RULES_PATH}/kimi/typescript-fp-style-guide.md

## Workflow Guide
!@${KIMI_RULES_PATH}/kimi/KIMI_WORKFLOW_GUIDE.md
```

### Git Submodule Approach

```markdown
# In your .kimirules file:

## Global Rules
!@.kimi-rules/kimi/KIMI.md

## Language-Specific
!@.kimi-rules/kimi/python-fp-style-guide.md
!@.kimi-rules/kimi/typescript-fp-style-guide.md
```

---

## Kimi-Specific Auto-Detection

Kimi can auto-detect your project type and load appropriate rules:

### Smart .kimirules Template

```markdown
# .kimirules - Smart Auto-Detection

## Base Rules (Always Load)
@${KIMI_RULES_PATH}/kimi/KIMI.md

## Language Detection and Loading
# Kimi checks your project files and loads appropriate guides

#{ if (package.json exists) }
@${KIMI_RULES_PATH}/kimi/typescript-fp-style-guide.md

#{ if (*.py files exist) }
@${KIMI_RULES_PATH}/kimi/python-fp-style-guide.md

#{ if (Cargo.toml exists) }
@${KIMI_RULES_PATH}/kimi/rust-fp-style-guide.md

#{ if (*.swift files exist) }
@${KIMI_RULES_PATH}/kimi/swift-fp-style-guide.md

#{ if (build.gradle.kts exists) }
@${KIMI_RULES_PATH}/kimi/kotlin-fp-style-guide.md

#{ if (*.hs files exist) }
@${KIMI_RULES_PATH}/kimi/haskell-fp-style-guide.md

## Platform Detection (Optional)
#{ if (serverless.yml exists) }
@${KIMI_RULES_PATH}/kimi/aws-fp-style-guide.md

#{ if (cloudbuild.yaml exists) }
@${KIMI_RULES_PATH}/kimi/gcp-fp-style-guide.md
```

---

## File Organization Reference

### Project Root Structure

```
my-project/
├── .kimirules                     # Links to global rules (symlink)
├── .cursorrules                   # Cursor rules (if also using Cursor)
├── ARCHITECTURE_PLAN.md           # Tier 1: Strategic
├── docs/
│   ├── plans/                     # Tier 2: Tactical plans
│   │   ├── FEATURE_PLAN.md
│   │   └── FEATURE_TODO.md
│   └── 2025_11_14/                # Tier 3: Daily work
│       ├── 20251114_0000_*.md
│       └── 20251114_0001_*.md
├── src/
│   ├── main.py                    # Your source code
│   └── main.ts
├── tests/
│   └── test_main.py
└── .git/
```

### Rules Repository Structure

```
projects/rules/                    # Your KIMI_RULES_PATH
├── KIMI.md                        # Root-level reference
├── CLAUDE.md                      # Claude rules (if needed)
├── .cursorrules                   # Cursor rules
├── kimi/                          # Kimi rules directory
│   ├── KIMI.md                    # Main Kimi rules
│   ├── KIMI_FP_PRINCIPLES.md      # FP principles
│   ├── KIMI_WORKFLOW_GUIDE.md     # Workflow guide
│   ├── SETUP_GUIDE.md             # Setup instructions
│   ├── python-fp-style-guide.md   # Python guide
│   ├── typescript-fp-style-guide.md
│   ├── rust-fp-style-guide.md
│   ├── swift-fp-style-guide.md
│   ├── kotlin-fp-style-guide.md
│   ├── haskell-fp-style-guide.md
│   ├── templates/
│   │   ├── .kimirules_smart_template_envvar
│   │   └── .kimirules_smart_template_submodule
│   └── examples/
│       ├── python_project/
│       ├── typescript_project/
│       └── ...
└── docs/
    ├── plans/
    └── 2025_11_14/
```

---

## Environment Variables Reference

### KIMI_RULES_PATH (Required)

```bash
# Set in your shell profile (~/.zshrc, ~/.bashrc)

# macOS/Linux
export KIMI_RULES_PATH="$HOME/projects/rules"

# Windows (PowerShell)
$env:KIMI_RULES_PATH="C:\Users\YourName\projects\rules"

# Windows (Command Prompt)
set KIMI_RULES_PATH=C:\Users\YourName\projects\rules

# Verify
echo $KIMI_RULES_PATH
ls $KIMI_RULES_PATH/kimi/
```

### Optional Language Variables

```bash
# Optional: Set individual language paths
export KIMI_PYTHON_RULES="$KIMI_RULES_PATH/kimi/python-fp-style-guide.md"
export KIMI_TYPESCRIPT_RULES="$KIMI_RULES_PATH/kimi/typescript-fp-style-guide.md"
export KIMI_RUST_RULES="$KIMI_RULES_PATH/kimi/rust-fp-style-guide.md"
export KIMI_SWIFT_RULES="$KIMI_RULES_PATH/kimi/swift-fp-style-guide.md"
export KIMI_KOTLIN_RULES="$KIMI_RULES_PATH/kimi/kotlin-fp-style-guide.md"
export KIMI_HASKELL_RULES="$KIMI_RULES_PATH/kimi/haskell-fp-style-guide.md"
```

---

## Platform-Specific Setup Examples

### macOS/Linux (Recommended)

```bash
# 1. Add to ~/.zshrc or ~/.bash_profile
echo 'export KIMI_RULES_PATH="$HOME/projects/rules"' >> ~/.zshrc

# 2. Reload shell
source ~/.zshrc

# 3. Create symlinks in project
cd my-project
ln -s "${KIMI_RULES_PATH}/kimi/KIMI.md" .kimirules

# 4. Verify
cat .kimirules  # Should show content
```

### Windows PowerShell

```powershell
# 1. Add to your PowerShell profile
$profileContent = @'
$env:KIMI_RULES_PATH = "$env:USERPROFILE\projects\rules"
'@
Add-Content -Path $PROFILE -Value $profileContent

# 2. Reload PowerShell
# Restart PowerShell

# 3. Create symlinks
# (Note: May require admin privileges)
cd my-project
New-Item -ItemType SymbolicLink -Path .kimirules -Target "$env:KIMI_RULES_PATH\kimi\KIMI.md"
```

### Git Submodule (Cross-Platform)

```bash
# 1. Add submodule (same on all platforms)
git submodule add https://github.com/user/rules-repo.git .kimi-rules

# 2. Create symlink/shortcut
# macOS/Linux:
ln -s .kimi-rules/kimi/KIMI.md .kimirules

# Windows (PowerShell - requires admin):
New-Item -ItemType SymbolicLink -Path .kimirules -Target .kimi-rules\kimi\KIMI.md
```

---

## Troubleshooting

### Problem: "File not found: ${KIMI_RULES_PATH}/kimi/KIMI.md"

**Solution**: Check environment variable
```bash
echo $KIMI_RULES_PATH  # Should show path

# If empty, set it:
export KIMI_RULES_PATH="$HOME/projects/rules"

# Then verify files exist:
ls $KIMI_RULES_PATH/kimi/KIMI.md
```

### Problem: "Permission denied creating symlink"

**Solution**: Check file permissions
```bash
# macOS/Linux
ls -la $(dirname .kimirules)  # Check directory permissions
chmod +w .  # Make directory writable

# Windows: Run as administrator or enable developer mode
```

### Problem: "Kimi not loading rules"

**Solution**: Verify symlink works
```bash
# Test the symlink
cat .kimirules  # Should show content

# If symlink broken:
rm .kimirules
ln -s "${KIMI_RULES_PATH}/kimi/KIMI.md" .kimirules
```

### Problem: "Wrong language guide loading"

**Solution**: Check auto-detection or use explicit
```bash
# Verify detection logic
cat .kimirules

# Make language explicit in .kimirules:
!@${KIMI_RULES_PATH}/kimi/typescript-fp-style-guide.md
```

---

## Verification Checklist

After setup, verify everything works:

- [ ] `echo $KIMI_RULES_PATH` shows correct path
- [ ] `ls $KIMI_RULES_PATH/kimi/KIMI.md` shows file
- [ ] `cat .kimirules` shows correct content
- [ ] Kimi recognizes rules: "test" command shows rules loaded
- [ ] Language-specific rules auto-load (if configured)
- [ ] Can create new project from template

---

## Example Project Workflows

### Python Project Setup

```bash
cd my-python-project

# 1. Link global rules
ln -s "${KIMI_RULES_PATH}/kimi/KIMI.md" .kimirules
ln -s "${KIMI_RULES_PATH}/kimi/python-fp-style-guide.md" .python-rules

# 2. Verify
cat .kimirules

# 3. Create daily work folder
mkdir -p docs/2025_11_14

# 4. Start coding with Kimi
```

### Multi-Language Project Setup

```bash
cd my-polyglot-project

# Link all relevant language guides
ln -s "${KIMI_RULES_PATH}/kimi/KIMI.md" .kimirules
ln -s "${KIMI_RULES_PATH}/kimi/typescript-fp-style-guide.md" .typescript-rules
ln -s "${KIMI_RULES_PATH}/kimi/python-fp-style-guide.md" .python-rules
ln -s "${KIMI_RULES_PATH}/kimi/rust-fp-style-guide.md" .rust-rules

# Kimi will auto-detect based on file extensions
```

---

## Summary

Use environment variables or git submodules for portability. Always reference Kimi rules using relative or env-var paths. Configure your project once, then Kimi handles the rest.

**Key Takeaways**:
- Set `$KIMI_RULES_PATH` environment variable
- Use symlinks to reference global rules
- Auto-detection for language-specific rules
- Git submodule alternative for team use
- Verify setup with checklist

**Next**: See [SETUP_GUIDE.md](SETUP_GUIDE.md) for step-by-step setup instructions.

---

**Last Updated**: 2025-11-14  
**Maintained By**: Kimi CLI Global Rules System  
**Status**: Active
