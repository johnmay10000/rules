---
title: Naming Conventions
category: universal_rules
type: documentation
applies_to: all
version: 1.0.0
last_updated: 2025-11-19
---

# Naming Conventions

Universal naming conventions for files, directories, and documentation artifacts across all projects and AI assistants (Cursor, Kimi, Claude, Gemini).

---

## Core Principles

### 1. Be Descriptive and Explicit

**Rule**: Names should clearly communicate purpose and content.

**Good**:
- `20251119_0000_cursor_rules_refactoring.md`
- `user_authentication_spec.md`
- `api_error_handling_guide.md`

**Bad**:
- `doc1.md` (no meaning)
- `stuff.txt` (vague)
- `final_final_v2.md` (confusing)

---

### 2. Use Consistent Patterns

**Rule**: Follow established patterns for different document types.

**Patterns**:
- Daily work: `YYYYMMDD_NNNN_description.md`
- Plans: `FEATURE_NAME_plan.md`
- TODOs: `FEATURE_NAME_todo.md`
- Templates: `template_purpose.md`
- Examples: `example_topic.md`

---

### 3. Prefer Kebab Case for Files

**Rule**: Use lowercase with hyphens for file names.

**Format**: `descriptive-file-name.md`

**Examples**:
- ✅ `user-authentication-guide.md`
- ✅ `api-error-handling.md`
- ✅ `setup-instructions.md`

**Not**:
- ❌ `UserAuthenticationGuide.md` (camelCase)
- ❌ `user_authentication_guide.md` (snake_case)
- ❌ `user authentication guide.md` (spaces)

**Rationale**:
- Works across all operating systems (Windows, macOS, Linux)
- URL-friendly (no encoding needed)
- Easy to read and type
- Consistent with web standards

---

## File Naming Conventions

### Daily Work Files

**Pattern**: `YYYYMMDD_NNNN_descriptive_name.md`

**Components**:
1. **Date**: `YYYYMMDD` (8 digits, no separators)
2. **Sequence**: `NNNN` (4 digits, starting from 0000)
3. **Description**: `descriptive_name` (kebab-case, summary of work)

**Examples**:
```
20251119_0000_cursor-rules-refactoring.md
20251119_0001_mcp-server-deployment.md
20251119_0002_phase-2-completion.md
```

**Rules**:
- Sequence increments throughout the day
- Reset sequence to 0000 for each new day
- Description should be 3-5 words summarizing the work
- Never reuse sequence numbers

---

### Plan Documents

**Pattern**: `FEATURE_NAME_plan.md`

**Components**:
1. **Feature**: `FEATURE_NAME` (kebab-case, uppercase for acronyms)
2. **Suffix**: `_plan.md`

**Examples**:
```
mcp-implementation_plan.md
unified-rules-refactor_plan.md
digital-ocean-deployment_plan.md
```

**Paired TODO List**:
- Every plan has a matching TODO list
- Pattern: `FEATURE_NAME_todo.md`

**Example Pair**:
```
mcp-implementation_plan.md
mcp-implementation_todo.md
```

---

### TODO Lists

**Pattern**: `FEATURE_NAME_todo.md`

**Components**:
1. **Feature**: `FEATURE_NAME` (matches corresponding plan)
2. **Suffix**: `_todo.md`

**Content Structure**:
```markdown
# TODO List: Feature Name

## Progress Overview
- Total Tasks: X
- Completed: Y ✅
- In Progress: Z 🔄
- Pending: W ⏳

## Tasks
- [ ] Task 1
- [ ] Task 2
```

---

### Template Files

**Pattern**: `template_purpose.md`

**Components**:
1. **Prefix**: `template_`
2. **Purpose**: `purpose` (kebab-case description)

**Examples**:
```
template_daily-work.md
template_plan-document.md
template_commit-message.md
```

**Location**: `templates/` directory

---

### Example Files

**Pattern**: `example_topic.md` or `example_language_topic.md`

**Components**:
1. **Prefix**: `example_`
2. **Topic**: `topic` or `language_topic`

**Examples**:
```
example_python_result-type.md
example_typescript_either.md
example_rust_error-handling.md
```

**Location**: `examples/language/` directory

---

### Documentation Guides

**Pattern**: `topic-guide.md` or `topic-overview.md`

**Examples**:
```
setup-guide.md
workflow-overview.md
architecture-overview.md
```

---

## Directory Naming Conventions

### Root-Level Directories

**Standard Directories**:
- `docs/` - All documentation
- `templates/` - Reusable templates
- `examples/` - Code and usage examples
- `src/` or `lib/` - Source code
- `tests/` - Test files
- `scripts/` - Automation scripts

**Rules**:
- All lowercase
- Kebab-case if multiple words
- No spaces or special characters

**Examples**:
- ✅ `docs/`
- ✅ `code-guidelines/`
- ✅ `project-setup/`

---

### Documentation Subdirectories

### Daily Work Folders

**Pattern**: `docs/YYYY_MM_DD/`

**Components**:
- `docs/` root
- Date folder: `YYYY_MM_DD` (year, month, day with underscores)

**Examples**:
```
docs/2025_11_19/
docs/2025_11_20/
```

**Contents**: All work files for that date

---

### Plans Directory

**Pattern**: `docs/plans/`

**Contents**:
- Plan documents (`*_plan.md`)
- TODO lists (`*_todo.md`)
- Architecture documents
- Strategic planning documents

**Examples**:
```
docs/plans/mcp-implementation_plan.md
docs/plans/mcp-implementation_todo.md
docs/plans/architecture-overview.md
```

---

### Language-Specific Examples

**Pattern**: `examples/language/`

**Examples**:
```
examples/python/
examples/typescript/
examples/rust/
```

**Contents**: Language-specific code examples

---

## Date and Version Conventions

### Dates in File Names

**Format**: `YYYYMMDD` (no separators)

**Examples**:
- ✅ `20251119` (November 19, 2025)
- ✅ `20251231` (December 31, 2025)

**Not**:
- ❌ `2025-11-19` (separators)
- ❌ `11-19-2025` (ambiguous format)

---

### Dates in Content

**Format**: `YYYY-MM-DD` (ISO 8601)

**Examples**:
- ✅ `2025-11-19`
- ✅ `2025-12-31`

**Usage**: Within document content, headers, metadata

---

### Version Numbers

**Format**: `MAJOR.MINOR.PATCH`

**Examples**:
- `1.0.0` (initial release)
- `1.1.0` (new features)
- `1.1.1` (bug fixes)
- `2.0.0` (breaking changes)

**Location**: In document frontmatter, README files, package files

---

## Metadata Conventions

### Document Frontmatter

All documentation files should include frontmatter:

```yaml
---
title: Document Title
category: universal_rules|code_guidelines|ai_tool_specific
type: git|testing|documentation|language
applies_to: all|[cursor, kimi, claude, gemini]
version: 1.0.0
last_updated: 2025-11-19
---
```

**Required Fields**:
- `title`: Descriptive title
- `category`: Document category
- `type`: Document type
- `applies_to`: Target audience
- `version`: Document version
- `last_updated`: ISO date

---

## Tool-Specific Naming

### AI Assistant Files

**Pattern**: `ai_tool_specific/tool/feature.md`

**Examples**:
```
ai_tool_specific/kimi/kimi-cli-features.md
ai_tool_specific/cursor/vscode-integration.md
ai_tool_specific/claude/api-usage.md
ai_tool_specific/gemini/bard-migration.md
```

---

### Language Guidelines

**Pattern**: `code_guidelines/languages/language/filename.md`

**Examples**:
```
code_guidelines/languages/python/fp_style_guide.md
code_guidelines/languages/typescript/patterns.md
code_guidelines/languages/rust/examples.md
```

**Files per Language**:
- `fp_style_guide.md` - Main style guide
- `patterns.md` - Pattern reference
- `examples.md` - Code examples
- `libraries.md` - Library guide

---

## Consistency Rules

### File Extensions

- Markdown: `.md`
- JSON: `.json`
- YAML: `.yaml` or `.yml` (be consistent)
- Python: `.py`
- TypeScript: `.ts`
- Rust: `.rs`

**Rule**: Use lowercase extensions consistently

---

### README Files

**Name**: `README.md` (exactly, uppercase)

**Location**: Root of directories that need explanation

**Contents**:
- Directory purpose
- File organization
- How to use contents
- Key files to start with

---

### Index Files

**Name**: `index.md` or `index.json`

**Purpose**: Provide overview and navigation for directory contents

**Examples**:
```
code_guidelines/index.json
templates/index.md
examples/index.md
```

---

## Anti-Patterns (DO NOT DO)

### ❌ Inconsistent Casing

```
docs/2025_11_19/20251119_0000_Some-File.md  # Mixed case
docs/2025_11_19/20251119_0001_another_file.md  # Inconsistent
```

**Problem**: Inconsistent naming makes files hard to find and sort

---

### ❌ Vague Descriptions

```
20251119_0000_work.md  # Too vague
20251119_0001_stuff.md  # No meaning
```

**Problem**: Cannot understand file contents from name

---

### ❌ Special Characters

```
docs/2025_11_19/20251119_0000_file@#$%.md  # Special chars
docs/plans/mcp implementation plan.md  # Spaces
```

**Problem**: Special characters cause issues in URLs and scripts

---

### ❌ Missing Sequence Numbers

```
docs/2025_11_19/20251119_work.md  # Missing sequence
```

**Problem**: Files don't sort chronologically within a day

---

### ❌ Wrong Date Format

```
docs/2025-11-19/2025-11-19_0000_work.md  # Wrong folder format
```

**Problem**: Breaks automation and sorting

---

## Examples by Document Type

### Daily Work File

```
docs/2025_11_19/20251119_0000_mcp-server-deployment.md
```

**Structure**:
- Folder: `docs/2025_11_19/`
- File: `20251119_0000_mcp-server-deployment.md`
- Contains: Work done on 2025-11-19, first task of the day, about MCP server deployment

---

### Plan Document

```
docs/plans/mcp-implementation_plan.md
docs/plans/mcp-implementation_todo.md
```

**Structure**:
- Folder: `docs/plans/`
- Files: `mcp-implementation_plan.md` and `mcp-implementation_todo.md`
- Contains: Implementation plan and TODO list for MCP feature

---

### Template

```
templates/template_daily-work.md
```

**Structure**:
- Folder: `templates/`
- File: `template_daily-work.md`
- Contains: Template for daily work files

---

### Language Example

```
examples/python/example_result-type-usage.md
```

**Structure**:
- Folder: `examples/python/`
- File: `example_result-type-usage.md`
- Contains: Python example showing Result type usage

---

## Tool-Specific Notes

### For Kimi Users

Kimi provides naming assistance:
- **Auto-completion**: Suggests next sequence number
- **Validation**: Checks naming format compliance
- **Batch Renaming**: Fix naming inconsistencies across files
- **Template Generation**: Creates correctly named files from templates

**Kimi Commands**:
```bash
# Create daily work file with correct naming
kimi create-daily-work "MCP server deployment"

# Validate naming in directory
kimi validate-naming docs/2025_11_19/

# Fix naming inconsistencies
kimi fix-naming --directory docs/plans/
```

---

### For Cursor Users

Cursor provides IDE-integrated naming support:
- **File Template Snippets**: Insert correct naming patterns
- **Rename Refactoring**: Bulk rename with pattern compliance
- **Linting**: Warn about naming violations
- **Auto-completion**: Suggest names based on patterns

**Cursor Features**:
- Snippet: `daily-work` → creates `YYYYMMDD_NNNN_description.md`
- Command: "Create plan document" → prompts for feature name, creates correctly named files

---

### For Claude Users

Claude excels at naming strategy:
- **Pattern Design**: Create naming conventions for new project types
- **Consistency Review**: Review and suggest naming improvements
- **Bulk Renaming Plans**: Generate scripts to fix naming issues
- **Documentation**: Explain naming rationale and benefits

---

### For Gemini Users

Gemini provides comprehensive naming guidance:
- **Multi-Language**: Naming conventions across programming languages
+- **Best Practices**: Industry-standard naming patterns
+- **Anti-Pattern Detection**: Identify and fix naming issues
+- **Automation**: Generate scripts for naming validation

---

## Migration Guide

### Converting Old Naming to New Convention

If you have existing files with inconsistent naming:

1. **Audit Current State**:
   ```bash
   find . -name "*.md" | grep -v "^[a-z0-9_/-]*$"  # Find files with bad naming
   ```

2. **Create Migration Script**:
   ```bash
   #!/bin/bash
   # Rename files to follow conventions
   mv "Old File Name.md" old-file-name.md
   mv "Another_Bad_Name.md" another-bad-name.md
   ```

3. **Update Internal References**:
   ```bash
   # Update all links in markdown files
   find . -name "*.md" -exec sed -i 's/Old-File-Name/old-file-name/g' {} \;
   ```

4. **Commit Changes**:
   ```bash
   git add .
   git commit -m "Standardize naming conventions
   
   Renamed files to follow kebab-case naming convention.
   Updated all internal references.
   
   Rationale: Consistent naming improves discoverability
   and enables automation.
   
   Files: All .md files renamed
   Status: Naming standardization complete ✅
   
   🤖 Generated with [AI Assistant]
   Co-Authored-By: AI <noreply@anthropic.com>"
   ```

---

## Validation

### Automated Naming Checks

Add to CI/CD pipeline:

```yaml
# .github/workflows/naming-check.yml
name: Naming Convention Check

on: [push, pull_request]

jobs:
  validate-naming:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      
      - name: Check file naming
        run: |
          # Check for uppercase letters
          if find . -name "*.md" | grep -q "[A-Z]"; then
            echo "❌ Found files with uppercase letters"
            find . -name "*.md" | grep "[A-Z]"
            exit 1
          fi
          
          # Check for spaces
          if find . -name "*.md" | grep -q " "; then
            echo "❌ Found files with spaces"
            find . -name "*.md" | grep " "
            exit 1
          fi
          
          echo "✅ All files follow naming conventions"
```

---

## Related Documents

- **File Organization**: `file_organization.md` - Where to place files
- **Documentation Standards**: `daily_workflow.md` - How to structure content
- **Project Structure**: `../project_structure/directory_layout.md` - Directory organization

---

**Last Updated**: 2025-11-19  
**Maintained By**: Global AI Rules System  
**Status**: Active  
**Applies To**: All AI assistants (Cursor, Kimi, Claude, Gemini)