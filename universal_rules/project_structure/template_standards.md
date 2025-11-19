---
title: Template Standards
category: universal_rules
type: project_structure
applies_to: all
version: 1.0.0
last_updated: 2025-11-19
---

# Template Standards

**Status**: MANDATORY - All templates must follow these standards

Universal standards for creating, organizing, and maintaining reusable templates across all projects and AI assistants.

---

## What Are Templates?

Templates are **reusable, parameterized documents** that provide consistent structure for common tasks. They eliminate duplication and ensure consistency across projects.

**Examples**:
- Daily work document templates
- Plan/TODO list templates
- Project setup templates
- Code file templates
- Configuration templates

---

## Template Storage Location

### Directory Structure

```
project/
├── templates/                      # Root templates directory
│   ├── daily_work/                # Daily work templates
│   │   └── daily_work_template.md
│   ├── plans/                     # Planning templates
│   │   ├── feature_plan.md
│   │   └── feature_todo.md
│   ├── project_setup/             # Project initialization templates
│   │   ├── cursor_project_setup.md
│   │   └── kimi_project_setup.md
│   └── code/                      # Code file templates
│       ├── python_class.md
│       └── typescript_interface.md
└── docs/                          # Documentation (not templates)
```

**Rule**: All templates **MUST** be stored in the `templates/` directory at repository root.

---

## Template Naming Conventions

### Format: `[purpose]_[type]_template.md`

**Examples**:
- `daily_work_template.md`
- `feature_plan_template.md`
- `feature_todo_template.md`
- `cursor_project_setup_template.md`
- `python_class_template.md`

### Naming Rules

1. **Use lowercase with underscores** (kebab-case or snake_case)
2. **Include purpose**: What the template is for
3. **Include type**: `template`, `snippet`, `boilerplate`
4. **Be descriptive**: Name should clearly indicate usage
5. **Version when needed**: `template_v2.md` (rare, prefer updates)

**✅ Good Examples**:
```
daily_work_template.md
feature_plan_template.md
api_endpoint_template.md
cursor_rules_template.md
kimi_project_setup_template.md
```

**❌ Bad Examples**:
```
template1.md          # Not descriptive
my_template.md        # Vague
temp.md               # Too short
DailyWork.md          # Wrong case (should be lowercase)
```

---

## Template Metadata

Every template **MUST** include frontmatter metadata:

```yaml
---
title: Template Title
category: [daily_work | plans | project_setup | code]
type: template
purpose: Brief description of what this template is for
usage: When to use this template
version: 1.0.0
last_updated: 2025-11-19
---
```

### Required Metadata Fields

| Field | Description | Example |
|-------|-------------|---------|
| `title` | Human-readable title | "Daily Work Template" |
| `category` | Template category | `daily_work`, `plans`, `project_setup`, `code` |
| `type` | Always "template" | `template` |
| `purpose` | What this template does | "Structure for daily work documentation" |
| `usage` | When to use it | "Use at start of each work session" |
| `version` | Semantic version | `1.0.0` |
| `last_updated` | ISO date | `2025-11-19` |

---

## Template Structure

### Section Order

Templates should follow this section order:

1. **Header/Title** (from frontmatter)
2. **Quick Start** (how to use the template)
3. **Parameters** (what to fill in)
4. **Sections** (the actual template structure)
5. **Examples** (filled-out example)
6. **Best Practices** (tips for using the template)

### Example: Daily Work Template Structure

```markdown
---
title: Daily Work Template
category: daily_work
type: template
purpose: Structure for documenting daily work sessions
usage: Use at start of each work session
version: 1.0.0
last_updated: 2025-11-19
---

# Daily Work Template

## Quick Start
1. Copy this template to `docs/YYYY_MM_DD/YYYYMMDD_NNNN_description.md`
2. Fill in sections below
3. Update at end of session

## Parameters
- **Date**: YYYY-MM-DD
- **Session Number**: NNNN (0000, 0001, etc.)
- **Focus**: What you're working on

## Template Sections

### 1. Session Goal
[What you plan to accomplish]

### 2. Tasks
- [ ] Task 1
- [ ] Task 2

### 3. Decisions Made
[Any decisions made during session]

### 4. Next Steps
[What to do next]

### 5. Blockers
[Any obstacles]

## Example
See `docs/2025_11_19/20251119_0000_example.md`

## Best Practices
- Be specific about goals
- Update throughout session
- Include context for future reference
```

---

## Creating Reusable Templates

### Principle: DRY (Don't Repeat Yourself)

If you find yourself creating similar documents more than twice, create a template.

### Steps to Create a Template

1. **Identify the pattern**: What common structure do you need?
2. **Extract parameters**: What parts change each time?
3. **Create template**: Follow the structure above
4. **Add metadata**: Complete all frontmatter fields
5. **Document usage**: Explain when and how to use it
6. **Provide example**: Show a filled-out version
7. **Test the template**: Use it yourself first
8. **Document in index**: Add to `templates/index.md`

### Template Parameterization

Use clear placeholders for variable content:

**Good**: `[PARAMETER_NAME]` or `{{parameter_name}}`
```markdown
# {{date}} - {{session_type}}

## Goal
{{session_goal}}

## Tasks
{{task_list}}
```

**Bad**: Vague or inconsistent placeholders
```markdown
# Date - Session

## Goal
[Enter goal here]

## Tasks
[Fill in tasks]
```

---

## Template Categories

### 1. Daily Work Templates

**Purpose**: Document daily work sessions

**Examples**:
- `daily_work_session_template.md`
- `session_summary_template.md`
- `task_tracking_template.md`

**Location**: `templates/daily_work/`

---

### 2. Planning Templates

**Purpose**: Plan features, projects, or initiatives

**Examples**:
- `feature_plan_template.md`
- `feature_todo_template.md`
- `architecture_plan_template.md`
- `migration_plan_template.md`

**Location**: `templates/plans/`

**Structure**:
- Plan overview
- Goals and objectives
- Success criteria
- Implementation phases
- Risk assessment
- Timeline

---

### 3. Project Setup Templates

**Purpose**: Initialize new projects with consistent structure

**Examples**:
- `cursor_project_setup_template.md`
- `kimi_project_setup_template.md`
- `python_project_template.md`
- `typescript_project_template.md`

**Location**: `templates/project_setup/`

**Contents**:
- Directory structure
- Configuration files
- Initial documentation
- Setup instructions

---

### 4. Code Templates

**Purpose**: Provide consistent code file structure

**Examples**:
- `python_class_template.md`
- `typescript_interface_template.md`
- `rust_module_template.md`
- `react_component_template.md`

**Location**: `templates/code/`

**Contents**:
- Boilerplate code
- Import statements
- Docstring/template
- Best practice examples

---

### 5. Documentation Templates

**Purpose**: Standardize documentation format

**Examples**:
- `api_documentation_template.md`
- `readme_template.md`
- `contributing_template.md`
- `changelog_template.md`

**Location**: `templates/documentation/`

---

## Template Best Practices

### ✅ DO:

1. **Keep templates focused**: One template per specific use case
2. **Make parameters clear**: Explicitly list what needs to be filled in
3. **Provide examples**: Show a completed version
4. **Version templates**: Update version when template changes
5. **Document usage**: Explain when and how to use the template
6. **Keep templates DRY**: Don't duplicate content across templates
7. **Use consistent formatting**: Follow markdown best practices
8. **Make templates discoverable**: Add to `templates/index.md`
9. **Test templates**: Use them yourself before sharing
10. **Keep templates up to date**: Review and update quarterly

### ❌ DON'T:

1. **Don't create overly generic templates**: Too vague to be useful
2. **Don't hardcode specific values**: Use parameters instead
3. **Don't skip documentation**: Templates need usage instructions
4. **Don't create too many templates**: If you have >20 templates, some are probably unused
5. **Don't let templates get outdated**: Review every 3 months
6. **Don't use templates as documentation**: Templates are for creation, not reference
7. **Don't make templates too rigid**: Allow for reasonable customization

---

## Template Index

Create an index file at `templates/index.md` that lists all available templates:

```markdown
---
title: Template Index
category: templates
type: index
purpose: Central index of all available templates
version: 1.0.0
last_updated: 2025-11-19
---

# Template Index

## Daily Work Templates
| Template | Purpose | File |
|----------|---------|------|
| Daily Work Session | Document daily work | `daily_work/daily_work_session_template.md` |
| Session Summary | Summarize work session | `daily_work/session_summary_template.md` |

## Planning Templates
| Template | Purpose | File |
|----------|---------|------|
| Feature Plan | Plan new features | `plans/feature_plan_template.md` |
| Feature TODO | Track feature tasks | `plans/feature_todo_template.md` |

## Project Setup Templates
| Template | Purpose | File |
|----------|---------|------|
| Cursor Project Setup | Initialize Cursor project | `project_setup/cursor_project_setup_template.md` |
| Kimi Project Setup | Initialize Kimi project | `project_setup/kimi_project_setup_template.md` |
```

---

## Template Maintenance

### Quarterly Review Process

Every 3 months, review all templates:

1. **Check usage**: Which templates are actually being used?
2. **Gather feedback**: Ask team what's working and what's not
3. **Update outdated templates**: Refresh content and examples
4. **Remove unused templates**: Archive templates that are never used
5. **Add missing templates**: Create templates for common new patterns
6. **Update versions**: Increment version numbers for changed templates

### Template Versioning

Use semantic versioning:
- **Major** (X.0.0): Breaking changes (structure changed significantly)
- **Minor** (0.X.0): New features (new sections or parameters)
- **Patch** (0.0.X): Bug fixes (typos, clarifications)

### Deprecation Process

When deprecating a template:

1. **Mark as deprecated** in frontmatter:
   ```yaml
   ---
   title: Old Template
   status: deprecated
   replacement: new_template_name.md
   ---
   ```

2. **Add deprecation notice** at top:
   ```markdown
   > **DEPRECATED**: This template is deprecated as of 2025-11-19.
   > Use [new_template_name.md](./new_template_name.md) instead.
   ```

3. **Update index**: Mark as deprecated in `templates/index.md`

4. **Remove after 3 months**: Delete deprecated template after grace period

---

## Template Examples

### Example 1: Feature Plan Template

```markdown
---
title: Feature Plan Template
category: plans
type: template
purpose: Structure for planning new features
usage: Use when starting work on a new feature
version: 1.0.0
last_updated: 2025-11-19
---

# Feature Plan: [FEATURE_NAME]

## Overview
**Feature**: [Brief description]
**Owner**: [Your name]
**Date**: [YYYY-MM-DD]

## Goals
- [ ] Goal 1
- [ ] Goal 2

## Success Criteria
- [ ] Criterion 1
- [ ] Criterion 2

## Implementation Plan
### Phase 1: [Description]
- [ ] Task 1
- [ ] Task 2

### Phase 2: [Description]
- [ ] Task 1
- [ ] Task 2

## Risks
- Risk 1: [Mitigation]

## Timeline
- Start: [Date]
- End: [Date]
```

### Example 2: Python Class Template

```markdown
---
title: Python Class Template
category: code
type: template
purpose: Boilerplate for Python classes
usage: Use when creating new Python classes
version: 1.0.0
last_updated: 2025-11-19
---

# Python Class Template

```python
from dataclasses import dataclass
from typing import Optional

@dataclass(frozen=True)
class ClassName:
    """
    Brief description of the class.
    
    Attributes:
        attribute1: Description of attribute1
        attribute2: Description of attribute2
    """
    
    attribute1: str
    attribute2: Optional[int] = None
    
    def method_name(self, param: str) -> ResultType:
        """
        Brief description of the method.
        
        Args:
            param: Description of parameter
            
        Returns:
            Description of return value
            
        Raises:
            ExceptionType: When this happens
        """
        # Implementation here
        pass
    
    @classmethod
    def create(cls, data: dict) -> 'ClassName':
        """Factory method to create instance from dict."""
        return cls(
            attribute1=data['attribute1'],
            attribute2=data.get('attribute2')
        )
```

## Best Practices
- Use type hints
- Keep classes immutable when possible
- Add docstrings
- Include factory methods
- Test all methods
```

---

## Tool-Specific Notes

### For Kimi Users

Kimi provides enhanced template support:
- **Template Generation**: Create templates from existing documents
- **Parameter Extraction**: Automatically identify variable parts
- **Batch Template Application**: Apply templates to multiple files
- **SetTodoList Integration**: Track template usage and completion

**Kimi-Specific Commands**:
```bash
# Generate template from existing file
kimi create-template --from docs/2025_11_19/20251119_0000_work.md

# Apply template to new file
kimi apply-template --template daily_work --output docs/2025_11_20/

# List available templates
kimi list-templates
```

### For Cursor Users

Cursor provides IDE-integrated template support:
- **Template Snippets**: Insert templates via autocomplete
- **Dynamic Templates**: Templates with live parameters
- **Project Templates**: Initialize entire projects from templates
- **Custom Templates**: Create and share templates within team

**Cursor-Specific Features**:
- `.cursor/templates/` directory for IDE templates
- Template variables with `${variable_name}` syntax
- Built-in template library

### For Claude Users

Claude excels at template design:
- **Template Architecture**: Design comprehensive template systems
- **Pattern Recognition**: Identify opportunities for templating
- **Best Practices**: Provide template design guidance
- **Customization**: Adapt templates to specific needs

### For Gemini Users

Gemini provides comprehensive template guidance:
- **Template Ecosystems**: Design template libraries
- **Multi-Language**: Templates across programming languages
- **Advanced Features**: Conditional templates, nested templates
- **Documentation**: Extensive template documentation examples

---

## Template Quality Checklist

Before adding a new template, verify:

- [ ] Template has clear, specific purpose
- [ ] All frontmatter metadata is complete
- [ ] Parameters are clearly documented
- [ ] Example is provided and accurate
- [ ] Usage instructions are clear
- [ ] Template follows naming conventions
- [ ] Template is stored in correct directory
- [ ] Template is added to `templates/index.md`
- [ ] Template has been tested by creator
- [ ] Version is set to 1.0.0
- [ ] last_updated date is current

---

## Common Pitfalls

### ❌ Template is Too Specific

**Problem**: Template only works for one specific case
**Solution**: Extract parameters to make it reusable

### ❌ Template is Too Generic

**Problem**: Template is so vague it's not useful
**Solution**: Add more structure and specific sections

### ❌ Missing Examples

**Problem**: Users don't know how to fill out the template
**Solution**: Always include a completed example

### ❌ No Usage Documentation

**Problem**: Users don't know when to use the template
**Solution**: Add clear usage section with examples

### ❌ Templates Get Outdated

**Problem**: Template content becomes obsolete
**Solution**: Quarterly review and update process

---

## Related Documents

- **File Organization**: `file_organization.md` - Where to store templates
- **Naming Conventions**: `naming_conventions.md` - How to name templates
- **Project Structure**: `directory_layout.md` - Overall project organization

---

**Last Updated**: 2025-11-19  
**Maintained By**: Global AI Rules System  
**Status**: Active  
**Applies To**: All AI assistants (Cursor, Kimi, Claude, Gemini) and all projects