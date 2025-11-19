---
title: Project Directory Layout Standards
category: universal_rules
type: project_structure
applies_to: all
version: 1.0.0
last_updated: 2025-11-19
---

# Project Directory Layout Standards

**Status**: MANDATORY - All projects must follow these directory layout standards

Universal directory structure requirements applicable to all projects, regardless of technology stack, programming language, or AI assistant used (Cursor, Kimi, Claude, Gemini).

---

## Core Principles

### 1. Consistency Across Projects

**Rule**: All projects must follow a predictable, consistent directory structure.

**Benefits**:
- Developers can navigate any project instantly
- AI assistants understand project layout automatically
- Onboarding new team members is faster
- Tools and scripts work across projects

**Standard Structure**:
```
project/
├── docs/                    # Documentation
├── src/                     # Source code
├── tests/                   # Test files
├── scripts/                 # Automation scripts
├── config/                  # Configuration files
├── assets/                  # Static assets
├── examples/                # Usage examples
├── tools/                   # Development tools
└── README.md               # Project overview
```

---

### 2. Separation of Concerns

**Rule**: Different types of files belong in different directories. No mixing concerns.

**Enforcement**:
- Source code **only** in `src/` (not in root)
- Tests **only** in `tests/` (not mixed with source)
- Documentation **only** in `docs/` (not scattered)
- Scripts **only** in `scripts/` (not random files)
- Configuration **only** in `config/` (not hardcoded)

**Anti-Pattern**:
```
project/                    # ❌ BAD: Everything mixed
├── main.py
├── test_main.py
├── config.json
├── README.md
├── deploy.sh
└── logo.png
```

**Correct Pattern**:
```
project/                    # ✅ GOOD: Clear separation
├── src/
│   └── main.py
├── tests/
│   └── test_main.py
├── config/
│   └── config.json
├── docs/
│   └── README.md
├── scripts/
│   └── deploy.sh
└── assets/
    └── logo.png
```

---

### 3. Flat is Better Than Nested (Within Reason)

**Rule**: Prefer shallow directory structures. Maximum 3-4 levels deep.

**Guidelines**:
- **Maximum depth**: 4 levels (e.g., `src/domain/models/user.py`)
- **Preferred depth**: 2-3 levels (e.g., `src/models/user.py`)
- **Exception**: Large monorepos may need deeper nesting

**Why**: Deep nesting makes imports long, paths hard to read, and navigation tedious.

---

## Standard Directory Structure

### Root Level Files

**Required**:
- `README.md` - Project overview, setup, usage
- `.gitignore` - Git ignore patterns
- `LICENSE` - Project license
- `CONTRIBUTING.md` - How to contribute

**Optional but Recommended**:
- `CHANGELOG.md` - Version history
- `CODE_OF_CONDUCT.md` - Community guidelines
- `SECURITY.md` - Security policy
- `Makefile` or `justfile` - Common tasks

---

### `src/` - Source Code

**Purpose**: All production source code.

**Structure by Language**:

#### Python
```
src/
├── __init__.py
├── main.py                 # Entry point
├── app.py                  # Application factory
├── domain/                 # Domain models
│   ├── __init__.py
│   ├── models/
│   └── services/
├── infrastructure/         # External concerns
│   ├── __init__.py
│   ├── database.py
│   └── api/
├── use_cases/             # Business logic
│   ├── __init__.py
│   └── user/
└── interfaces/            # Adapters (CLI, web)
    ├── __init__.py
    └── cli.py
```

#### TypeScript
```
src/
├── index.ts               # Entry point
├── app.ts                 # Application setup
├── domain/                # Domain layer
│   ├── models/
│   └── services/
├── infrastructure/        # External concerns
│   ├── database.ts
│   └── api/
├── use_cases/            # Application layer
│   └── user/
└── interfaces/           # Presentation layer
    ├── cli/
    └── web/
```

#### Rust
```
src/
├── main.rs               # Binary entry point
├── lib.rs                # Library root
├── domain/               # Domain models
│   ├── mod.rs
│   └── models.rs
├── infrastructure/       # External adapters
│   ├── mod.rs
│   └── database.rs
├── use_cases/           # Business logic
│   ├── mod.rs
│   └── user.rs
└── interfaces/          # Delivery mechanisms
    ├── mod.rs
    └── cli.rs
```

---

### `tests/` - Test Files

**Purpose**: All test code (unit, integration, e2e).

**Structure**:
```
tests/
├── unit/                 # Unit tests
│   ├── test_domain.py
│   ├── test_services.py
│   └── test_utils.py
├── integration/          # Integration tests
│   ├── test_database.py
│   ├── test_api.py
│   └── test_workflows.py
├── e2e/                  # End-to-end tests
│   └── test_user_journey.py
└── conftest.py          # Test fixtures (pytest)
```

**Rule**: Mirror `src/` structure in `tests/unit/`.

```
src/
  domain/
    user.py
tests/
  unit/
    domain/
      test_user.py  # Mirrors src structure
```

---

### `docs/` - Documentation

**Purpose**: All project documentation.

**Structure**:
```
docs/
├── README.md            # Main documentation
├── ARCHITECTURE.md      # Architecture decisions
├── API.md              # API documentation
├── DEPLOYMENT.md       # Deployment guide
├── YYYY_MM_DD/         # Daily work logs
│   ├── 20251119_0001_TASK_DESCRIPTION.md
│   └── 20251119_0002_ANOTHER_TASK.md
├── plans/              # Strategic plans
│   ├── FEATURE_PLAN.md
│   └── FEATURE_TODO.md
└── examples/           # Usage examples
    └── basic_usage.py
```

**Daily Work Folders** (Mandatory):
- Format: `docs/YYYY_MM_DD/`
- Files: `YYYYMMDD_NNNN_DESCRIPTIVE_NAME.md`
- NNNN = 4-digit sequence (0000, 0001, 0002, ...)
- Example: `docs/20251119/20251119_0000_MCP_IMPLEMENTATION.md`

---

### `config/` - Configuration

**Purpose**: All configuration files.

**Structure**:
```
config/
├── development/         # Development environment
│   ├── database.json
│   └── api.json
├── staging/            # Staging environment
│   ├── database.json
│   └── api.json
└── production/         # Production environment
    ├── database.json
    └── api.json
```

**Alternative** (single-file configs):
```
config/
├── config.yaml         # Default configuration
├── config.dev.yaml     # Development override
├── config.staging.yaml # Staging override
└── config.prod.yaml    # Production override
```

**Rule**: No configuration in source code. All config externalized.

---

### `scripts/` - Automation Scripts

**Purpose**: Scripts for development, testing, deployment.

**Structure**:
```
scripts/
├── setup.sh            # Initial setup
├── test.sh             # Run all tests
├── lint.sh             # Run linting
├── format.sh           # Format code
├── build.sh            # Build project
├── deploy.sh           # Deploy to environment
└── ci/                 # CI/CD scripts
    ├── before_install.sh
    ├── install.sh
    └── after_success.sh
```

**Requirements**:
- Scripts must be executable (`chmod +x`)
- Scripts must have shebang (`#!/bin/bash`)
- Scripts must be documented (usage comments)
- Scripts must be idempotent (safe to run multiple times)

---

### `assets/` - Static Assets

**Purpose**: Static files (images, fonts, data).

**Structure**:
```
assets/
├── images/             # Images and graphics
│   ├── logo.png
│   └── diagram.png
├── fonts/              # Font files
│   └── custom-font.woff2
└── data/               # Static data files
    └── sample-data.json
```

**Rule**: No source code in `assets/`. Only static files.

---

### `examples/` - Usage Examples

**Purpose**: Example code showing how to use the project.

**Structure**:
```
examples/
├── basic/              # Basic usage examples
│   └── basic_usage.py
├── advanced/           # Advanced patterns
│   └── advanced_patterns.py
└── integration/        # Integration examples
    └── with_database.py
```

**Requirements**:
- Examples must be runnable
- Examples must be documented
- Examples must be tested (if possible)
- Examples must use realistic scenarios

---

### `tools/` - Development Tools

**Purpose**: Internal tools for development and maintenance.

**Structure**:
```
tools/
├── generate_docs.sh    # Documentation generator
├── benchmark.py        # Performance benchmarking
├── analyze_logs.py     # Log analysis tool
└── cleanup.sh          # Cleanup script
```

**Rule**: Tools are for developers, not for production use.

---

## Language-Specific Variations

### Python Projects

**Standard Layout**:
```
project/
├── src/
│   └── package_name/
│       ├── __init__.py
│       ├── __main__.py
│       └── ...
├── tests/
│   ├── __init__.py
│   └── test_package_name/
├── docs/
├── pyproject.toml        # Modern Python packaging
├── requirements.txt      # Dependencies
├── requirements-dev.txt  # Dev dependencies
└── setup.py             # Package setup (legacy)
```

**Note**: Use `src/` layout (not flat layout) to avoid import issues.

---

### TypeScript/JavaScript Projects

**Standard Layout**:
```
project/
├── src/
│   ├── index.ts
│   └── ...
├── tests/
│   └── ...
├── docs/
├── package.json          # Dependencies and scripts
├── tsconfig.json         # TypeScript configuration
├── jest.config.js        # Test configuration
├── .eslintrc.js          # Linting configuration
└── dist/                 # Compiled output (gitignored)
```

**Note**: `dist/` or `build/` should be in `.gitignore`.

---

### Rust Projects

**Standard Layout** (Cargo default):
```
project/
├── src/
│   ├── main.rs           # Binary entry point
│   └── lib.rs            # Library root
├── tests/                # Integration tests
├── benches/              # Benchmarks
├── examples/             # Example binaries
├── docs/
├── Cargo.toml            # Dependencies and metadata
└── target/               # Build output (gitignored)
```

**Note**: Follow Cargo conventions. `src/bin/` for multiple binaries.

---

### Multi-Language Projects

**Standard Layout**:
```
project/
├── docs/
├── scripts/
├── config/
├── python/               # Python-specific code
│   ├── src/
│   ├── tests/
│   └── pyproject.toml
├── typescript/           # TypeScript-specific code
│   ├── src/
│   ├── tests/
│   └── package.json
└── rust/                 # Rust-specific code
    ├── src/
    ├── tests/
    └── Cargo.toml
```

---

## File Organization Rules

### 1. Maximum File Size

**Rule**: No file exceeds 250-300 lines.

**Rationale**:
- Small files are easier to understand
- Small files are easier to test
- Small files enable parallel development
- Small files improve compilation/build speed

**When to Split**:
- File > 250 lines → Consider splitting
- File > 300 lines → Must split
- Multiple classes/modules in one file → Split
- Mixed abstraction levels → Split

---

### 2. One Concept Per File

**Rule**: Each file should have one primary responsibility.

**Good**:
```
src/
  user.py           # User model and operations
  order.py          # Order model and operations
  payment.py        # Payment processing
```

**Bad**:
```
src/
  models.py         # User, Order, Payment all mixed
```

---

### 3. Co-location of Related Files

**Rule**: Keep related files close together.

**Good**:
```
src/
  user/
    __init__.py
    model.py        # User model
    service.py      # User business logic
    repository.py   # User data access
    test_model.py   # Tests co-located
```

**Bad**:
```
src/
  models/
    user.py         # User model far from related files
  services/
    user_service.py # Hard to find related files
  repositories:
    user_repo.py    # Scattered across directories
```

---

## Naming Conventions

### Directories

- **Lowercase**: `src/`, `docs/`, `tests/` (not `Src/`, `Docs/`)
- **Hyphens for multi-word**: `test-data/`, `build-output/` (not `test_data/`)
- **No spaces**: `my-project/` (not `my project/`)

### Files

- **Source code**: Follow language conventions
  - Python: `snake_case.py`
  - TypeScript: `camelCase.ts` or `PascalCase.ts` (for classes)
  - Rust: `snake_case.rs`
- **Tests**: `test_*.py`, `*.test.ts`, `*_test.rs`
- **Documentation**: `README.md`, `ARCHITECTURE.md`, `CHANGELOG.md`
- **Config**: `config.yaml`, `settings.json`

---

## Examples by Project Type

### 1. Python Library

```
mylibrary/
├── src/
│   └── mylibrary/
│       ├── __init__.py
│       ├── __main__.py
│       └── ...
├── tests/
│   ├── __init__.py
│   └── test_mylibrary/
├── docs/
│   ├── README.md
│   └── examples/
│       └── basic_usage.py
├── pyproject.toml
├── LICENSE
└── README.md
```

### 2. TypeScript Web App

```
myapp/
├── src/
│   ├── index.ts
│   ├── app.ts
│   ├── routes/
│   └── models/
├── tests/
│   ├── unit/
│   └── e2e/
├── public/
│   └── index.html
├── docs/
├── package.json
├── tsconfig.json
└── README.md
```

### 3. Rust CLI Tool

```
mytool/
├── src/
│   ├── main.rs
│   ├── cli.rs
│   └── commands/
├── tests/
│   └── integration_tests.rs
├── examples/
│   └── basic.rs
├── docs/
├── Cargo.toml
└── README.md
```

### 4. Documentation-Heavy Project

```
rules/
├── docs/
│   ├── YYYY_MM_DD/
│   │   ├── 20251119_0000_TASK.md
│   │   └── 20251119_0001_SUMMARY.md
│   └── plans/
│       ├── FEATURE_PLAN.md
│       └── FEATURE_TODO.md
├── templates/
├── examples/
├── scripts/
│   └── migrate.sh
├── README.md
└── CONTRIBUTING.md
```

---

## Anti-Patterns (DO NOT DO)

### ❌ Mixed Concerns

```
project/
├── main.py                 # ❌ Mixes domain, infra, and UI
├── config.py              # ❌ Config mixed with logic
├── test.py                # ❌ Tests mixed with source
└── README.md
```

### ❌ Deep Nesting

```
project/
└── src/
    └── com/
        └── company/
            └── project/
                └── module/
                    └── submodule/
                        └── component/
                            └── file.py  # ❌ Too deep!
```

### ❌ Flat Structure

```
project/
├── user_model.py
├── user_service.py
├── user_repository.py
├── order_model.py
├── order_service.py
├── order_repository.py
├── payment_model.py
├── payment_service.py
└── payment_repository.py  # ❌ Too flat, hard to navigate!
```

### ❌ Source in Root

```
project/
├── main.py                # ❌ Source in root
├── utils.py              # ❌ Hard to distinguish from config
├── config.json
└── tests/
    └── test_main.py
```

---

## Tool-Specific Notes

### For Kimi Users

Kimi provides enhanced project structure management:
- **Parallel File Operations**: Work on multiple directories simultaneously
- **Structure Validation**: Verify directory layout follows standards
- **Migration Scripts**: Generate scripts to restructure projects
- **SetTodoList Integration**: Track refactoring tasks

**Kimi-Specific Commands**:
```bash
# Validate project structure
kimi validate-structure --standard=universal

# Generate directory layout for new project
kimi generate-structure --type=python-library

# Check for violations
kimi check-structure --max-depth=4 --max-file-size=300
```

### For Cursor Users

Cursor provides IDE-integrated structure management:
- **Project View**: Visualize directory structure
- **Refactoring Tools**: Move/rename files with automatic updates
- **Structure Templates**: Generate standard layouts
- **Violation Warnings**: Highlight structure violations inline

**Cursor-Specific Features**:
- Right-click → "Check Directory Structure"
- Command palette → "Validate Project Layout"
- Inline warnings for files > 300 lines

### For Claude Users

Claude excels at project structure design:
- **Architecture Planning**: Design optimal directory layouts
- **Refactoring Strategy**: Plan large-scale restructures
- **Best Practices**: Detailed structure guidance
- **Migration Planning**: Step-by-step restructure plans

### For Gemini Users

Gemini provides comprehensive structure guidance:
- **Multi-Language**: Structure patterns across languages
+- **Monorepo Guidance**: Large project organization
+- **Tool Comparison**: Different structure approaches
+- **Best Practices**: Detailed explanations

---

## Migration Strategy

### Existing Projects

If your project doesn't follow this structure:

**Phase 1: Documentation** (1-2 hours)
1. Create `docs/structure_migration_plan.md`
2. Document current structure
3. Identify violations
4. Plan migration steps

**Phase 2: Core Restructure** (2-4 hours)
1. Create new directory structure
2. Move files to correct locations
3. Update imports and references
4. Update build configuration

**Phase 3: Validation** (1 hour)
1. Verify all files in correct locations
2. Run tests
3. Update documentation
4. Commit with descriptive message

**Phase 4: Team Onboarding** (30 minutes)
1. Document new structure
2. Update onboarding guide
3. Announce changes to team

---

## Validation Checklist

Before considering structure complete:

- [ ] All source code in `src/` directory
- [ ] All tests in `tests/` directory
- [ ] All docs in `docs/` directory
- [ ] Configuration externalized to `config/`
- [ ] No files in root (except README, LICENSE, etc.)
- [ ] Maximum directory depth: 4 levels
- [ ] Maximum file size: 300 lines
- [ ] Clear separation of concerns
- [ ] Consistent naming conventions
- [ ] README.md exists and is up-to-date
- [ ] .gitignore properly configured

---

## Related Documents

- **File Size Limits**: `file_size_limits.md` - Maximum file size requirements
- **Template Standards**: `template_standards.md` - Standards for project templates
- **Naming Conventions**: `../documentation/naming_conventions.md` - File naming rules
- **Git Rules**: `../git/git_checkpoint_rules.md` - When to commit structure changes

---

**Last Updated**: 2025-11-19  
**Maintained By**: Global AI Rules System  
**Status**: Active  
**Applies To**: All AI assistants (Cursor, Kimi, Claude, Gemini) and all programming languages