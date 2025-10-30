# Auto-Detection Requirement for Cursor Guidelines

**Created**: 2025-10-30 00:15  
**Priority**: 🔥 CRITICAL - Core Requirement  
**Status**: ✅ SPECIFICATION COMPLETE  

---

## User Requirement

> "Ultimately, the requirement is for cursor to AUTO DETECT the guidelines to be used for particular stack and not have to be told all the time for each repo and tech stack, is that possible from what has been learned so far?"

**Answer**: ✅ **YES - Absolutely Possible!**

---

## How Cursor Auto-Detection Works

Cursor automatically reads and applies rules from:

1. **`.cursorrules` file** in project root (automatically detected)
2. **File type detection** (language, framework)
3. **Project structure detection** (package.json, pyproject.toml, etc.)
4. **Cloud configuration files** (SAM templates, Cloud Functions, etc.)

---

## Solution: Smart .cursorrules with Auto-Detection

### Concept

Create a **master `.cursorrules` template** that:
1. ✅ Detects tech stack automatically
2. ✅ Applies appropriate language guidelines
3. ✅ Applies appropriate platform guidelines
4. ✅ Always applies universal rules (Tier 1)
5. ✅ No manual specification needed

### How It Works

```markdown
# .cursorrules (in project root)

## 🤖 AUTO-DETECT: Tech Stack & Guidelines

### Universal Rules (ALWAYS APPLY)
@CURSOR.md#tier-1-mandatory-rules
- Git checkpoints every 30-60 min
- Documentation hierarchy (ARCHITECTURE_PLAN.md + docs/plans/ + docs/YYYY_MM_DD/)
- File size limits (250-300 lines)
- Comprehensive testing
- TODO lists updated as work completes

### Language Detection (AUTO)

**IF** package.json exists:
  → Apply @CURSOR.md#typescript-guidelines
  → Apply @typescript-fp-style-guide.md
  → Libraries: fp-ts or Effect
  → Testing: Jest/Vitest

**IF** requirements.txt OR pyproject.toml exists:
  → Apply @CURSOR.md#python-guidelines
  → Apply @python-fp-style-guide.md
  → Libraries: returns, toolz, mypy
  → Testing: pytest
  → Command: uv run python

**IF** Podfile OR Package.swift exists:
  → Apply @CURSOR.md#swift-guidelines
  → Apply @swift-fp-style-guide.md
  → Libraries: Result, Bow (optional)
  → Testing: XCTest

**IF** build.gradle.kts OR settings.gradle.kts exists:
  → Apply @CURSOR.md#kotlin-guidelines
  → Apply @kotlin-fp-style-guide.md
  → Libraries: Arrow
  → Testing: JUnit

### Platform Detection (AUTO)

**IF** gc/ folder exists AND deployment/functions/*.sh exists:
  → Apply @CURSOR_CLOUD_GCP.md
  → Pattern: Cloud Run Functions
  → Testing: sys.path.append pattern
  → Deployment: gcloud CLI

**IF** template.yaml exists (SAM template):
  → Apply @CURSOR_CLOUD_AWS.md
  → Pattern: Lambda Functions
  → Testing: SAM local
  → Deployment: SAM CLI

**IF** workflows/*.yaml exists in gc/ context:
  → Apply @CURSOR_CLOUD_GCP.md#cloud-workflows
  → Pattern: Workflow orchestration

**IF** state-machines/*.json exists:
  → Apply @CURSOR_CLOUD_AWS.md#step-functions
  → Pattern: Step Functions orchestration

### Project Type Detection (AUTO)

**IF** next.config.js exists:
  → Next.js project
  → Apply server actions patterns
  → Apply Supabase integration if detected

**IF** inngest/ folder exists:
  → Inngest functions
  → Apply TaskEither patterns

**IF** mlx OR torch in requirements.txt:
  → ML/AI project
  → Apply pure functional ML patterns
```

---

## Implementation Strategy

### Phase 1: Create Smart .cursorrules Template

**File**: `templates/.cursorrules_smart_template`

**Structure**:
```markdown
# Project: {{PROJECT_NAME}}
# Auto-Generated: {{DATE}}

## 🤖 AUTO-DETECTION ENABLED

This file enables automatic detection of:
- Programming languages
- Cloud platforms
- Frameworks
- Testing tools

Cursor will automatically apply appropriate guidelines.

## 📋 Universal Rules (Always Applied)

[Reference to CURSOR.md Tier 1 rules]

## 🔍 Detected Stack

<!-- Cursor auto-populates based on project structure -->

Language: [Auto-detected]
Platform: [Auto-detected]
Framework: [Auto-detected]

## 📚 Applied Guidelines

<!-- Cursor automatically includes relevant sections -->

### Language Guidelines
- [Auto-selected based on detection]

### Platform Guidelines  
- [Auto-selected based on detection]

### Framework Patterns
- [Auto-selected based on detection]

## 🎯 Project-Specific Overrides

<!-- Add any project-specific rules here -->

```

---

## Detection Logic

### Language Detection

```yaml
Python:
  detect_files:
    - requirements.txt
    - pyproject.toml
    - setup.py
    - Pipfile
  apply_guidelines:
    - CURSOR.md#python-guidelines
    - python-fp-style-guide.md
  libraries:
    - returns
    - toolz
    - mypy
  testing: pytest
  command: uv run python

TypeScript:
  detect_files:
    - package.json (with typescript dependency)
    - tsconfig.json
  apply_guidelines:
    - CURSOR.md#typescript-guidelines
    - typescript-fp-style-guide.md
  libraries:
    - fp-ts OR Effect
  testing: jest OR vitest
  
Swift:
  detect_files:
    - Package.swift
    - Podfile
    - *.xcodeproj
  apply_guidelines:
    - CURSOR.md#swift-guidelines
    - swift-fp-style-guide.md
  libraries:
    - Result (built-in)
    - Bow (optional)
  testing: XCTest

Kotlin:
  detect_files:
    - build.gradle.kts
    - settings.gradle.kts
  apply_guidelines:
    - CURSOR.md#kotlin-guidelines
    - kotlin-fp-style-guide.md
  libraries:
    - Arrow
  testing: JUnit
```

### Platform Detection

```yaml
Google Cloud Platform:
  detect_files:
    - gc/ folder structure
    - deployment/functions/*.sh
    - workflows/*.yaml
  detect_patterns:
    - "gcloud functions deploy" in scripts
    - "@functions_framework.http" in Python files
  apply_guidelines:
    - CURSOR_CLOUD_GCP.md
  patterns:
    - Cloud Run Functions structure
    - sys.path.append testing
    - No __init__.py files
    - Direct and folder.module imports

Amazon Web Services:
  detect_files:
    - template.yaml (SAM)
    - samconfig.toml
    - cdk.json (CDK)
  detect_patterns:
    - "lambda_handler" in Python files
    - "exports.handler" in JS files
  apply_guidelines:
    - CURSOR_CLOUD_AWS.md
  patterns:
    - Lambda function structure
    - SAM local testing
    - Lambda Layers
```

### Framework Detection

```yaml
Next.js:
  detect_files:
    - next.config.js
    - app/ OR pages/ directory
  apply_patterns:
    - Server actions with TaskEither
    - Supabase integration

Supabase:
  detect_files:
    - supabase/ folder
    - .env with SUPABASE_URL
  apply_patterns:
    - TaskEither for queries
    - Type-safe database operations

Inngest:
  detect_files:
    - inngest/ folder
    - inngest.config.ts
  apply_patterns:
    - Function composition
    - TaskEither for workflows

MLX:
  detect_files:
    - requirements.txt with "mlx"
  apply_patterns:
    - Pure functional ML
    - Result types for training loops
```

---

## Example: Auto-Detection in Action

### Scenario 1: Python + GCP Project

**Project Structure**:
```
my-project/
├── .cursorrules                 # Smart template
├── requirements.txt             # ✅ Detected: Python
├── gc/                         # ✅ Detected: GCP
│   └── my_function/
│       ├── main.py
│       └── requirements.txt
├── deployment/
│   └── functions/
│       └── deploy.sh           # ✅ Detected: Cloud Functions
└── workflows/
    └── main_workflow.yaml      # ✅ Detected: Cloud Workflows
```

**Cursor Automatically Applies**:
1. ✅ Universal rules (Tier 1 from CURSOR.md)
2. ✅ Python guidelines (returns, toolz, mypy)
3. ✅ GCP guidelines (CURSOR_CLOUD_GCP.md)
4. ✅ Cloud Functions patterns (sys.path.append, no __init__.py)
5. ✅ Cloud Workflows patterns

**User does NOT need to specify anything!**

---

### Scenario 2: TypeScript + Next.js + Supabase + AWS

**Project Structure**:
```
my-app/
├── .cursorrules                 # Smart template
├── package.json                 # ✅ Detected: TypeScript
├── next.config.js              # ✅ Detected: Next.js
├── tsconfig.json
├── supabase/                   # ✅ Detected: Supabase
├── template.yaml               # ✅ Detected: AWS SAM
└── functions/
    └── handler.ts              # ✅ Detected: Lambda
```

**Cursor Automatically Applies**:
1. ✅ Universal rules (Tier 1)
2. ✅ TypeScript guidelines (fp-ts or Effect)
3. ✅ Next.js patterns (server actions with TaskEither)
4. ✅ Supabase patterns (type-safe queries)
5. ✅ AWS Lambda guidelines (CURSOR_CLOUD_AWS.md)

**Again, NO manual specification needed!**

---

### Scenario 3: Swift iOS Project

**Project Structure**:
```
MyApp/
├── .cursorrules                 # Smart template
├── Package.swift               # ✅ Detected: Swift
├── MyApp.xcodeproj/
└── Sources/
    └── MyApp/
```

**Cursor Automatically Applies**:
1. ✅ Universal rules (Tier 1)
2. ✅ Swift guidelines (Result type, pattern matching)
3. ✅ iOS patterns (SwiftUI integration)

---

## Implementation in Global Rule Set Plan

### Update Phase 2: Add Auto-Detection Section

**New Section in CURSOR.md**:

```markdown
## 🤖 AUTO-DETECTION & PROJECT SETUP

### How Cursor Detects Your Stack

Cursor automatically detects your tech stack by analyzing:
1. Project files (package.json, requirements.txt, etc.)
2. Folder structure (gc/, functions/, etc.)
3. Configuration files (SAM templates, Cloud configs, etc.)

**No manual configuration needed!**

### Setting Up a New Project

1. Create project directory
2. Copy `.cursorrules_smart_template` to `.cursorrules`
3. Add your tech stack files (package.json, etc.)
4. **Cursor automatically detects and applies guidelines!**

### What Gets Auto-Detected

| Stack Component | Detection Method | Applied Guidelines |
|----------------|------------------|-------------------|
| Python | requirements.txt, pyproject.toml | Python FP guidelines |
| TypeScript | package.json, tsconfig.json | TypeScript FP guidelines |
| Swift | Package.swift, *.xcodeproj | Swift FP guidelines |
| Kotlin | build.gradle.kts | Kotlin FP guidelines |
| GCP | gc/ folder, gcloud scripts | CURSOR_CLOUD_GCP.md |
| AWS | template.yaml, SAM config | CURSOR_CLOUD_AWS.md |
| Next.js | next.config.js | Next.js patterns |
| Supabase | supabase/ folder | Supabase patterns |

### Override Detection

If auto-detection is wrong, specify in `.cursorrules`:

```markdown
## Manual Override
Language: Python (using uv)
Platform: GCP (Cloud Run Functions)
Testing: pytest with sys.path.append
```
```

---

## Benefits of Auto-Detection

### 1. Zero Configuration ✅
- Drop `.cursorrules` template in project
- Cursor detects stack automatically
- Appropriate guidelines applied

### 2. Consistent Application ✅
- Same guidelines applied across all projects
- No forgetting to mention rules
- Automatic enforcement

### 3. New Team Members ✅
- No onboarding needed for rules
- Cursor knows what to do
- Consistent code from day one

### 4. Multi-Language Projects ✅
- Detects multiple languages
- Applies appropriate rules per file type
- Seamless polyglot support

### 5. Platform Agnostic ✅
- Works with GCP, AWS, Azure, etc.
- Detects based on structure
- Correct patterns automatically

---

## Template Structure

### Master Template: `.cursorrules_smart_template`

```markdown
# {{PROJECT_NAME}} - Cursor Rules

**Auto-Detection**: ENABLED  
**Last Updated**: {{DATE}}

---

## 🤖 AUTO-DETECTED STACK

<!-- Cursor populates this automatically -->

**Language**: [Auto-detected from project files]  
**Platform**: [Auto-detected from structure/config]  
**Framework**: [Auto-detected from dependencies]

---

## 📋 MANDATORY RULES (Always Applied)

### From CURSOR.md (Tier 1)

1. ✅ **Git Checkpoints** - Every 30-60 min
2. ✅ **Documentation Hierarchy** - ARCHITECTURE_PLAN.md + docs/plans/ + docs/YYYY_MM_DD/
3. ✅ **TODO Lists** - Updated as work completes
4. ✅ **File Size Limits** - 250-300 lines max
5. ✅ **Comprehensive Testing** - All code tested

---

## 🎯 LANGUAGE GUIDELINES (Auto-Applied)

<!-- Cursor includes appropriate language guide -->

**Detection Logic**:
- IF requirements.txt → Apply Python guidelines
- IF package.json → Apply TypeScript guidelines
- IF Package.swift → Apply Swift guidelines
- IF build.gradle.kts → Apply Kotlin guidelines

**Applied**: [Language-specific guidelines auto-included]

---

## ☁️ PLATFORM GUIDELINES (Auto-Applied)

<!-- Cursor includes appropriate platform guide -->

**Detection Logic**:
- IF gc/ folder + gcloud → Apply GCP guidelines
- IF template.yaml → Apply AWS guidelines
- IF azure-pipelines.yml → Apply Azure guidelines

**Applied**: [Platform-specific guidelines auto-included]

---

## 🛠️ PROJECT-SPECIFIC OVERRIDES

<!-- Add custom project rules here if needed -->

_No overrides specified - using auto-detected defaults_

---

## 📖 FULL DOCUMENTATION

For complete guidelines, see:
- Universal Rules: @CURSOR.md
- Language Guide: @[language]-fp-style-guide.md
- Platform Guide: @CURSOR_CLOUD_[platform].md

---

**Cursor will automatically apply all relevant guidelines based on project structure!** ✨
```

---

## Updates Needed to Plans

### Global Rule Set Plan

**Add to Phase 2 (Core Document Creation)**:
- [ ] **2.16**: Write Auto-Detection section (30 min)
  - How detection works
  - Detection logic tables
  - Override instructions
  - Benefits explanation

**Add to Phase 4 (Templates & Examples)**:
- [ ] **4.13**: Create `.cursorrules_smart_template` (45 min)
  - Auto-detection logic
  - Language detection
  - Platform detection
  - Framework detection
  - Override mechanism

- [ ] **4.14**: Create auto-detection examples (30 min)
  - Python + GCP example
  - TypeScript + AWS example
  - Swift iOS example
  - Polyglot project example

### Cloud Platform Guidelines Plan

**Add to Phase 1 (GCP Guidelines)**:
- [ ] **1.6**: Write GCP detection patterns (20 min)
  - gc/ folder detection
  - gcloud command detection
  - Cloud Workflows detection

**Add to Phase 3 (AWS Guidelines)**:
- [ ] **3.5**: Write AWS detection patterns (20 min)
  - SAM template detection
  - Lambda handler detection
  - Step Functions detection

---

## Timeline Impact

**Additional Time Needed**:
- Global Rule Set: +2 hours (auto-detection section + smart template + examples)
- Cloud Platform Guidelines: +40 min (detection patterns)

**Updated Totals**:
- Global Rule Set: 12 hours (was 10h)
- Platform Guidelines: 16.67 hours (was 16h)
- **Grand Total**: 28.67 hours (was 26h)

---

## Validation

### How to Test Auto-Detection

1. **Create test projects** with different stacks
2. **Copy .cursorrules_smart_template** to each
3. **Ask Cursor to analyze** the project
4. **Verify correct guidelines** are applied

### Example Test Cases

```bash
# Test 1: Python + GCP
mkdir test-python-gcp
cd test-python-gcp
touch requirements.txt
mkdir -p gc/my_function
cp .cursorrules_smart_template .cursorrules
# Ask Cursor: "What guidelines should I follow?"
# Expected: Python + GCP guidelines

# Test 2: TypeScript + AWS
mkdir test-ts-aws
cd test-ts-aws
npm init -y
touch template.yaml
cp .cursorrules_smart_template .cursorrules
# Ask Cursor: "What guidelines should I follow?"
# Expected: TypeScript + AWS guidelines

# Test 3: Polyglot
mkdir test-polyglot
cd test-polyglot
touch requirements.txt package.json
cp .cursorrules_smart_template .cursorrules
# Ask Cursor: "What guidelines should I follow?"
# Expected: Python + TypeScript guidelines
```

---

## Summary

### ✅ Answer to Your Question

**YES - Cursor CAN auto-detect and apply guidelines automatically!**

**How**:
1. ✅ Create smart `.cursorrules` template
2. ✅ Cursor detects files/structure
3. ✅ Automatically applies appropriate guidelines
4. ✅ NO manual specification needed

**What Gets Detected**:
- ✅ Programming languages (Python, TypeScript, Swift, Kotlin)
- ✅ Cloud platforms (GCP, AWS, Azure)
- ✅ Frameworks (Next.js, Supabase, Inngest, MLX)
- ✅ Testing tools (pytest, jest, XCTest)

**Benefits**:
- ✅ Zero configuration for developers
- ✅ Consistent application across projects
- ✅ New team members productive immediately
- ✅ Multi-language projects supported
- ✅ Platform-agnostic

**Implementation**:
- Add auto-detection section to CURSOR.md
- Create `.cursorrules_smart_template`
- Add detection logic to platform guidelines
- Provide examples for common stacks

---

## Status

✅ **REQUIREMENT UNDERSTOOD**  
✅ **SOLUTION DESIGNED**  
✅ **IMPLEMENTATION PLAN READY**  
⏳ **AWAITING APPROVAL TO ADD TO PLANS**

---

**This addresses your core requirement: NO MORE TELLING CURSOR WHAT TO DO - IT DETECTS AUTOMATICALLY!** 🎉

