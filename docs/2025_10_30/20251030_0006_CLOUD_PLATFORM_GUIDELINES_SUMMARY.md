# Cloud Platform Guidelines Plan - Summary

**Created**: 2025-10-30 00:11  
**Status**: ✅ PLAN COMPLETE - Ready for Implementation  
**Related**: Global Rule Set Decision 5 (Platform-Specific Rules)

---

## Overview

Created comprehensive plan for cloud platform guidelines based on user request:

> "Have separate guideline docs for GCP and AWS. Create the guidelines for GCP & AWS as I don't think there are any. For a GCP project that uses workflows, google cloud run functions and GCS, this is an example of the structure I've been using: solar_data_augmentation"

---

## What Was Created

### 1. Implementation Plan ✅

**File**: `docs/plans/CLOUD_PLATFORM_GUIDELINES_PLAN.md`

**Contents**:
- 6-phase implementation plan (12 hours core, 16 hours total)
- Comprehensive GCP guidelines based on solar_data_augmentation
- Comprehensive AWS guidelines for comparison
- Templates and examples
- Integration with main CURSOR.md

**Key Sections**:
- Phase 1: GCP Guidelines Foundation (3h)
- Phase 2: GCP Testing & Workflows (2h)
- Phase 3: AWS Guidelines Foundation (2.5h)
- Phase 4: AWS Deployment & Testing (2h)
- Phase 5: Templates & Examples (1.5h)
- Phase 6: Integration & Validation (1h)

### 2. TODO List ✅

**File**: `docs/plans/CLOUD_PLATFORM_GUIDELINES_TODO.md`

**Contents**:
- 35 detailed tasks (22 core + 13 additional)
- Time estimates per task
- Progress tracking structure
- Phase breakdowns
- Deliverables checklist

**Progress Tracking**:
- Task checkboxes with estimated vs actual time
- Phase progress percentages
- Overall completion tracking
- Update history section

---

## Deliverables (When Implemented)

### Documents
1. ✅ **CURSOR_CLOUD_GCP.md** - Comprehensive GCP guide
2. ✅ **CURSOR_CLOUD_AWS.md** - Comprehensive AWS guide

### Templates
3. ✅ **templates/gcp_project_structure/** - Complete GCP project
4. ✅ **templates/aws_project_structure/** - Complete AWS project

### Examples
5. ✅ **examples/gcp_cloud_function_example/** - Working GCP example
6. ✅ **examples/aws_lambda_example/** - Working AWS example

### Integration
7. ✅ Updated CURSOR.md with platform references
8. ✅ Migration guides (GCP ↔ AWS)
9. ✅ Validation checklists

---

## GCP Pattern (Documented from solar_data_augmentation)

### Project Structure

```
project-root/
├── gc/                          # All Cloud Functions
│   ├── function1/
│   │   ├── main.py             # @functions_framework.http
│   │   ├── orchestrator.py     # Main logic
│   │   ├── types.py            # ADTs
│   │   ├── request_handler.py  # Request parsing
│   │   ├── requirements.txt    # Dependencies
│   │   ├── subfolder1/         # Organized by feature
│   │   │   ├── module1.py
│   │   │   └── module2.py
│   │   └── utils/              # Utilities
│   │       ├── gcs_operations.py
│   │       └── logging_utils.py
│   ├── function2/              # Another function
│   │   └── [same structure]
│   └── common/                 # Shared across functions
│       └── logging_utils.py
├── deployment/
│   ├── functions/
│   │   ├── deploy_function1.sh
│   │   └── deploy_function2.sh
│   └── workflows/
│       └── deploy_workflows.sh
├── workflows/
│   ├── main_workflow.yaml
│   └── sub_workflow.yaml
└── tests/
    ├── function1/
    │   ├── test_module.py      # Uses sys.path.append
    │   └── test_integration.py
    ├── function2/
    │   └── test_module.py
    ├── common/
    │   ├── mock_gcs.py
    │   └── test_data_generators.py
    ├── conftest.py
    └── run_all_tests.sh
```

### Key Patterns

**1. Import Pattern** (NO package imports):
```python
# Production code (gc/function1/orchestrator.py):
from types import Result, Success, Failure
from request_handler import parse_request
from subfolder1.module1 import helper_function
from utils.gcs_operations import load_data

# Tests (tests/function1/test_module.py):
import sys
from pathlib import Path

# CRITICAL: Add function directory to path
sys.path.insert(0, str(Path(__file__).parent.parent.parent / "gc" / "function1"))

from orchestrator import orchestrate
```

**2. No __init__.py Files**:
- Cloud Functions don't support package structure
- All imports are direct or folder.module pattern
- Tests simulate this with sys.path.append

**3. Deployment Pattern**:
```bash
gcloud functions deploy function-name \
  --gen2 \
  --runtime=python311 \
  --region=australia-southeast1 \
  --source=./gc/function1 \
  --entry-point=handler_function \
  --trigger-http \
  --memory=2GB \
  --timeout=540s
```

**4. Multiple Functions**:
- Each function in separate folder under gc/
- Each has own main.py and requirements.txt
- Common code in gc/common/
- Independent deployment scripts

---

## AWS Pattern (To Be Documented)

Similar structure but with:
- **Lambda handler pattern** instead of @functions_framework
- **SAM templates** instead of deployment scripts
- **Local testing with SAM CLI** instead of sys.path.append
- **Lambda Layers** for common code instead of common/ folder

---

## Integration with Global Rule Set

This plan addresses **Decision 5** from global rule set plan:

### Decision 5: Platform-Specific Rules

**Question**: Should cloud platform rules be in main CURSOR.md or separate documents?

**Resolution**: ✅ **SEPARATE DOCUMENTS**

**Rationale**:
1. Cleaner main document (CURSOR.md stays focused on universal rules)
2. Optional (teams using only one platform don't need both)
3. Detailed (can provide extensive platform-specific guidance)
4. Maintainable (update platform docs without affecting main rules)
5. Extensible (easy to add Azure, etc. later)

**Documents**:
- CURSOR_CLOUD_GCP.md (GCP-specific)
- CURSOR_CLOUD_AWS.md (AWS-specific)
- Referenced from main CURSOR.md
- Follow Tier 1 mandatory rules (git, docs, testing, file size)

---

## Reference Project

**Location**: `/Users/johnmay/projects/clients/solvingzero/solvingzero-bat-energy-simulations/solar_data_augmentation`

**Proven in Production**:
- ✅ 5 Cloud Functions (analysis, augmentation, business_validation, reporting, validation)
- ✅ Cloud Workflows orchestration
- ✅ 368 passing tests using sys.path.append pattern
- ✅ Modular, maintainable, deployable architecture
- ✅ Real production workload handling

**This is the gold standard for GCP Python projects.**

---

## Timeline

| Phase | Duration | Focus |
|-------|----------|-------|
| Phase 1 | 3h | GCP guidelines foundation |
| Phase 2 | 2h | GCP testing & workflows |
| Phase 3 | 2.5h | AWS guidelines foundation |
| Phase 4 | 2h | AWS deployment & testing |
| Phase 5 | 1.5h | Templates & examples |
| Phase 6 | 1h | Integration & validation |
| **Core** | **12h** | Main implementation |
| Additional | 4h | Extra tasks |
| **Total** | **16h** | Complete implementation |

---

## Key Benefits

### For GCP Users

1. ✅ **Proven structure** - Based on production project
2. ✅ **Clear patterns** - Import rules, testing, deployment
3. ✅ **Multiple functions** - How to organize many functions
4. ✅ **Workflow integration** - Orchestration patterns
5. ✅ **Testing strategy** - sys.path.append pattern documented
6. ✅ **Common code** - How to share code between functions

### For AWS Users

1. ✅ **Similar patterns** - Familiar structure for GCP users
2. ✅ **Lambda best practices** - Handler patterns, layers
3. ✅ **SAM templates** - Infrastructure as code
4. ✅ **Step Functions** - Orchestration patterns
5. ✅ **Testing strategy** - Local testing with SAM
6. ✅ **Migration path** - Easy to switch from/to GCP

### For All Users

1. ✅ **Consistent** - Follow universal rules (Tier 1)
2. ✅ **Optional** - Use only what you need
3. ✅ **Detailed** - Comprehensive guidance
4. ✅ **Maintainable** - Separate from main rules
5. ✅ **Extensible** - Easy to add more platforms

---

## Example Usage

### Starting a New GCP Project

1. **Read**: CURSOR_CLOUD_GCP.md
2. **Use**: templates/gcp_project_structure/
3. **Copy**: Example function structure
4. **Follow**: Import patterns
5. **Test**: Using sys.path.append pattern
6. **Deploy**: Using deployment scripts

### Starting a New AWS Project

1. **Read**: CURSOR_CLOUD_AWS.md
2. **Use**: templates/aws_project_structure/
3. **Copy**: Example Lambda structure
4. **Follow**: Handler patterns
5. **Test**: Using SAM local
6. **Deploy**: Using SAM CLI

### Migrating GCP → AWS

1. **Read**: Migration guide (GCP → AWS)
2. **Map**: Functions → Lambdas
3. **Convert**: @functions_framework → lambda_handler
4. **Adapt**: Deployment scripts → SAM templates
5. **Update**: Tests (sys.path.append → direct imports)

---

## Dependencies

### Required
- ✅ Main CURSOR.md complete (from global rule set)
- ✅ Access to solar_data_augmentation (/Users/johnmay/projects/clients/solvingzero/...)

### Optional
- AWS account for testing AWS patterns
- GCP account for testing GCP patterns (if not already available)

---

## Next Steps

### Immediate
1. Review this plan
2. Approve plan and TODO structure
3. Add to global rule set progress tracking

### After Global Rule Set Complete
1. Begin Phase 1 (GCP Guidelines Foundation)
2. Update TODO list as tasks complete
3. Git commit after each phase
4. Create templates and examples
5. Integrate with main CURSOR.md

---

## Files Created

1. ✅ `docs/plans/CLOUD_PLATFORM_GUIDELINES_PLAN.md` (Implementation plan)
2. ✅ `docs/plans/CLOUD_PLATFORM_GUIDELINES_TODO.md` (Paired TODO list)
3. ✅ `docs/2025_10_30/20251030_0006_CLOUD_PLATFORM_GUIDELINES_SUMMARY.md` (This document)

All following proper documentation hierarchy:
- Plan in docs/plans/ (living document, NOT timestamped)
- TODO in docs/plans/ (living document, paired with plan)
- Summary in docs/2025_10_30/ (point-in-time snapshot, timestamped)

---

## Status

✅ **PLAN COMPLETE** - Ready for review and implementation  
🔄 **TODO READY** - 35 tasks defined, 16-hour estimate  
⏳ **AWAITING** - Global rule set completion before starting

---

## Cursor Update Instructions

When implementing this plan, Cursor MUST:

1. **Mark tasks complete** in CLOUD_PLATFORM_GUIDELINES_TODO.md
2. **Record actual time** for each task
3. **Update progress percentages** after each phase
4. **Add Update History entries** with timestamps
5. **Git commit** TODO updates regularly

**Example**:
```markdown
- [x] Task 1.1: Analyze structure (Estimated: 30min, Actual: 25min)
```

```
Update TODO: completed task 1.1 - Analyze structure

- Marked task 1.1 complete
- Actual time: 25min (estimated: 30min)
- Phase 1: now 20% complete

🤖 Generated with Cursor
Co-Authored-By: Claude <noreply@anthropic.com>
```

---

**Plan is ready for your review and approval!** 🎉

