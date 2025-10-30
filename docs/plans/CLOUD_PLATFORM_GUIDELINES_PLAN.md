# Cloud Platform Guidelines Implementation Plan

**Status**: 🔄 PLANNED  
**Priority**: Related to Global Rule Set (Decision 5)  
**Estimated Effort**: 8-10 hours  
**Target**: Create comprehensive GCP and AWS guidelines for Cursor

---

## Overview

Create separate, comprehensive cloud platform guidelines for Google Cloud Platform (GCP) and Amazon Web Services (AWS). These guidelines will be referenced from the main CURSOR.md but exist as standalone documents.

**Based on proven patterns** from production project:
- `/Users/johnmay/projects/clients/solvingzero/solvingzero-bat-energy-simulations/solar_data_augmentation`

### Success Criteria

- ✅ CURSOR_CLOUD_GCP.md complete with Python examples
- ✅ CURSOR_CLOUD_AWS.md complete with Python/Node.js examples
- ✅ Project structure templates for both platforms
- ✅ Deployment patterns documented
- ✅ Testing strategies defined
- ✅ All patterns tested and validated

---

## Prerequisites

- ✅ Main CURSOR.md complete (from global rule set plan)
- ✅ Access to solar_data_augmentation reference project
- ✅ Understanding of Cloud Run Functions (GCP)
- ✅ Understanding of Lambda Functions (AWS)

---

## Phase 1: GCP Guidelines Foundation (3 hours)

### Goal

Create comprehensive GCP guidelines based on solar_data_augmentation proven pattern.

### Tasks

**1.1: Analyze solar_data_augmentation Structure** (30 min)
- Document folder organization pattern
- Extract project structure template
- Identify key patterns (gc/, deployment/, workflows/, tests/)
- Document import patterns (sys.path.append)

**1.2: Create CURSOR_CLOUD_GCP.md Skeleton** (30 min)
- Table of contents
- Section headers
- Integration with main CURSOR.md

**1.3: Write Project Structure Section** (45 min)
- Root-level organization (gc/, deployment/, workflows/, tests/)
- Function folder structure (main.py, orchestrator, utils/, subfolder/)
- Multiple functions pattern
- Common code sharing strategies

**1.4: Write Cloud Run Functions Section** (45 min)
- Entry point pattern (@functions_framework.http)
- Import rules (direct and folder.module patterns)
- File size limits application
- No __init__.py files rule
- requirements.txt per function

**1.5: Write Deployment Section** (30 min)
- Deployment script organization (deployment/functions/, deployment/workflows/)
- gcloud commands and parameters
- Environment variables
- Memory and timeout configuration
- Multi-function deployment strategies

---

## Phase 2: GCP Testing & Workflows (2 hours)

### Goal

Document testing strategies and Cloud Workflows integration.

### Tasks

**2.1: Write Testing Strategy Section** (45 min)
- sys.path.append() pattern for Cloud Functions
- Test directory structure (tests/function_name/)
- conftest.py usage
- Mock patterns for GCS, HTTP
- Integration testing approach

**2.2: Write Cloud Workflows Section** (45 min)
- Workflow YAML structure
- Sub-workflow patterns
- Function orchestration
- Error handling
- Parameter passing patterns

**2.3: Write GCS Integration Section** (30 min)
- Storage patterns
- Artifact organization
- Path construction
- Access patterns
- Secret Manager integration

---

## Phase 3: AWS Guidelines Foundation (2.5 hours)

### Goal

Create comprehensive AWS guidelines for Lambda, S3, and Step Functions.

### Tasks

**3.1: Research AWS Patterns** (30 min)
- Lambda function structure
- SAM/CDK patterns
- Testing strategies
- Common project layouts

**3.2: Create CURSOR_CLOUD_AWS.md Skeleton** (30 min)
- Table of contents
- Section headers
- Integration with main CURSOR.md

**3.3: Write Project Structure Section** (45 min)
- Root-level organization (functions/, layers/, deployment/, tests/)
- Lambda function structure
- Multiple functions pattern
- Layer usage for common code

**3.4: Write Lambda Functions Section** (45 min)
- Handler pattern (lambda_handler)
- Import rules
- Runtime options (Python, Node.js)
- File size limits
- requirements.txt vs package.json

---

## Phase 4: AWS Deployment & Testing (2 hours)

### Goal

Document AWS deployment and testing strategies.

### Tasks

**4.1: Write Deployment Section** (45 min)
- SAM template patterns
- CDK patterns (optional)
- Deployment scripts
- Environment variables
- IAM roles and permissions

**4.2: Write Testing Strategy Section** (45 min)
- Test structure for Lambda
- Mock patterns (S3, DynamoDB, etc.)
- Local testing with SAM
- Integration testing
- Unit testing patterns

**4.3: Write S3 Integration Section** (30 min)
- Storage patterns
- Bucket organization
- Access patterns
- Event triggers

**4.4: Write Step Functions Section** (optional, 30 min)
- Step Functions vs Cloud Workflows
- State machine patterns
- Lambda orchestration
- Error handling

---

## Phase 5: Templates & Examples (1.5 hours)

### Goal

Create ready-to-use templates and examples.

### Tasks

**5.1: Create GCP Project Template** (30 min)
- Complete directory structure
- Example function with main.py, orchestrator, requirements.txt
- Example deployment script
- Example test with sys.path.append
- Example workflow YAML

**5.2: Create AWS Project Template** (30 min)
- Complete directory structure
- Example Lambda function
- Example SAM template
- Example deployment script
- Example test

**5.3: Create Migration Guides** (30 min)
- GCP → AWS migration guide
- AWS → GCP migration guide
- Pattern equivalences
- Common pitfalls

---

## Phase 6: Integration & Validation (1 hour)

### Goal

Integrate with main rule set and validate.

### Tasks

**6.1: Update CURSOR.md References** (15 min)
- Add Decision 5 result (separate platform docs)
- Add links to CURSOR_CLOUD_GCP.md and CURSOR_CLOUD_AWS.md
- Update table of contents

**6.2: Create Validation Checklist** (15 min)
- GCP project validation steps
- AWS project validation steps
- Common issues checklist

**6.3: Review & Polish** (30 min)
- Proofread all documents
- Check internal links
- Verify examples
- Ensure consistency

---

## Document Structure

### CURSOR_CLOUD_GCP.md

```markdown
# Cloud Platform Guidelines - Google Cloud Platform (GCP)

## Overview
- When to use these guidelines
- Integration with main CURSOR.md

## Project Structure
### Root Organization
- gc/ (Cloud Functions)
- deployment/ (Scripts)
- workflows/ (Workflow YAMLs)
- tests/ (Tests with sys.path.append)

### Function Folder Structure
- main.py (entry point)
- orchestrator.py (main logic)
- types.py (ADTs)
- subfolder/ (organized by feature)
- utils/ (utilities)
- requirements.txt (dependencies)

## Cloud Run Functions (Python)
### Entry Point Pattern
### Import Rules
### No __init__.py Files
### File Organization
### Multiple Functions
### Common Code Sharing

## Deployment
### Deployment Scripts
### gcloud Commands
### Environment Variables
### Configuration
### Multi-Function Deployment

## Testing
### sys.path.append() Pattern
### Test Directory Structure
### Mocking GCS/HTTP
### Integration Testing
### Running Tests

## Cloud Workflows
### Workflow Structure
### Sub-Workflows
### Function Orchestration
### Error Handling
### Parameter Passing

## Google Cloud Storage
### Storage Patterns
### Path Construction
### Access Patterns
### Artifact Organization

## Secret Manager
### Configuration
### Access Patterns
### Best Practices

## Common Patterns
### Multi-Stage Workflows
### Error Handling
### Logging
### Monitoring

## Troubleshooting
### Import Errors
### Deployment Issues
### Testing Issues

## Examples
### Complete Project Structure
### Example Function
### Example Test
### Example Deployment Script
### Example Workflow
```

### CURSOR_CLOUD_AWS.md

```markdown
# Cloud Platform Guidelines - Amazon Web Services (AWS)

## Overview
- When to use these guidelines
- Integration with main CURSOR.md

## Project Structure
### Root Organization
- functions/ (Lambda Functions)
- layers/ (Lambda Layers)
- deployment/ (SAM/CDK)
- tests/ (Tests)

### Lambda Function Structure
- handler.py or index.js
- requirements.txt or package.json
- Function organization

## Lambda Functions
### Handler Pattern
### Import Rules
### Runtimes (Python, Node.js)
### File Organization
### Multiple Functions
### Layers for Common Code

## Deployment
### SAM Templates
### CDK Patterns (optional)
### Deployment Scripts
### Environment Variables
### IAM Roles

## Testing
### Test Structure
### Local Testing (SAM)
### Mocking (S3, DynamoDB, etc.)
### Integration Testing
### Unit Testing

## S3 Integration
### Storage Patterns
### Bucket Organization
### Access Patterns
### Event Triggers

## Step Functions (Optional)
### State Machines
### Lambda Orchestration
### Error Handling

## Common Patterns
### Event-Driven Architecture
### Error Handling
### Logging (CloudWatch)
### Monitoring

## Troubleshooting
### Import Errors
### Deployment Issues
### Testing Issues

## Examples
### Complete Project Structure
### Example Lambda (Python)
### Example Lambda (Node.js)
### Example SAM Template
### Example Test
```

---

## Key Patterns to Document

### GCP Patterns (from solar_data_augmentation)

1. **Project Structure**:
```
project-root/
├── gc/                          # All Cloud Functions
│   ├── function1/
│   │   ├── main.py             # @functions_framework.http
│   │   ├── orchestrator.py
│   │   ├── types.py
│   │   ├── request_handler.py
│   │   ├── requirements.txt
│   │   ├── subfolder1/
│   │   │   ├── module1.py
│   │   │   └── module2.py
│   │   └── utils/
│   │       ├── gcs_operations.py
│   │       └── logging_utils.py
│   ├── function2/
│   │   └── [same structure]
│   └── common/                  # Shared across functions
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
    │   ├── test_module.py       # Uses sys.path.append
    │   └── test_integration.py
    ├── function2/
    │   └── test_module.py
    ├── common/
    │   ├── mock_gcs.py
    │   └── test_data_generators.py
    ├── conftest.py
    └── run_all_tests.sh
```

2. **Import Pattern** (NO package imports):
```python
# In production code (gc/function1/orchestrator.py):
from types import Result, Success, Failure
from request_handler import parse_request
from subfolder1.module1 import helper_function
from utils.gcs_operations import load_data

# In tests (tests/function1/test_orchestrator.py):
import sys
from pathlib import Path

# Add function directory to path
sys.path.insert(0, str(Path(__file__).parent.parent.parent / "gc" / "function1"))

from orchestrator import orchestrate
from types import Success, Failure
```

3. **Deployment Pattern**:
```bash
# deployment/functions/deploy_function1.sh
gcloud functions deploy function-name \
  --gen2 \
  --runtime=python311 \
  --region=australia-southeast1 \
  --source=./gc/function1 \
  --entry-point=handler_function \
  --trigger-http \
  --allow-unauthenticated \
  --memory=2GB \
  --timeout=540s \
  --set-env-vars GCP_PROJECT=project-id
```

4. **Test Pattern**:
```python
# tests/function1/test_module.py
import sys
from pathlib import Path

# CRITICAL: Add function directory to path
sys.path.insert(0, str(Path(__file__).parent.parent.parent / "gc" / "function1"))

import pytest
from unittest.mock import patch, MagicMock

# Now imports work as they do in Cloud Functions
from orchestrator import process_data
from types import Success, Failure

@patch('utils.gcs_operations.load_data')
def test_process_data(mock_load):
    mock_load.return_value = {"data": "test"}
    result = process_data("test_id")
    assert isinstance(result, Success)
```

### AWS Patterns (to be developed)

Similar structure but with:
- Lambda handler pattern instead of @functions_framework
- SAM templates instead of deployment scripts
- Local testing with SAM CLI
- Layers for common code instead of common/ folder

---

## Deliverables

### Documents
1. ✅ CURSOR_CLOUD_GCP.md (comprehensive GCP guide)
2. ✅ CURSOR_CLOUD_AWS.md (comprehensive AWS guide)

### Templates
3. ✅ templates/gcp_project_structure/ (complete project template)
4. ✅ templates/aws_project_structure/ (complete project template)

### Examples
5. ✅ examples/gcp_cloud_function_example/ (working example)
6. ✅ examples/aws_lambda_example/ (working example)

### Integration
7. ✅ Updated CURSOR.md with platform references
8. ✅ Migration guides (GCP ↔ AWS)
9. ✅ Validation checklists

---

## Timeline

| Phase | Duration | Deliverable |
|-------|----------|-------------|
| Phase 1 | 3h | GCP guidelines foundation |
| Phase 2 | 2h | GCP testing & workflows |
| Phase 3 | 2.5h | AWS guidelines foundation |
| Phase 4 | 2h | AWS deployment & testing |
| Phase 5 | 1.5h | Templates & examples |
| Phase 6 | 1h | Integration & validation |
| **Total** | **12h** | Complete platform guidelines |

---

## Dependencies

- Main CURSOR.md (from global rule set plan)
- Access to solar_data_augmentation reference project ✅
- AWS account for testing (optional but recommended)

---

## Integration with Global Rule Set

This plan addresses **Decision 5** from the global rule set plan:

**Decision 5: Platform-Specific Rules**
- ✅ Resolution: Create separate CURSOR_CLOUD_GCP.md and CURSOR_CLOUD_AWS.md
- ✅ Referenced from main CURSOR.md
- ✅ Optional additions to rule set
- ✅ Platform-specific but following universal rules (Tier 1)

---

## Notes

### Why Separate Documents?

1. **Cleaner main document**: CURSOR.md stays focused on universal rules
2. **Optional**: Teams using only one platform don't need both
3. **Detailed**: Can provide extensive platform-specific guidance
4. **Maintainable**: Update platform docs without affecting main rules
5. **Extensible**: Easy to add Azure, GCP, etc. later

### Key Differences: GCP vs AWS

| Aspect | GCP (Cloud Run Functions) | AWS (Lambda) |
|--------|---------------------------|--------------|
| Entry point | @functions_framework.http | lambda_handler(event, context) |
| Deployment | gcloud CLI | SAM/CDK |
| Common code | common/ folder | Lambda Layers |
| Testing | sys.path.append | Direct imports |
| Orchestration | Cloud Workflows | Step Functions |
| Storage | GCS | S3 |
| Secrets | Secret Manager | Secrets Manager |

### Proven Pattern

The solar_data_augmentation project structure is **proven in production**:
- 5 Cloud Functions (analysis, augmentation, business_validation, reporting, validation)
- Cloud Workflows orchestration
- 368 passing tests using sys.path.append pattern
- Modular, maintainable, deployable

This is the **gold standard** for GCP Python projects.

---

## Update History

### 2025-10-30 00:10
- Created plan based on user request
- Structure derived from solar_data_augmentation production project
- 12-hour estimate across 6 phases
- Ready for review and approval

---

## Status

🔄 **PLANNED** - Ready for review and implementation after global rule set completion

---

**Next**: Create paired TODO list (CLOUD_PLATFORM_GUIDELINES_TODO.md)

