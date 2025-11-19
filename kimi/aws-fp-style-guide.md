# AWS Functional Programming Style Guide for Kimi CLI

**Version**: 2.0.0
**Last Updated**: 2025-11-19
**Part of**: [KIMI.md](KIMI.md) Global Rule Set
**Target**: AWS projects (Lambda, Step Functions, DynamoDB, EventBridge, S3)

> **📖 Global Rules**: This document extends [KIMI.md](KIMI.md) with AWS-specific FP guidance. For mandatory universal rules (Git, documentation, testing), see [KIMI.md](KIMI.md).

---

## Quick Links

-   **Mandatory Rules**: See [KIMI.md](KIMI.md) sections 1-4
-   **FP Principles**: See [KIMI_FP_PRINCIPLES.md](KIMI_FP_PRINCIPLES.md)
-   **Universal AWS Patterns**: See [../code_guidelines/cloud/aws/fp_patterns.md](../code_guidelines/cloud/aws/fp_patterns.md)
-   **Universal Serverless Patterns**: See [../code_guidelines/cloud/serverless_fp_patterns.md](../code_guidelines/cloud/serverless_fp_patterns.md)

---

## Kimi-Specific Patterns for AWS

### Parallel Deployment Validation

Kimi can validate multiple AWS resources simultaneously before deployment:

```typescript
// Kimi excels at validating AWS SAM/CloudFormation templates
// Run parallel validations:
// ✓ Lambda handler type safety
// ✓ IAM policy correctness
// ✓ Event source mappings
// ✓ Resource naming conventions

const userProcessingStateMachine = new sfn.StateMachine(
  this,
  'UserProcessingStateMachine',
  {
    definitionBody: sfn.DefinitionBody.fromChainable(
      sfn.Chain.start(
        new tasks.LambdaInvoke(this, 'ValidateUser', {
          lambdaFunction: validateUserLambda,
          // Kimi validates: ✓ Lambda handler exists
          //                 ✓ Input payload matches schema
          //                 ✓ ResultPath configuration
        })
      )
    ),
  }
)
```

### Subagent Pattern for Multi-Region Validation

Use Kimi's Task tool to spawn subagents for validating multi-region deployments:

```typescript
// Complex multi-region deployment benefits from Kimi subagents
// Spawn subagent per region:
// - us-east-1: ✓ Validate Lambda versions
// - eu-west-1: ✓ Validate DynamoDB Global Tables
// - ap-southeast-2: ✓ Validate S3 cross-region replication

interface RegionalDeployment {
  region: string
  lambdaVersion: string
  tableName: string
  bucketName: string
}

const deployToRegions = (regions: RegionalDeployment[]) => {
  // Kimi can spawn subagent for each region in parallel
  regions.forEach((region) => {
    // Subagent validates: ✓ Service availability
    //                      ✓ IAM permissions
    //                      ✓ Resource quotas
    //                      ✓ Cost limits
  })
}
```

---

## Kimi CLI Integration

### Project Setup

```bash
# 1. Environment variables
export KIMI_RULES_PATH="$HOME/projects/rules"
export KIMI_AWS_RULES="$KIMI_RULES_PATH/kimi/aws-fp-style-guide.md"

# 2. Project .kimirules
cat > .kimirules << 'EOF'
## AWS FP Rules
@${KIMI_RULES_PATH}/kimi/KIMI.md
@${KIMI_RULES_PATH}/kimi/aws-fp-style-guide.md
@${KIMI_RULES_PATH}/kimi/typescript-fp-style-guide.md

## Language-Specific
!if-exists package.json @${KIMI_RULES_PATH}/kimi/typescript-fp-style-guide.md
!if-exists requirements.txt @${KIMI_RULES_PATH}/kimi/python-fp-style-guide.md

## AWS Services
!if-exists template.yaml @${KIMI_RULES_PATH}/kimi/aws-fp-style-guide.md
!if-exists serverless.yml @${KIMI_RULES_PATH}/kimi/aws-fp-style-guide.md
EOF

# 3. Validate setup
kimi read-file .kimirules
```

### Deployment Validation Script

```bash
#!/bin/bash
# Kimi can validate AWS deployment in parallel

# Validate SAM template
sam validate --template template.yaml &

# Check Lambda handler types
python -m mypy src/handlers/ &

# Verify IAM policies (security)
python -m parliament --file iam-policies.yaml &

# Run integration tests
pytest tests/integration/ -v &

# All validations in parallel
wait
```
