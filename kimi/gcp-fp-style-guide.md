# GCP Functional Programming Style Guide for Kimi CLI

**Version**: 2.0.0
**Last Updated**: 2025-11-19
**Part of**: [KIMI.md](KIMI.md) Global Rule Set
**Target**: GCP projects (Cloud Functions, Cloud Run, Firestore, Pub/Sub, Eventarc)

> **📖 Global Rules**: This document extends [KIMI.md](KIMI.md) with GCP-specific FP guidance. For mandatory universal rules (Git, documentation, testing), see [KIMI.md](KIMI.md).

---

## Quick Links

-   **Mandatory Rules**: See [KIMI.md](KIMI.md) sections 1-4
-   **FP Principles**: See [KIMI_FP_PRINCIPLES.md](KIMI_FP_PRINCIPLES.md)
-   **Universal GCP Patterns**: See [../code_guidelines/cloud/gcp/fp_patterns.md](../code_guidelines/cloud/gcp/fp_patterns.md)
-   **Universal Serverless Patterns**: See [../code_guidelines/cloud/serverless_fp_patterns.md](../code_guidelines/cloud/serverless_fp_patterns.md)

---

## Kimi-Specific Patterns for GCP

### Parallel Multi-Service Validation

Kimi can validate multiple GCP services simultaneously:

```typescript
// Kimi excels at validating GCP project structure
// Run parallel validations:
// ✓ Cloud Function IaC (Terraform/Cloud Build)
// ✓ Cloud Run service configurations
// ✓ Firestore security rules
// ✓ Pub/Sub topic/subscription setup
// ✓ Eventarc triggers

const eventProcessingPipeline = new gcp.cloudfunctions.Function(
  'event-processor',
  {
    runtime: 'nodejs20',
    sourceArchiveBucket: sourceBucket.name,
    sourceArchiveObject: object.name,
    eventTrigger: {
      eventType: 'google.storage.object.finalize',
      resource: inputBucket.name,
    },
  }
)

// Kimi validates: ✓ Event trigger configuration
//                 ✓ Environment variable setup
//                 ✓ Error handling through Pub/Sub
//                 ✓ Resource permissions
```

### Subagent Pattern for Multi-Project Validation

Use Kimi's Task tool for complex multi-project architectures:

```typescript
// Multi-environment setup benefits from Kimi subagents
interface EnvironmentConfig {
  projectId: string
  region: string
  environment: 'dev' | 'staging' | 'prod'
}

const validateEnvironments = (configs: EnvironmentConfig[]) => {
  // Kimi can spawn subagent per environment:
  // - dev: ✓ Relaxed security rules
  // - staging: ✓ Mirror production config
  // - prod: ✓ Strict IAM, encryption, audit logging

  return pipe(
    configs,
    A.traverse(TE.taskEither)(validateEnvironment)
  )
}
```

---

## Kimi CLI Integration

### Project Setup

```bash
# 1. Environment variables
export KIMI_RULES_PATH="$HOME/projects/rules"
export KIMI_GCP_RULES="$KIMI_RULES_PATH/kimi/gcp-fp-style-guide.md"

# 2. Project .kimirules
cat > .kimirules << 'EOF'
## GCP FP Rules
@${KIMI_RULES_PATH}/kimi/KIMI.md
@${KIMI_RULES_PATH}/kimi/gcp-fp-style-guide.md
@${KIMI_RULES_PATH}/kimi/typescript-fp-style-guide.md

## Language-Specific
!if-exists package.json @${KIMI_RULES_PATH}/kimi/typescript-fp-style-guide.md
!if-exists main.py @${KIMI_RULES_PATH}/kimi/python-fp-style-guide.md

## GCP Services
!if-exists firebase.json @${KIMI_RULES_PATH}/kimi/gcp-fp-style-guide.md
!if-exists app.yaml @${KIMI_RULES_PATH}/kimi/gcp-fp-style-guide.md
EOF

# 3. Validate setup
kimi read-file .kimirules
```

### Local Development Script

```bash
#!/bin/bash
# Kimi can validate GCP local development setup

echo "Starting Firebase emulators..."

# Run emulators in background
firebase emulators:start --only firestore,pubsub,functions &
EMULATOR_PID=$!

# Wait for ready
echo "Waiting for emulators..."
sleep 5

# Validate configuration
kimi read-file firebase.json &
curl -s http://127.0.0.1:8080 &

# Run type checking
npm run type-check &

# Run tests
npm test -- --watchAll=false &

# Parallel validations
wait

echo "All checks passing!"

# Cleanup on exit
trap "kill $EMULATOR_PID" EXIT
```
