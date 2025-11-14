# Basic .kimirules Template
# 
# MINIMAL: Use this for simple projects
# No auto-detection, no magic, just explicit includes
# Copy to your project root as .kimirules
#
# Instructions:
# 1. Set KIMI_RULES_PATH environment variable:
#    echo 'export KIMI_RULES_PATH="$HOME/projects/rules"' >> ~/.zshrc
#    source ~/.zshrc
#
# 2. Uncomment the appropriate lines for your project
# 3. Delete lines you're not using
#
# Pros: Simple, explicit, easy to understand
# Cons: Must manually edit when adding new languages/services

---

## 🌍 GLOBAL RULES (All Projects)

@${KIMI_RULES_PATH}/kimi/KIMI.md
@${KIMI_RULES_PATH}/kimi/KIMI_WORKFLOW_GUIDE.md
@${KIMI_RULES_PATH}/kimi/KIMI_FP_PRINCIPLES.md

---

## 📝 LANGUAGE-SPECIFIC (Uncomment your language)

# Python:
# @${KIMI_RULES_PATH}/kimi/python-fp-style-guide.md

# TypeScript:
# @${KIMI_RULES_PATH}/kimi/typescript-fp-style-guide.md

# Rust:
# @${KIMI_RULES_PATH}/kimi/rust-fp-style-guide.md

# Swift:
# @${KIMI_RULES_PATH}/kimi/swift-fp-style-guide.md

# Kotlin:
# @${KIMI_RULES_PATH}/kimi/kotlin-fp-style-guide.md

# Haskell:
# @${KIMI_RULES_PATH}/kimi/haskell-fp-style-guide.md

---

## ☁️ PLATFORM-SPECIFIC (Uncomment if using cloud)

# AWS:
# @${KIMI_RULES_PATH}/kimi/aws-fp-style-guide.md

# GCP:
# @${KIMI_RULES_PATH}/kimi/gcp-fp-style-guide.md

---

## 🏗️ PROJECT-SPECIFIC CUSTOMIZATIONS

# Add brief project-specific notes here
# Example:
# - This project uses Effect (not fp-ts)
# - All functions return TaskEither
# - 100% test coverage required
# - No mutations of inputs

---

**Template Version**: 1.0.0  
**Last Updated**: 2025-11-14  
**Maintained By**: Kimi CLI Global Rules System
