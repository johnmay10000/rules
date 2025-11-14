# KIMI_FP_PRINCIPLES.md

**Note**: Refer to [cursor/CURSOR_FP_PRINCIPLES.md](cursor/CURSOR_FP_PRINCIPLES.md) for comprehensive functional programming principles.

This document provides Kimi-specific notes on applying functional programming principles.

---

## Kimi-Specific FP Guidance

### Tool Integration

When working with FP patterns in Kimi CLI:

1. **Use SetTodoList for Tracking**: Track FP refactoring tasks
2. **Parallel Verification**: Use parallel tool calls to verify multiple pure functions
3. **Subagent Pattern**: Spawn subagents for complex type checking or property-based testing

### Key Differences from Cursor/Claude

Kimi has unique strengths for FP development:

- **Parallel tool execution**: Can verify type safety, run tests, and check formatting simultaneously
- **Subagent delegation**: Can spawn specialized subagents for specific FP validations
- **Task-based workflow**: Aligns well with FP's composable nature

---

## Core FP Principles (Reference)

For detailed FP principles including:
- Category Theory basics
- Monad laws
- Functor and Applicative patterns
- Railway-oriented programming
- Lens and optics

See: [cursor/CURSOR_FP_PRINCIPLES.md](cursor/CURSOR_FP_PRINCIPLES.md)

This is a **shared reference document** used by all AI agents (Cursor, Claude, Kimi).

---

**Last Updated**: 2025-11-14
**Status**: Active (References shared FP principles)
