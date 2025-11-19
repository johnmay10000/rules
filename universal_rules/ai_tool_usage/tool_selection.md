---
title: AI Tool Selection Guidelines
+category: universal_rules
++type: ai_tool_usage
++applies_to: all
++version: 1.0.0
++last_updated: 2025-11-19
++---
++
++# AI Tool Selection Guidelines
++
++**Status**: RECOMMENDED - Guidelines for choosing the optimal AI assistant
++
++This document provides guidance on selecting the most appropriate AI assistant (Cursor, Kimi, Claude, Gemini) for different tasks based on their unique strengths and capabilities.
++
++---
++
++## Tool Comparison Matrix
++
++| Feature | Cursor | Kimi | Claude | Gemini |
++|---------|--------|------|--------|--------|
++| **IDE Integration** | ✅ Native (VS Code) | ⚠️ Limited (CLI) | ⚠️ Limited (API) | ⚠️ Limited (API) |
++| **Parallel Execution** | ⚠️ Partial | ✅ Excellent | ⚠️ Limited | ⚠️ Limited |
++| **Task Planning** | ✅ Good | ✅ Excellent (SetTodoList) | ✅ Good | ✅ Good |
++| **Code Generation** | ✅ Excellent | ✅ Very Good | ✅ Excellent | ✅ Very Good |
++| **FP Patterns** | ✅ Strong | ✅ Strong | ✅ Excellent | ✅ Strong |
++| **Testing** | ✅ Integrated | ✅ Parallel | ✅ Strategic | ✅ Comprehensive |
++| **Documentation** | ✅ Good | ✅ Excellent | ✅ Excellent | ✅ Excellent |
++| **Subagents** | ⚠️ Limited | ✅ Excellent | ⚠️ Limited | ⚠️ Limited |
++| **Context Window** | Standard | ✅ Very Large | Standard | ✅ Very Large |
++| **Best For** | IDE work, refactoring | Complex workflows, planning | Architecture, design | Multi-language, education |
++
++---
++
++## Selection Criteria
++
++### Choose Cursor When:
++
++- ✅ Working primarily in VS Code
++- ✅ Doing extensive refactoring
++- ✅ Need inline code suggestions
++- ✅ Want real-time IDE integration
++- ✅ Debugging complex code
++- ✅ Navigating large codebases
++
++**Strengths**: IDE-native experience, refactoring, inline suggestions
++
++**Example Tasks**:
++```bash
++# Refactoring a large TypeScript codebase
++# Debugging with breakpoints and variable inspection
++# Real-time code completion and suggestions
++# Multi-file edits with preview
++```
++
++---
++
++### Choose Kimi When:
++
++- ✅ Need complex task planning and tracking
++- ✅ Want parallel execution of multiple tasks
++- ✅ Working with SetTodoList integration
++- ✅ Need subagents for complex validation
++- ✅ Doing multi-phase project planning
++- ✅ Want to track progress across sessions
++
++**Strengths**: Task management, parallel execution, SetTodoList, subagents
++
++**Example Tasks**:
++```bash
++# Plan and execute multi-phase refactoring
++kimi --plan "Refactor auth system" --phases 5
++
++# Run tests in parallel
++kimi test --parallel unit integration e2e
++
++# Generate tests with coverage requirements
++kimi generate-tests --coverage 80
++
++# Complex workflow with subagents
++kimi task --spawn-subagent validation --spawn-subagent testing
++```
++
++---
++
++### Choose Claude When:
++
++- ✅ Need architectural design and planning
++- ✅ Want detailed explanations and reasoning
++- ✅ Designing system architecture
++- ✅ Need to understand complex trade-offs
++- ✅ Want comprehensive documentation
++- ✅ Working on API design
++
++**Strengths**: Architecture, design, detailed explanations, reasoning
++
++**Example Tasks**:
++```bash
++# Design system architecture
++# Create comprehensive API design document
++# Review and improve existing architecture
++# Generate detailed technical documentation
++# Explain complex design decisions
++```
++
++---
++
++### Choose Gemini When:
++
++- ✅ Need multi-language support
++- ✅ Want comprehensive educational content
++- ✅ Comparing approaches across languages
++- ✅ Need extensive examples and patterns
++- ✅ Working with unfamiliar technology
++- ✅ Want best practices and anti-patterns
++
++**Strengths**: Multi-language, education, examples, best practices
++
++**Example Tasks**:
++```bash
++# Generate examples in multiple languages
++# Create comprehensive style guide
++# Explain concepts with detailed examples
++# Compare implementations across languages
++# Generate educational content
++```
++
++---
++
++## Task-Based Selection Guide
++
++### 1. Refactoring Existing Code
++
++**Primary Choice**: Cursor
++**Alternative**: Kimi
++
++**Rationale**: Cursor's IDE integration shows real-time impact of changes across files
++
++**Workflow**:
++1. Use Cursor's inline suggestions for small changes
++2. Use Cursor's multi-file edit for coordinated changes
++3. Use Kimi's parallel validation for complex refactorings
++
++---
++
++### 2. Greenfield Project Setup
++
++**Primary Choice**: Kimi
++**Alternative**: Claude
++
++**Rationale**: Kimi's SetTodoList tracks all setup tasks; Claude provides architectural guidance
++
++**Workflow**:
++1. Use Claude to design overall architecture
++2. Use Kimi to create detailed project plan with TODOs
++3. Use Kimi's parallel execution to set up multiple components
++
++---
++
++### 3. Writing Tests
++
++**Primary Choice**: Kimi
++**Alternative**: Cursor
++
++**Rationale**: Kimi can generate tests in parallel for multiple functions
++
++**Workflow**:
++1. Use Kimi to generate unit tests for all functions
++2. Use Kimi to generate integration tests
++3. Use Cursor to debug and fix failing tests in IDE
++
++---
++
++### 4. Debugging Complex Issues
++
++**Primary Choice**: Cursor
++**Alternative**: Claude
++
++**Rationale**: Cursor's IDE integration allows breakpoints and step debugging
++
++**Workflow**:
++1. Use Cursor's debugger to identify issue
++2. Use Claude to understand root cause and design fix
++3. Use Cursor to implement and verify fix
++
++---
++
++### 5. Documentation Writing
++
++**Primary Choice**: Claude
++**Alternative**: Gemini
++
++**Rationale**: Claude provides comprehensive, well-structured documentation
++
++**Workflow**:
++1. Use Claude to create architecture documentation
++2. Use Gemini to generate multi-language examples
++3. Use Kimi to create and track documentation tasks
++
++---
++
++### 6. Code Review
++
++**Primary Choice**: Claude
++**Alternative**: Kimi
++
++**Rationale**: Claude provides detailed analysis and suggestions
++
++**Workflow**:
++1. Use Claude to review PR and suggest improvements
++2. Use Kimi to validate suggestions don't break tests
++3. Use Cursor to apply fixes in IDE
++
++---
++
++### 7. Learning New Technology
++
++**Primary Choice**: Gemini
++**Alternative**: Claude
++
++**Rationale**: Gemini provides comprehensive educational content
++
++**Workflow**:
++1. Use Gemini to understand concepts and patterns
++2. Use Gemini to generate examples
++3. Use Cursor to experiment in IDE
++
++---
++
++### 8. Performance Optimization
++
++**Primary Choice**: Kimi
++**Alternative**: Cursor
++
++**Rationale**: Kimi can run benchmarks in parallel
++
++**Workflow**:
++1. Use Kimi to run performance tests across codebase
++2. Use Cursor's profiler integration to identify bottlenecks
++3. Use Kimi to validate optimizations don't break functionality
++
++---
++
++## Multi-Tool Workflows
++
++### Complex Feature Development
++
++```bash
++# Phase 1: Planning (Claude + Kimi)
++claude --task "Design user authentication system"
++kimi --create-plan --phases 5 --output auth_plan.md
++
++# Phase 2: Implementation (Cursor + Kimi)
++cursor --open-project
++kimi --execute-phase 1 --parallel
++
++# Phase 3: Testing (Kimi + Cursor)
++kimi --generate-tests --coverage 80
++cursor --debug-failing-tests
++
++# Phase 4: Documentation (Claude + Gemini)
++claude --document-architecture
++gemini --generate-examples --languages python,typescript,rust
++```
++
++### Refactoring Legacy Code
++
++```bash
++# Step 1: Analysis (Claude)
++claude --analyze-legacy-code --identify-issues
++
++# Step 2: Planning (Kimi)
++kimi --create-refactoring-plan --estimate 3-days
++
++# Step 3: Implementation (Cursor)
++cursor --refactor --preview-changes
++
++# Step 4: Validation (Kimi)
++kimi --run-tests --parallel --validate-refactoring
++```
++
++## Best Practices
++
++### 1. Use the Right Tool for the Task
++
++Don't use a single tool for everything. Each AI assistant has strengths:
++
++```bash
++# ❌ Bad: Using one tool for everything
++cursor --plan-architecture  # Not ideal
++
++# ✅ Good: Using right tool for each task
++claude --plan-architecture  # Claude excels at this
++kimi --execute-plan         # Kimi excels at execution
++cursor --implement          # Cursor excels at coding
++```
++
++### 2. Combine Tools for Best Results
++
++```bash
++# Example: Create comprehensive documentation
++claude --create-architecture-doc > ARCHITECTURE.md
++gemini --add-examples --input ARCHITECTURE.md > ARCHITECTURE_WITH_EXAMPLES.md
++kimi --review --check-completeness > FINAL_ARCHITECTURE.md
++```
++
++### 3. Leverage Unique Features
++
++```bash
++# Use Kimi's SetTodoList for tracking
++kimi --task "Implement feature X" --track
++
++# Use Cursor's IDE integration for refactoring
++cursor --refactor --multi-file --preview
++
++# Use Claude for detailed explanations
++claude --explain --concept "railway-oriented-programming"
++
++# Use Gemini for multi-language examples
++gemini --generate-examples --topic "error-handling" --langs python,typescript,rust
++```
++
++### 4. Consider Context Window
++
++```bash
++# For large codebases: Use Kimi or Gemini (larger context)
++kimi --analyze --large-codebase
++gemini --understand --project-structure
++
++# For focused files: Use Cursor or Claude
++cursor --edit --file src/main.ts
++claude --review --pr 123
++```
++
++### 5. Optimize for Speed
++
++```bash
++# Parallel execution for independent tasks
++kimi --parallel --tasks "test-unit,test-integration,lint"
++
++# Sequential for dependent tasks
++kimi --sequential --tasks "build,test,deploy"
++```
++
++## Tool-Specific Notes
++
++### Cursor-Specific
++
++**Best For**:
++- Real-time code editing
++- Refactoring with preview
++- Debugging with breakpoints
++- Multi-cursor editing
++
++**Limitations**:
++- Limited parallel execution
++- No built-in task tracking
++- Smaller context window than Kimi/Gemini
++
++**Tips**:
++- Use `.cursorrules` for project-specific guidance
++- Leverage inline suggestions
++- Use multi-file edit for coordinated changes
++
++### Kimi-Specific
++
++**Best For**:
++- Complex project planning
++- Parallel task execution
++- Long-running workflows
++- Task tracking with SetTodoList
++
++**Limitations**:
++- Limited IDE integration
++- CLI-only interface
++
++**Tips**:
++- Use SetTodoList for all tasks
++- Leverage subagents for validation
++- Use parallel execution extensively
++- Create detailed plans before execution
++
++### Claude-Specific
++
++**Best For**:
++- Architectural design
++- Detailed explanations
++- Code review and analysis
++- Documentation writing
++
++**Limitations**:
++- No IDE integration
++- No parallel execution
++
++**Tips**:
++- Ask for detailed reasoning
++- Request multiple alternatives
++- Use for design decisions
++- Leverage for documentation
++
++### Gemini-Specific
++
++**Best For**:
++- Multi-language examples
++- Educational content
++- Best practices and patterns
++- Anti-pattern identification
++
++**Limitations**:
++- No IDE integration
++- No parallel execution
++
++**Tips**:
++- Ask for examples in multiple languages
++- Request comparison of approaches
++- Use for learning new technologies
++- Leverage for comprehensive guides
++
++## Common Mistakes
++
++### ❌ Using Wrong Tool for Task
++
++```bash
++# Bad: Using Cursor for complex planning
++cursor --plan "Design microservices architecture"
++# Result: Limited by context window, no task tracking
++
++# Good: Using Claude for planning
++claude --design-architecture --output architecture.md
++kimi --create-plan --from architecture.md
++```
++
++### ❌ Not Combining Tools
++
++```bash
++# Bad: Using only one tool
++cursor --implement --test --document
++# Result: Suboptimal for testing and documentation
++
++# Good: Using best tool for each phase
++cursor --implement
++kimi --test --parallel
++claude --document
++gemini --add-examples
++```
++
++### ❌ Ignoring Tool Strengths
++
++```bash
++# Bad: Not leveraging unique features
++kimi --single-threaded  # Not using parallel execution
++cursor --no-preview      # Not using preview feature
++
++# Good: Leverage unique strengths
++kimi --parallel --subagents
++cursor --preview --multi-file
++```
++
++## Decision Flowchart
++
++```
++Start Task
++  ↓
++Is it IDE-focused coding?
++  ↓ YES → Use Cursor
++  ↓ NO
++Is it complex multi-step planning?
++  ↓ YES → Use Kimi
++  ↓ NO
++Is it architectural design?
++  ↓ YES → Use Claude
++  ↓ NO
++Is it learning/documentation?
++  ↓ YES → Use Gemini
++  ↓ NO
++Use Kimi (general purpose)
++```
++
++## Related Documents
++
++- **MCP Protocol**: `mcp_protocol.md` - MCP integration guidelines
++- **Context Management**: `context_management.md` - Managing AI context
++- **Tool-Specific Features**: See `ai_tool_specific/` directory
++
++---
++
++**Last Updated**: 2025-11-19  
++**Maintained By**: Global AI Rules System  
++**Status**: Active  
++**Applies To**: All AI assistants (Cursor, Kimi, Claude, Gemini) and all developers
