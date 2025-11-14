- Writing guides for other AI agents and developers

### Success Metrics

- ✅ Documentation is clear, accurate, and comprehensive
- ✅ All Git commits follow mandatory format
- ✅ File size limits respected (250-300 lines target)
- ✅ Cross-references work correctly
- ✅ Templates function correctly when tested
- ✅ FP patterns are consistent across all language guides

---

## 🔧 Tools and Commands

### Git Commands (Used Frequently)

```bash
# Check status before commit
git status

# Add files (be specific, don't use git add .)
git add cursor/new-guide.md docs/plans/plan.md

# Commit with detailed message (template below)
git commit -m "Brief summary" -m "Detailed description..."

# View recent commits to understand pattern
git log --oneline -10
```

### File Operations

```bash
# Create new daily log (follow naming convention)
touch docs/20251114_0000_DESCRIPTIVE_NAME.md

# Check file length (must stay under 350 lines)
wc -l cursor/guide.md

# Find all TODO files
find docs/plans -name "*_TODO.md"
```

### Verification Commands

```bash
# Check markdown formatting
find . -name "*.md" -exec echo "Checking {}" \; -exec head -5 {} \;

# Find broken links (manual check needed)
grep -r "\[.*\](.*\.md)" cursor/ | head -20

# Verify template structure
ls -la cursor/examples/python_project/
```

---

## ⚠️ Critical Warnings

### NEVER Do These

1. ❌ **Wait longer than 60 minutes between Git commits** - Always checkpoint
2. ❌ **Create files without updating TODO lists** - Keep TODOs in sync
3. ❌ **Exceed 350 lines without documenting exception** - Split files instead
4. ❌ **Commit without reading your changes** - Always verify content first
5. ❌ **Use vague commit messages** - Follow the mandatory format exactly
6. ❌ **Create documentation without cross-references** - Link to related docs
7. ❌ **Ignore file naming conventions** - Use exact formats specified

### Common Mistakes to Avoid

- **Creating multiple logical units in one commit** - Each phase/template/example = separate commit
- **Not updating version numbers** - Update version/date when modifying files
- **Forgetting to test templates** - Always copy templates to test projects
- **Inconsistent terminology** - Use consistent terms across all guides
- **Missing rationale in commits** - Always explain WHY changes were made

---

## 📊 Repository Statistics

- **Total Markdown Files**: 104+
- **Languages Covered**: 6 (Python, TypeScript, Swift, Kotlin, Rust, Haskell)
- **Core Rule Documents**: 10+
- **Language Guides**: 6 comprehensive guides
- **Project Templates**: 4+ complete examples
- **Smart Templates**: 2+ `.cursorrules` templates
- **Planning Documents**: 15+ in `docs/plans/`
- **Daily Work Logs**: 40+ in dated folders

**Repository Size**: ~460KB total (all documentation)

---

## 🎉 Getting Started as AI Agent

**First Actions When Working on This Repo:**

1. **Read this AGENTS.md** completely ✓ (you're doing it!)
2. **Read `cursor/CURSOR.md`** - Understand mandatory universal rules
3. **Check current TODO lists** - Look in `docs/plans/` for active work
4. **Review recent commits** - `git log` to understand commit style
5. **Understand the structure** - Navigate `cursor/`, `docs/`, `examples/`

**When User Makes a Request:**

1. **Identify the task type** (create guide, template, example, or update)
2. **Check for existing plans** in `docs/plans/`
3. **Follow the mandatory conventions** (Git checkpoints, naming, file sizes)
4. **Reference existing patterns** (don't reinvent, be consistent)
5. **Test your work** (copy templates, verify examples)
6. **Update TODOs and commit** (every 30-60 minutes)

---

**Last Updated**: 2025-11-14  
**Version**: 1.0.0  
**Status**: Production Ready  

**Remember**: You are maintaining documentation that will be used by thousands of developers and AI agents. Follow the rules precisely - they exist for good reasons!

---

**Part 3 of 3** | Back: **[AGENTS_OVERVIEW.md](AGENTS_OVERVIEW.md)** | Previous: **[AGENTS_WORKFLOWS.md](AGENTS_WORKFLOWS.md)**

**Last Updated**: 2025-11-14
**Version**: 1.0.1
**Status**: Production Ready

---

## 🤖 Kimi CLI Specific Guidelines

**When working on Kimi-specific files (`kimi/` folder):**

### Kimi Tool Architecture

Kimi CLI uses a **tool-based architecture** with parallel execution capabilities:

- **ReadFile**: Read multiple files simultaneously
- **WriteFile**: Write multiple files in one response  
- **SetTodoList**: Tool-based task tracking (not just markdown)
- **Bash**: Execute system commands with timeout
- **Task**: Spawn subagents for complex operations

**Parallel Execution Example** (Kimi-specific pattern):
```bash
# Kimi can read 4 files at once (much faster than sequential)
ReadFile: cursor/CURSOR.md
ReadFile: cursor/CURSOR_WORKFLOW_GUIDE.md
ReadFile: cursor/SETUP_GUIDE.md
ReadFile: cursor/FILE_LOCATIONS_USER_GUIDE.md

# Or write multiple files simultaneously
WriteFile: docs/plans/FEATURE_PLAN.md
WriteFile: docs/plans/FEATURE_TODO.md
WriteFile: docs/2025_11_14/20251114_0001_EXAMPLE.md
```

### SetTodoList Tool Integration

**Unlike Cursor/Claude which use markdown TODOs, Kimi has a dedicated SetTodoList tool:**

```typescript
// Use SetTodoList tool instead of just editing markdown
SetTodoList: [
  { title: "TASK-1: Create KIMI.md", status: "Done" },
  { title: "TASK-2: Create KIMI_WORKFLOW_GUIDE.md", status: "In Progress" }
]
```

**Key Differences from Markdown TODOs:**
- ✅ Tool-enforced structure (not free-form text)
- ✅ Status tracking (Pending/In Progress/Done)
- ✅ Can be integrated with actual task management systems
- ✅ More formal than markdown checkboxes

### Kimi Commit Format

**All Kimi commits MUST include:**

```
Commit message with detailed description

🤖 Generated with [Kimi](https://kimi.ai)

Co-Authored-By: Kimi <noreply@kimi.ai>
```

### Parallel Validation Patterns

**Kimi excels at parallel validation - document these patterns:**

```bash
# Validate multiple components simultaneously
cd project && pytest tests/unit/ &
cd project && pytest tests/integration/ &
wait

# Type check multiple files
cat file1.py file2.py file3.py | python -m pyflakes &
cat file1.ts file2.ts file3.ts | npx tsc --noEmit &
wait
```

### Subagent Usage

**For complex, independent subtasks, spawn subagents:**

```typescript
// Use Task tool to spawn subagent
Task: {
  description: "Research Kimi patterns",
  prompt: "Find Kimi-specific workflow patterns and best practices"
}

// Context isolation - subagent gets clean context
// Use for: code generation, research, refactoring operations
```

### Kimi-Specific Documentation Files

**When working with Kimi, reference these files:**

- **`kimi/KIMI.md`** - Main Kimi mandatory rules (parallel tools, SetTodoList)
- **`kimi/KIMI_WORKFLOW_GUIDE.md`** - Kimi-specific workflow patterns
- **`kimi/KIMI_FP_PRINCIPLES.md`** - FP principles with Kimi examples
- **`kimi/DATA_STRUCTURE_PATTERNS.md`** - FP data structures for Kimi
- **`kimi/FILE_LOCATIONS_USER_GUIDE.md`** - File organization for Kimi projects
- **`kimi/NAMING_CONVENTION.md`** - Naming conventions with Kimi examples

### Effect-ts for TypeScript (Kimi Difference)

**Note: Kimi uses Effect-ts while Cursor uses fp-ts:**

- **Effect-ts**: More comprehensive, better TypeScript integration
- **fp-ts**: Lighter weight, more established
- **Key difference**: Effect-ts includes streaming, concurrency, dependency injection
- **Both valid**: Kimi examples demonstrate Effect-ts patterns

### Kimi Example Projects

**The `kimi/examples/` folder demonstrates Kimi-specific patterns:**

1. **plan_with_todo** - SetTodoList integration across all tiers
2. **python_project** - Returns library with Result types
3. **rust_project** - Native Result<T, E> with railway composition
4. **typescript_project** - Full-stack with Effect-ts

**What Each Example Shows:**
- Three-tier documentation structure
- Cross-reference patterns
- Language-specific FP implementation
- Kimi workflow integration
- Parallel validation examples

### Testing Kimi Templates

**Verify Kimi templates work correctly:**

```bash
# For envvar template
export KIMI_RULES_PATH="$HOME/projects/rules"
cp kimi/templates/.kimirules_smart_template_envvar ./.kimirules
# Test that Kimi loads rules correctly

# For submodule template
git submodule add <repo> .kimi-rules
cp kimi/templates/.kimirules_smart_template_submodule ./.kimirules
# Verify relative paths work
```

### Repository Statistics Update (with Kimi)

- **Total Markdown Files**: 165+ (104+ original + 60+ Kimi files)
- **Languages Covered**: 6 languages × 2 AI tools = 12 guides
- **Core Rule Documents**: 15+ (original + Kimi-specific)
- **Project Templates**: 8+ (4 Cursor + 4 Kimi)
- **Smart Templates**: 5+ (2 Cursor + 3 Kimi)
- **Kimi-Specific Examples**: 4 complete examples
- **Planning Documents**: 20+ including Kimi implementation plans

**Repository Size**: ~700KB total (including Kimi documentation)

---

**Key Takeaway**: Kimi has parallel, tool-based architecture. Leverage parallel execution, SetTodoList tool, and subagents for maximum efficiency. Documentation goes in `kimi/` folder mirroring `cursor/` structure.

