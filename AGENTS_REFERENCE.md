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
