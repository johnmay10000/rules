# MIGRATION_GUIDE.md - Upgrading to Global Rules v1.0

**Target Audience**: Users migrating from legacy setups to the new global rule set  
**Time Required**: 15-30 minutes  
**Version**: 1.0.0  

---

## Overview

This guide helps you migrate from:
- ❌ Project-specific scattered rules
- ❌ Copy-pasted `.cursorrules` files
- ❌ Inconsistent patterns across projects

To:
- ✅ Centralized global rules
- ✅ Portable setup (works on any machine)
- ✅ Auto-detection of tech stacks
- ✅ Universal FP patterns

---

## Migration Checklist

- [ ] **Step 1**: Set up global rules location
- [ ] **Step 2**: Update existing `.cursorrules` files
- [ ] **Step 3**: Remove duplicated content
- [ ] **Step 4**: Test in one project
- [ ] **Step 5**: Roll out to all projects
- [ ] **Step 6**: Clean up legacy files

**Estimated Time**: 15-30 minutes

---

## Step 1: Set Up Global Rules Location

### Choose Your Approach

**Option A: Environment Variable** (Recommended for multiple machines)

```bash
# 1. Clone this repo to a permanent location
cd ~/
git clone <repo-url> cursor-rules

# 2. Add to shell config (~/.zshrc or ~/.bashrc)
export CURSOR_RULES_PATH="$HOME/cursor-rules"

# 3. Reload shell
source ~/.zshrc  # or source ~/.bashrc
```

**Option B: Git Submodule** (Recommended for team projects)

```bash
# In each project root
git submodule add <repo-url> .cursor-rules

# Team members clone with submodules
git clone --recurse-submodules <project-url>
```

See [SETUP_GUIDE.md](SETUP_GUIDE.md) for detailed instructions.

---

## Step 2: Update Existing `.cursorrules` Files

### Before (Old Style)

```markdown
# .cursorrules - Old scattered approach

# Lots of repeated content copied from other projects...
Use Python 3.11+
Use type hints
Use functional programming
Use returns library
All functions return Result types
No exceptions
File size limit 250 lines
Commit every hour
Use pytest for testing
...hundreds of lines...
```

**Problems**:
- ❌ Duplication across projects
- ❌ Inconsistent between projects
- ❌ No central updates
- ❌ Hard to maintain

### After (New Centralized)

```markdown
# .cursorrules - New centralized approach

## Global Rules
@${CURSOR_RULES_PATH}/CURSOR.md

## Language-Specific Rules
@${CURSOR_RULES_PATH}/python-fp-style-guide.md

## Project-Specific Overrides
- **Tech Stack**: Python 3.11 + Polars + GCP
- **Testing**: pytest with 80%+ coverage
- **Deployment**: Cloud Run Functions

# That's it! ~10 lines instead of 200+
```

**Benefits**:
- ✅ No duplication
- ✅ Consistent across all projects
- ✅ Central updates benefit all projects
- ✅ Easy to maintain

---

## Step 3: Identify What to Remove

### What's Now in Global Rules (Remove from `.cursorrules`)

**Git Workflow** → Now in [CURSOR.md](CURSOR.md) Section 1
- Commit frequency
- Commit message format
- Checkpoint triggers

**Documentation Structure** → Now in [CURSOR.md](CURSOR.md) Section 2
- 3-tier hierarchy
- Timestamped files
- TODO list format

**Testing Requirements** → Now in [CURSOR.md](CURSOR.md) Section 3
- Coverage targets
- Test types
- When to test

**File Size Limits** → Now in [CURSOR.md](CURSOR.md) Section 4
- 250-300 line target
- How to split files

**FP Principles** → Now in [CURSOR.md](CURSOR.md) Section 5
- Pure functions
- Result types
- Railway-oriented programming

**Language Patterns** → Now in language guides
- Python: [python-fp-style-guide.md](python-fp-style-guide.md)
- TypeScript: [typescript-fp-style-guide.md](typescript-fp-style-guide.md)
- Swift: [swift-fp-style-guide.md](swift-fp-style-guide.md)
- Kotlin: [kotlin-fp-style-guide.md](kotlin-fp-style-guide.md)

### What to Keep in `.cursorrules` (Project-Specific)

**Keep**:
- ✅ Specific tech stack versions
- ✅ Project structure
- ✅ Custom requirements
- ✅ Team conventions
- ✅ Deployment details

**Example**:
```markdown
## Project-Specific Overrides

### Tech Stack
- Python 3.11.5
- Polars 0.19.0
- GCP Cloud Functions (Python 3.11 runtime)

### Project Structure
```
src/
  types/      # ADTs
  pure/       # Business logic
  io/         # GCS operations
  functions/  # Cloud Function entry points
```

### Custom Requirements
- All data frames must have explicit schemas
- GCS bucket: gs://my-project-data
- Max file upload: 10MB
```

---

## Step 4: Migration Example

### Legacy `.cursorrules` (200+ lines)

```markdown
# .cursorrules (old)

## Code Style
- Use Python 3.11+
- Use type hints on all functions
- All public functions return Result types
- Use dataclasses with frozen=True
- No exceptions except at IO boundaries
- Pattern matching for ADTs
- File size limit: 250 lines
- Split large files into modules

## Testing
- Use pytest
- 80%+ coverage for business logic
- 3+ tests per function
- All tests must pass before commit
- Mock external services
- Test happy path, errors, edge cases

## Git Workflow
- Commit every 30-60 minutes
- Commit after bug fixes
- Commit after features
- Use this commit message format:
  <summary>
  
  <details>
  
  <rationale>
  ...

## Documentation
- Daily work docs in docs/YYYY_MM_DD/
- Timestamp format: YYYYMMDD_HHMM_NAME.md
- Architecture plan at root
- Sub-plans in docs/plans/
...

## FP Principles
- Pure functions
- Immutable data
- Result types
- No defaults
- Railway-oriented programming
...

## Libraries
- returns for Result types
- toolz for FP utilities
- polars for data (no pandas)
- pytest for testing
...

## Project Structure
...more content...
```

### Migrated `.cursorrules` (~30 lines)

```markdown
# .cursorrules (new)

## Global Rules
@${CURSOR_RULES_PATH}/CURSOR.md

## Language-Specific Rules
@${CURSOR_RULES_PATH}/python-fp-style-guide.md

## Platform-Specific Rules
@${CURSOR_RULES_PATH}/GCP_GUIDELINES.md

## Project-Specific Overrides

### Tech Stack
- **Language**: Python 3.11.5
- **Data**: Polars 0.19.0
- **Platform**: GCP Cloud Functions
- **Testing**: pytest with 80%+ coverage

### Project Structure
```
src/
  types/      # ADTs and type definitions
  pure/       # Pure business logic
  io/         # GCS operations
  functions/  # Cloud Function entry points
```

### Custom Requirements
- All DataFrames have explicit schemas
- GCS bucket: `gs://my-project-data`
- Max upload: 10MB
- Use Pub/Sub for async jobs
```

**Reduction**: 200+ lines → 30 lines (85% reduction!)

---

## Step 5: Roll Out to All Projects

### Migration Strategy

**Phase 1: Pilot** (1 project, 1 day)
1. Pick one simple project
2. Migrate `.cursorrules`
3. Test thoroughly
4. Fix any issues

**Phase 2: Team Projects** (Week 1)
1. Migrate team projects
2. Update team documentation
3. Brief team on changes

**Phase 3: All Projects** (Week 2)
1. Migrate remaining projects
2. Archive old rules
3. Update wikis/docs

### Testing Each Project

```bash
# After migration, verify:

# 1. Cursor recognizes rules
# Open project in Cursor, check AI applies rules

# 2. File paths resolve
cat .cursorrules  # Should reference global rules correctly

# 3. Run existing tests
pytest  # Or your test command

# 4. Make a small change
# Verify Cursor enforces rules (file size, testing, etc.)
```

---

## Step 6: Clean Up Legacy Files

### What to Archive

**Remove from projects**:
- Old scattered rule files
- Duplicated content
- Copy-pasted guides

**Archive in wiki/docs**:
- Legacy patterns
- Historical decisions
- Migration notes

**Example cleanup**:
```bash
# In each project
git rm old-coding-standards.md
git rm legacy-rules.txt
git rm copied-fp-guide.md

# Keep minimal .cursorrules
git add .cursorrules
git commit -m "Migrate to global rules v1.0"
```

---

## Common Migration Issues

### Issue 1: Rules Not Found

**Symptom**: Cursor can't find `${CURSOR_RULES_PATH}/CURSOR.md`

**Solutions**:
```bash
# 1. Verify environment variable
echo $CURSOR_RULES_PATH
# Should print: /path/to/rules

# 2. Reload shell
source ~/.zshrc  # or ~/.bashrc

# 3. Restart Cursor
# Close and reopen Cursor completely
```

### Issue 2: Rules Not Applied

**Symptom**: Cursor doesn't enforce rules

**Solutions**:
1. Check `.cursorrules` syntax (must be markdown)
2. Verify file paths are correct
3. Try absolute path temporarily to debug
4. Check Cursor settings → Rules → Enabled

### Issue 3: Conflicting Rules

**Symptom**: Project-specific rules conflict with global

**Solution**:
```markdown
# In .cursorrules, project-specific overrides global

## Global Rules
@${CURSOR_RULES_PATH}/CURSOR.md  # Default: 250 lines

## Project-Specific Overrides
- File size limit: 350 lines  # Override for this project
```

### Issue 4: Team Onboarding

**Symptom**: New team members don't have setup

**Solution**:
Add to team onboarding docs:
```markdown
# Setup Cursor Rules

1. Clone rules repo
2. Set CURSOR_RULES_PATH
3. Restart shell and Cursor
4. Open any project - rules apply automatically
```

---

## Migration Support

### Before Migration

**Backup current setup**:
```bash
# Save current .cursorrules
cp .cursorrules .cursorrules.backup
```

### During Migration

**Use examples**:
- [Python example](examples/python_project/.cursorrules)
- [TypeScript example](examples/typescript_project/.cursorrules)
- [Polyglot example](examples/polyglot_project/.cursorrules)

### After Migration

**Verify**:
- [ ] Cursor recognizes global rules
- [ ] Project-specific rules work
- [ ] Auto-detection works
- [ ] Tests still pass
- [ ] Team can use it

---

## FAQ

**Q: Do I have to migrate all projects at once?**  
A: No! Migrate one at a time. Old and new can coexist.

**Q: What if I have custom rules not in global set?**  
A: Keep them in project-specific section of `.cursorrules`.

**Q: Can I still customize for my project?**  
A: Yes! Project-specific rules override global rules.

**Q: What about existing code that doesn't follow new rules?**  
A: New rules apply to new code. Migrate old code gradually.

**Q: How do I update global rules?**  
A: `cd $CURSOR_RULES_PATH && git pull` → applies to all projects.

---

## Next Steps

After migration:

1. ✅ **Read**: [CURSOR.md](CURSOR.md) for full rule set
2. ✅ **Learn**: [CURSOR_FP_PRINCIPLES.md](CURSOR_FP_PRINCIPLES.md) for FP deep dive
3. ✅ **Apply**: Start using in daily work
4. ✅ **Share**: Update team documentation
5. ✅ **Improve**: Submit PRs for missing patterns

---

**Migration Complete!** 🎉

Your projects now use:
- ✅ Centralized global rules
- ✅ Portable setup
- ✅ Auto-detection
- ✅ Universal FP patterns
- ✅ Easy updates (just `git pull`)

**See Also**:
- [README.md](README.md) - Full overview
- [SETUP_GUIDE.md](SETUP_GUIDE.md) - Detailed setup
- [CURSOR.md](CURSOR.md) - Main rule set

---

**Version**: 1.0.0  
**Last Updated**: 2025-10-31  
**Status**: Production Ready ✅

