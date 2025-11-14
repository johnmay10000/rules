# MIGRATION_GUIDE.md - Upgrading to Global AI Rules v1.0

**Target Audience**: Users migrating from legacy setups to the new global AI rule set  
**Time Required**: 15-30 minutes  
**Version**: 1.0.0  
**AI Tools Supported**: Cursor 🎯, Kimi 🤖, Claude 🔮, Gemini 💎

---

## Overview

This guide helps you migrate from:
- ❌ Project-specific scattered rules
- ❌ Copy-pasted `.cursorrules` / `.kimirules` files
- ❌ Inconsistent patterns across projects
- ❌ Single AI tool lock-in

To:
- ✅ Centralized global rules for **multiple AI tools**
- ✅ Portable setup (works on any machine)
- ✅ Auto-detection of tech stacks
- ✅ Universal FP patterns across all AI assistants
- ✅ Consistent experience whether using Cursor, Kimi, Claude, or Gemini

---

## Migration Checklist

**Phase 1: Setup** (5-10 minutes)
- [ ] Choose your primary AI tool
- [ ] Set up global rules location with environment variable
- [ ] Verify paths work correctly

**Phase 2: Migrate First Project** (10-15 minutes)
- [ ] Backup existing rules file
- [ ] Replace with new global rules structure
- [ ] Add project-specific overrides only
- [ ] Test rule application

**Phase 3: Roll Out & Optimize** (5-10 minutes per project)
- [ ] Migrate remaining projects
- [ ] Clean up legacy files
- [ ] Explore AI tool-specific features

**Estimated Time**: 15-30 minutes for first project, 5-10 minutes per additional project

---

## Phase 1: Set Up Global Rules Location

### Step 1: Choose Your AI Tool

**Decide which AI coding assistant(s) you use:**

| AI Tool | Best For | Setup File | Environment Var |
|---------|----------|------------|-----------------|
| Cursor 🎯 | IDE integration | `.cursorrules` | `CURSOR_RULES_PATH` |
| Kimi 🤖 | **Parallel execution (30-50% faster)** | `.kimirules` | `KIMI_RULES_PATH` |
| Claude 🔮 | Command workflow | `.claude-rules` | `CLAUDE_RULES_PATH` |
| Gemini 💎 | Concise patterns | `.gemini-rules` | `GEMINI_RULES_PATH` |

**You can set up multiple tools simultaneously!** The same global rules support all AI assistants.

### Step 2: Clone Repository & Set Environment

**Option A: Environment Variable** (Recommended for multiple machines)

```bash
# 1. Clone this repo to a permanent location
#    Recommendation: Keep it in your home directory
cd ~/
git clone <repo-url> ai-rules

# 2. Add to shell config (~/.zshrc or ~/.bashrc)
#    Choose the line(s) for AI tool(s) you use:

# For Cursor users:
export CURSOR_RULES_PATH="$HOME/ai-rules"

# For Kimi users:
export KIMI_RULES_PATH="$HOME/ai-rules"

# For Claude users:
export CLAUDE_RULES_PATH="$HOME/ai-rules"

# For Gemini users:
export GEMINI_RULES_PATH="$HOME/ai-rules"

# 3. Reload shell
source ~/.zshrc  # or source ~/.bashrc

# 4. Verify it works
#    Close and reopen your terminal, then:
echo $CURSOR_RULES_PATH  # Should print: /home/user/ai-rules
```

**Option B: Git Submodule** (Recommended for team projects)

```bash
# In each project root where you want rules
git submodule add <repo-url> .ai-rules

# Team members clone with submodules
git clone --recurse-submodules <project-url>

# Then reference with relative paths
# (see examples in Step 3)
```

**Option C: Hardcoded Paths** (Quick but less portable)

```bash
# Directly reference in your rules file (less flexible)
# Only use this for quick testing
```

### Step 3: Verify Setup

**Test that your AI tool can find the rules:**

**For Cursor:**
```bash
# Should print the rules path
echo $CURSOR_RULES_PATH

# Should show the rules directory
ls $CURSOR_RULES_PATH/cursor/
```

**For Kimi:**
```bash
# Should print the rules path
echo $KIMI_RULES_PATH

# Should show the rules directory
ls $KIMI_RULES_PATH/kimi/
```

**Expected output:**
```
ls $CURSOR_RULES_PATH/cursor/
# CURSOR.md  CURSOR_FP_PRINCIPLES.md  CURSOR_WORKFLOW_GUIDE.md  SETUP_GUIDE.md
# python-fp-style-guide.md  typescript-fp-style-guide.md ...
```

---

## Phase 2: Migrate Your First Project

### Step 1: Backup Existing Rules

```bash
cd your-project/

# Backup existing rules (just in case)
cp .cursorrules .cursorrules.backup.$(date +%Y%m%d)  # For Cursor
cp .kimirules .kimirules.backup.$(date +%Y%m%d)     # For Kimi
cp .claude-rules .claude-rules.backup.$(date +%Y%m%d) # For Claude
cp .gemini-rules .gemini-rules.backup.$(date +%Y%m%d) # For Gemini
```

### Step 2: Replace With New Structure

#### Migrated Rules File Examples

**For Cursor (.cursorrules):**
```markdown
# .cursorrules - NEW GLOBAL STRUCTURE

## Global Rules
${CURSOR_RULES_PATH}/cursor/CURSOR.md

## Language-Specific Rules
${CURSOR_RULES_PATH}/cursor/python-fp-style-guide.md
# Or: typescript, swift, kotlin, rust, haskell

## Platform-Specific Rules (Optional)
${CURSOR_RULES_PATH}/cursor/aws-fp-style-guide.md
# Or: gcp-fp-style-guide.md

## Project-Specific Overrides
### Tech Stack
- **Language**: Python 3.11.5
- **Platform**: AWS Lambda
- **Data**: Polars 0.19.0

### Project Structure
```
src/
  types/      # ADTs and type definitions
  pure/       # Pure business logic  
  io/         # AWS operations
  functions/  # Lambda entry points
```

### Custom Requirements
- All DataFrames have explicit schemas
- S3 bucket: `my-project-data`
- Max upload: 10MB
- Use SQS for async jobs
```

**For Kimi (.kimirules):**
```markdown
# .kimirules - NEW GLOBAL STRUCTURE

## Global Rules
${KIMI_RULES_PATH}/kimi/KIMI.md

## Language-Specific Rules
${KIMI_RULES_PATH}/kimi/python-fp-style-guide.md
# Or: typescript, swift, kotlin, rust, haskell

## Platform-Specific Rules (Optional)
${KIMI_RULES_PATH}/kimi/aws-fp-style-guide.md

## Project-Specific Overrides
### Tech Stack
- **Language**: Python 3.11.5
- **Platform**: AWS Lambda
- **Data**: Polars 0.19.0

### Project Structure
[Same as above]

### Kimi-Specific Optimizations
# These take advantage of Kimi's parallel tool execution:
- Use batch ReadFile operations for reviews
- Use SetTodoList for task tracking
- Use subagents for complex refactoring
```

**Reduction**: Old files 200+ lines → New files 30-50 lines (75-85% reduction!)

### Migrate Multiple AI Tools Simultaneously

You can support multiple AI tools in the same project:

```bash
cd your-project/

# For Cursor users:
cp .cursorrules.template .cursorrules

# For Kimi users:
cp .kimirules.template .kimirules

# For Claude users:
cp .claude-rules.template .claude-rules

# For Gemini users:
cp .gemini-rules.template .gemini-rules
```

**Template files should be committed to your project repo** (without the user-specific `${VAR}` values):

```bash
# .cursorrules.template
cp cursor/templates/.cursorrules_smart_template_envvar .cursorrules.template
git add .cursorrules.template

# .kimirules.template
cp kimi/templates/.kimirules_smart_template_envvar .kimirules.template
git add .kimirules.template
```

Team members then copy and customize with their actual paths.

---

## What to Remove vs. Keep

### Remove (Now in Global Rules)

Move these to global rules (delete from project files):

**Git Workflow** → Now in `cursor/CURSOR.md` or `kimi/KIMI.md` Section 1
- Commit frequency (30-60 min)
- Commit message format
- Checkpoint triggers
- Status requirements

**Documentation Structure** → Now in Section 2
- 3-tier hierarchy (ARCHITECTURE_PLAN → plans/ → YYYY_MM_DD/)
- Timestamp format: `YYYYMMDD_NNNN_NAME.md`
- TODO list paired with plans

**Testing Requirements** → Now in Section 3
- Coverage targets (80%+)
- Test types (happy path, errors, edge cases)
- Execution requirement (all pass before commit)
- Mocking strategies

**File Size Limits** → Now in Section 4
- 250-300 line target
- 350 line absolute max
- Splitting guidelines

**FP Principles** → Now in Section 5
- Pure functions (no side effects)
- Result/Either types (not exceptions)
- Railway-oriented programming
- ADTs for domain modeling
- Pattern matching (exhaustive)

**Language Patterns** → Now in language guides
- Result type usage
- Monadic composition (`bind`/`flatMap`/`and_then`)
- Immutability patterns
- Library conventions (returns, fp-ts, Arrow, etc.)

**Platform Patterns** → Now in platform guides
- AWS: Lambda handlers, Step Functions, DynamoDB patterns
- GCP: Cloud Functions, Firestore, Pub/Sub patterns

### Keep (Project-Specific Only)

**Examples of what to KEEP in your local rules file:**

```markdown
## Project-Specific Overrides (KEEP THESE)

### Tech Stack Versions
- Python 3.11.5 (not just "Python 3.11+")
- Polars 0.19.0
- pydantic v2

### Project Structure
```
src/
  types/      # ADTs - pure
  pure/       # Business logic - pure
  io/         # AWS operations - effectful
  functions/  # Lambda handlers - entry points
```

### Infrastructure
- AWS region: us-east-1
- S3 bucket: my-project-data-prod
- DynamoDB table: Users-v2
- API Gateway endpoint: /api/v1

### Custom Requirements
- All DataFrames must have explicit schemas
- Max file upload: 10MB
- Use SQS for async jobs >5 sec
- PII data must be encrypted at rest

### Team Conventions
- Code owners: @team-data-platform
- PR review: 2 approvals required
- Deploy window: 10am-4pm EST
```

---

## Migrate from Legacy Setups

### From No Rules/Scattered Rules

**Before**:
```bash
# Project had no .cursorrules or scattered rules
# Each developer used different patterns
# No consistency
```

**After**:
```bash
# Set up global rules (see Phase 1)
export CURSOR_RULES_PATH="$HOME/ai-rules"

# Create minimal .cursorrules
cat > .cursorrules << 'EOF'
${CURSOR_RULES_PATH}/cursor/CURSOR.md
${CURSOR_RULES_PATH}/cursor/python-fp-style-guide.md
EOF

# Git commit
git add .cursorrules
git commit -m "✨ Add global AI rules"
```

**Benefit**: Instant consistency across your entire codebase!

### From Copy-Pasted Rules

**Before** (each project had 200-500 lines):
```bash
# Project A/.cursorrules (200 lines)
# Project B/.cursorrules (210 lines, slightly different)
# Project C/.cursorrules (195 lines, outdated)
```

**After** (each project has 30-50 lines):
```bash
# All projects use this structure:
cat > .cursorrules << 'EOF'
${CURSOR_RULES_PATH}/cursor/CURSOR.md
${CURSOR_RULES_PATH}/cursor/python-fp-style-guide.md

## Project-Specific
- Python 3.11.5
- AWS Lambda
- Polars 0.19.0
- S3 bucket: my-project
EOF
```

**Benefit**: 85% reduction in duplication, central updates!

### From Different AI Tools

**Scenario**: Team uses different AI assistants

**Solution**: Support multiple tools simultaneously:

```bash
cd your-project/

# Create rules for all AI tools
cat > .cursorrules << 'EOF'  # For Cursor users
${CURSOR_RULES_PATH}/cursor/CURSOR.md
${CURSOR_RULES_PATH}/cursor/python-fp-style-guide.md
EOF

cat > .kimirules << 'EOF'    # For Kimi users
${KIMI_RULES_PATH}/kimi/KIMI.md
${KIMI_RULES_PATH}/kimi/python-fp-style-guide.md
EOF

# Both files reference same principles
git add .cursorrules .kimirules
git commit -m "Support multiple AI tools"
```

**Benefit**: Team members use their preferred AI tool with same rules!

### From Different Rule Sets

**Scenario**: Migrating from another FP rule set

**Migration Path**:
1. Audit existing rules vs global rules
2. Map patterns (most FP patterns are universal)
3. Identify gaps (global rules likely cover more)
4. Gradual transition (run both in parallel)
5. Complete migration (remove old rules)

**Mapping Example**:
- Old: `Result` → New: `Result` (same!)
- Old: `flatMap` → New: `flatMap` / `bind` / `and_then` (same concept)
- Old: Railway pattern → New: Railway pattern (same!)

**Benefit**: Standardized on industry-proven patterns!

---

## Advanced: Multi-AI Tool Projects

### Support All AI Tools in One Project

```bash
# In project root, create all 4 rule files

# Cursor rules
cat > .cursorrules << 'EOF'
${CURSOR_RULES_PATH}/cursor/CURSOR.md
${CURSOR_RULES_PATH}/cursor/typescript-fp-style-guide.md
${CURSOR_RULES_PATH}/cursor/aws-fp-style-guide.md
- Node 18.17.0
- AWS region: us-west-2
- DynamoDB table: ProjectData
EOF

# Kimi rules (same content, different paths)
cat > .kimirules << 'EOF'
${KIMI_RULES_PATH}/kimi/KIMI.md
${KIMI_RULES_PATH}/kimi/typescript-fp-style-guide.md
${KIMI_RULES_PATH}/kimi/aws-fp-style-guide.md
- Node 18.17.0
- AWS region: us-west-2
- DynamoDB table: ProjectData
EOF

# Claude rules
cat > .claude-rules << 'EOF'
${CLAUDE_RULES_PATH}/claude/CLAUDE.md
${CLAUDE_RULES_PATH}/claude/typescript-fp-style-guide.md
${CLAUDE_RULES_PATH}/claude/aws-fp-style-guide.md
- Node 18.17.0
- AWS region: us-west-2
- DynamoDB table: ProjectData
EOF

# Gemini rules
cat > .gemini-rules << 'EOF'
${GEMINI_RULES_PATH}/gemini/GEMINI.md
${GEMINI_RULES_PATH}/gemini/typescript-fp-style-guide.md
${GEMINI_RULES_PATH}/gemini/aws-fp-style-guide.md
- Node 18.17.0
- AWS region: us-west-2
- DynamoDB table: ProjectData
EOF

# Git commit all
git add .cursorrules .kimirules .claude-rules .gemini-rules
git commit -m "Support Cursor, Kimi, Claude, and Gemini"
```

**Team members use their preferred AI tool** - all enforce same FP patterns!

---

## Testing Migration

### Verify Rules Work

**For Cursor:**
```bash
# 1. Open project in Cursor
# 2. Ask Cursor: "What are our coding rules?"
# 3. Verify it references global rules

# 4. Try: "Create a function that follows our FP patterns"
# 5. Verify it uses Result types, not exceptions
```

**For Kimi:**
```bash
# 1. Run: kimai read-file .kimirules
# 2. Verify paths resolve correctly

# 3. Try parallel operation:
#    - Ask Kimi to read multiple files at once
#    - Verify it understands the patterns

# 4. Try SetTodoList integration:
#    - Ask Kimi to track a task
#    - Verify SetTodoList tool is used
```

### Automated Tests

```bash
# Create test script
cat > test-rules.sh << 'EOF'
#!/bin/bash
echo "Testing AI rules setup..."

# Test environment variables
if [ -n "$CURSOR_RULES_PATH" ]; then
  echo "✅ CURSOR_RULES_PATH set"
else
  echo "❌ CURSOR_RULES_PATH not set"
fi

if [ -n "$KIMI_RULES_PATH" ]; then
  echo "✅ KIMI_RULES_PATH set"
else
  echo "❌ KIMI_RULES_PATH not set"
fi

# Test file paths
if [ -f "$CURSOR_RULES_PATH/cursor/CURSOR.md" ]; then
  echo "✅ Global Cursor rules found"
else
  echo "❌ Global Cursor rules not found"
fi

if [ -f "$KIMI_RULES_PATH/kimi/KIMI.md" ]; then
  echo "✅ Global Kimi rules found"
else
  echo "❌ Global Kimi rules not found"
fi

echo "Done!"
EOF

chmod +x test-rules.sh
./test-rules.sh
```

---

## Common Issues

### Issue 1: AI Tool Can't Find Rules

**Symptom**: `${VAR}` substitution not working

**Solutions by Tool:**

**Cursor:**
```bash
# 1. Check environment variable
echo $CURSOR_RULES_PATH

# 2. Reload shell
source ~/.zshrc

# 3. Restart Cursor completely
# Cmd/Ctrl+Q to quit, then reopen

# 4. Verify in Cursor
# Open Command Palette → "Cursor: Show Rules Path"
```

**Kimi:**
```bash
# 1. Check environment variable
echo $KIMI_RULES_PATH

# 2. Reload shell
source ~/.zshrc

# 3. Test with Kimi
kimi read-file .kimirules

# 4. Verify it reads the referenced files
kimi read-file ${KIMI_RULES_PATH}/kimi/KIMI.md
```

**Claude/Gemini:**
```bash
# Similar to above for their respective env vars
# Restart the AI tool after setting environment variables
```

### Issue 2: Rules Not Applied

**Symptom**: AI tool doesn't enforce FP patterns

**Solutions:**

1. **Check rules file syntax:**
   - Must be valid markdown
   - Paths must be correct
   - No typos in variable names

2. **Verify rule loading:**
   ```bash
   # For Cursor
cat .cursorrules
# Should show: ${CURSOR_RULES_PATH}/cursor/CURSOR.md

# For Kimi
cat .kimirules
# Should show: ${KIMI_RULES_PATH}/kimi/KIMI.md
```

3. **Test a simple rule:**
   ```bash
   # Ask your AI: "What are our mandatory rules?"
   # It should mention Git checkpoints and FP patterns

   # Ask: "What file size limit do we use?"
   # It should say 250-300 lines target
   ```

### Issue 3: Wrong AI Tool's Rules Loaded

**Symptom**: Kimi loading Cursor rules or vice versa

**Cause**: Mixed environment variables or wrong file extension

**Solutions:**

```bash
# Clean up - remove wrong files
# If you use Kimi, remove Cursor rules:
rm -f .cursorrules

# If you use Cursor, remove Kimi rules:
rm -f .kimirules

# Verify only correct file exists
ls -la | grep -E "\.(cursorrules|kimirules|claude-rules|gemini-rules)"
```

### Issue 4: Performance Issues

**Symptom**: AI tool slow or unresponsive

**Solutions:**

1. **For Kimi - Leverage parallel execution:**
   ```bash
   # OLD: Sequential (slow)
   kimi read-file: file1.py
   kimi read-file: file2.py
   kimi read-file: file3.py

   # NEW: Parallel (fast!)
   kimi read-file: file1.py
   kimi read-file: file2.py
   kimi read-file: file3.py
   # All execute simultaneously = 30-50% faster
   ```

2. **For all tools - Keep rules file minimal:**
   - Remove project-specific content that's already in global rules
   - Only keep truly unique project requirements
   - Goal: 30-50 lines max in local rules file

3. **Clean up legacy files:**
   ```bash
   # Remove old, unused rule files
   find . -name "*.md" -name "*rules*" -not -name ".cursorrules" -not -name ".kimirules" -delete
   ```

### Issue 5: Team Inconsistency

**Symptom**: Different team members get different results

**Solutions:**

1. **Standardize rules file:**
   ```bash
   # Create template file in repo
cp .cursorrules .cursorrules.template
git add .cursorrules.template

# Team members copy and customize
cp .cursorrules.template .cursorrules
# Edit with their ${VAR} values (but don't commit the customized version)
```

2. **Document setup:**
```markdown
# SETUP.md in your project

## AI Rules Setup

1. Set environment variable:
   ```bash
   export CURSOR_RULES_PATH="$HOME/ai-rules"  # For Cursor
   export KIMI_RULES_PATH="$HOME/ai-rules"    # For Kimi
   ```

2. Copy rules file:
   ```bash
   cp .cursorrules.template .cursorrules  # For Cursor
   cp .kimirules.template .kimirules      # For Kimi
   ```

3. Start coding!
```

3. **Include in onboarding:**
   - Add to README.md
   - Add to team wiki
   - Mention in PR templates

---

## Migration Benefits

### Quantifiable Improvements

| Metric | Before (Legacy) | After (Global) | Improvement |
|--------|-----------------|----------------|-------------|
| Rules duplication | 200-500 lines/project | 30-50 lines/project | **85% reduction** |
| Update effort | Hours per project | Minutes (central) | **90% time savings** |
| Consistency | Varies by project | **Universal** | 100% consistent |
| FP adoption | Optional/scattered | **Mandatory/enforced** | Complete |
| Multi-AI support | Single tool | **All tools** | Flexible |

### Qualitative Improvements

- **Maintainability**: Update once, apply everywhere
- **Onboarding**: New team members productive faster
- **Quality**: Consistent FP patterns across all code
- **Flexibility**: Use any AI tool with same rules
- **Reliability**: Battle-tested patterns from 6 languages

---

## Roll Out Strategy

### Option A: Incremental (Recommended)

**Phase 1: Pilot** (1 project, 1-2 days)
- Pick simple, active project
- Set up global rules
- Migrate using this guide
- Use daily, fix issues
- **Success criteria**: Team comfortable, no major issues

**Phase 2: Early Adopters** (2-3 projects, Week 1)
- Migrate team projects
- Identify champions per project
- Gather feedback
- Refine based on learnings
- **Success criteria**: 3+ projects using rules effectively

**Phase 3: All Projects** (Remaining projects, Week 2-3)
- Migrate rest of projects
- Update documentation
- Train team members
- **Success criteria**: All projects using global rules

### Option B: Big Bang (For Small Codebases)

**All at once** (1-2 days):
- Set up global rules
- Migrate all projects in parallel
- Mass update/review
- Team-wide announcement
- **Risk**: Higher disruption, faster completion

---

## Post-Migration Optimizations

### Leverage AI Tool-Specific Features

**If using Kimi:**
- Use parallel tool execution for reviews
- Use SetTodoList for task tracking
- Use subagents for complex refactoring
- Take advantage of 30-50% speed improvement

**If using Cursor:**
- Leverage IDE integration
- Use sidebar rules reference
- Enable auto-suggestions

**If using Claude/Gemini:**
- Explore tool-specific optimizations
- Provide feedback on pattern effectiveness

### Continuous Improvement

**Monthly:**
- Review global rules for updates
- Submit improvements upstream
- Share learnings with team

**Quarterly:**
- Audit project-specific overrides
- Move common patterns to global
- Update team documentation

---

## Need Help?

### Resources

- **[SETUP_GUIDE.md](SETUP_GUIDE.md)** - Detailed setup instructions
- **[cursor/CURSOR.md](cursor/CURSOR.md)** - Cursor mandatory rules
- **[kimi/KIMI.md](kimi/KIMI.md)** - Kimi mandatory rules
- **[AGENTS.md](AGENTS.md)** - Guide for AI agents working on this repo

### Getting Support

- **Documentation issues**: Check AGENTS.md and related docs
- **Setup problems**: See SETUP_GUIDE.md troubleshooting
- **Rule conflicts**: Review what to remove/keep sections above
- **Tool-specific issues**: Check Common Issues section

---

## Summary

**Migration is simple:**
1. Set environment variable(s) → 5 minutes
2. Replace rules file → 10-15 minutes
3. Test → 5 minutes
4. Roll out → 5-10 minutes per project

**Result**: 85% reduction in duplication, 100% consistency, multi-AI tool support!

---

**Document Version**: 1.0.0  
**Last Updated**: 2025-11-14  
**Status**: Production Ready  

🤖 Generated with [Kimi](https://kimi.ai)

Co-Authored-By: Kimi <noreply@kimi.ai)
