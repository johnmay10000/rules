---
title: Documentation Organization and Standards
+category: universal_rules
++type: documentation
++applies_to: all
++version: 1.0.0
++last_updated: 2025-11-19
++---
+++
++# Documentation Organization and Standards
+++
+++**Status**: MANDATORY - All documentation must follow this structure
+++
+++Universal documentation organization rules applicable to all projects, regardless of AI assistant (Cursor, Kimi, Claude, Gemini) or technology stack.
+++
+++---
+++
+++## Core Principles
+++
+++### 1. Three-Tier Documentation Hierarchy
+++
+++All documentation **MUST** follow the three-tier structure:
+++
+++```
+++Tier 1: Strategic (Project-Level)
+++├── docs/ARCHITECTURE_PLAN.md          # Overall system architecture
+++├── docs/PRODUCT_ROADMAP.md            # Product direction and milestones
+++└── docs/TECHNICAL_VISION.md           # Long-term technical vision
+++
+++Tier 2: Tactical (Feature/Module-Level)
+++├── docs/plans/FEATURE_X_PLAN.md       # Feature-specific plan
+++├── docs/plans/FEATURE_X_TODO.md       # Feature-specific TODO
+++└── docs/plans/MIGRATION_PLAN.md       # Migration/refactoring plan
+++
+++Tier 3: Execution (Day-to-Day Work)
+++├── docs/2025_11_19/20251119_0000_TASK_DESCRIPTION.md
+++├── docs/2025_11_19/20251119_0001_SESSION_SUMMARY.md
+++└── docs/2025_11_19/20251119_0002_DECISION_LOG.md
+++```
+++
+++**Rule**: No documentation exists outside this hierarchy. Every document has a clear home.
+++
+++---
+++
+++### 2. Daily Work Folders (MANDATORY)
+++
+++**Location**: `docs/YYYY_MM_DD/`
+++
+++**Purpose**: All day-to-day work, notes, and incremental progress
+++
+++**Naming Convention**: `YYYYMMDD_NNNN_DESCRIPTION.md`
+++
+++Where:
+++- `YYYY` = 4-digit year (2025)
+++- `MM` = 2-digit month (11)
+++- `DD` = 2-digit day (19)
+++- `NNNN` = 4-digit sequence number (0000, 0001, 0002, ...)
+++- `DESCRIPTION` = kebab-case description
+++
+++**Examples**:
+++```
+++docs/2025_11_19/
+++├── 20251119_0000_CURSOR_MD_IMPLEMENTATION.md
+++├── 20251119_0001_MCP_SERVER_SETUP.md
+++├── 20251119_0002_SESSION_SUMMARY.md
+++├── 20251119_0003_BUG_FIX_VALIDATION.md
+++└── 20251119_0004_PHASE_2_COMPLETE.md
+++```
+++
+++**Sequence Number Rules**:
+++- Start at `0000` for each new day
+++- Increment by 1 for each new document
+++- Never reuse numbers
+++- Never skip numbers
+++- Sequence reflects chronological order
+++
+++---
+++
+++### 3. Plans Folder (MANDATORY)
+++
+++**Location**: `docs/plans/`
+++
+++**Purpose**: Strategic planning documents and paired TODO lists
+++
+++**Structure**: Every plan has TWO files:
+++- `FEATURE_NAME_PLAN.md` - The strategic plan
+++- `FEATURE_NAME_TODO.md` - The tactical TODO list
+++
+++**Examples**:
+++```
+++docs/plans/
+++├── MCP_IMPLEMENTATION_PLAN.md
+++├── MCP_IMPLEMENTATION_TODO.md
+++├── RULES_REFACTOR_PLAN.md
+++├── RULES_REFACTOR_TODO.md
+++├── AWS_MIGRATION_PLAN.md
+++└── AWS_MIGRATION_TODO.md
+++```
+++
+++**PLAN.md Contents**:
+++- Executive summary
+++- Goals and objectives
+++- Architecture/approach
+++- Timeline and milestones
+++- Risks and mitigation
+++- Success criteria
+++
+++**TODO.md Contents**:
+++- Task list with checkboxes
+++- Time estimates vs. actual
+++- Progress tracking
+++- Blockers and dependencies
+++- Update history
+++
+++---
+++
+++### 4. Templates Folder
+++
+++**Location**: `templates/`
+++
+++**Purpose**: Reusable document templates
+++
+++**Organization**:
+++```
+++templates/
+++├── daily_work/
+++│   └── daily_session_template.md
+++├── plans/
+++│   ├── plan_template.md
+++│   └── todo_template.md
+++├── project_setup/
+++│   ├── README_template.md
+++│   └── CONTRIBUTING_template.md
+++└── ai_tool_specific/
+++    ├── cursor/
+++    │   └── .cursorrules_template.md
+++    └── kimi/
+++        └── .kimirules_template.md
+++```
+++
+++**Usage**:
+++1. Copy template to appropriate location
+++2. Fill in project-specific details
+++3. Follow template structure exactly
+++4. Update metadata (title, dates, version)
+++
+++---
+++
+++### 5. Examples Folder
+++
+++**Location**: `examples/`
+++
+++**Purpose**: Real-world usage examples
+++
+++**Organization by Language**:
+++```
+++examples/
+++├── python/
+++│   ├── basic_fp_patterns.py
+++│   └── error_handling_example.py
+++├── typescript/
+++│   ├── either_patterns.ts
+++│   └── option_example.ts
+++└── rust/
+++    ├── result_patterns.rs
+++    └── iterator_examples.rs
+++```
+++
+++**Requirements**:
+++- Examples must be runnable code
+++- Examples must include comments explaining patterns
+++- Examples must be tested (examples are not exempt from testing)
+++- Update examples when patterns change
+++
+++---
+++
+++## File Naming Conventions
+++
+++### Document Files
+++
+++**Format**: `kebab-case-description.md`
+++
+++**Examples**:
+++- ✅ `git-checkpoint-rules.md`
+++- ✅ `testing-philosophy.md`
+++- ✅ `mcp-implementation-plan.md`
+++- ❌ `GitCheckpointRules.md` (no camelCase)
+++- ❌ `testing_philosophy.md` (no snake_case)
+++- ❌ `MCP Implementation Plan.md` (no spaces)
+++
+++### Daily Work Files
+++
+++**Format**: `YYYYMMDD_NNNN_DESCRIPTION.md`
+++
+++**Examples**:
+++- ✅ `20251119_0000_CURSOR_MD_SETUP.md`
+++- ✅ `20251119_0001_SESSION_SUMMARY.md`
+++- ❌ `2025-11-19-task.md` (wrong format)
+++- ❌ `0000_20251119_task.md` (wrong order)
+++
+++### Plan Files
+++
+++**Format**: `FEATURE_NAME_PLAN.md` and `FEATURE_NAME_TODO.md`
+++
+++**Examples**:
+++- ✅ `MCP_IMPLEMENTATION_PLAN.md`
+++- ✅ `MCP_IMPLEMENTATION_TODO.md`
+++- ❌ `MCP-PLAN.md` (too vague)
+++- ❌ `plan.md` (not descriptive)
+++
+++---
+++
+++## Documentation Metadata
+++
+++### Required Frontmatter
+++
+++Every documentation file **MUST** include:
+++
+++```yaml
+++---
+++title: Descriptive Title
+++category: [project_structure | documentation | testing | git | ai_tool_usage]
+++type: [plan | todo | guide | reference | decision]
+++applies_to: [all | cursor | kimi | claude | gemini]
+++version: 1.0.0
+++last_updated: YYYY-MM-DD
+++---
+++```
+++
+++**Example**:
+++```yaml
+++---
+++title: MCP Implementation Plan
+++category: project_structure
+++type: plan
+++applies_to: all
+++version: 1.0.0
+++last_updated: 2025-11-19
+++---
+++```
+++
+++**Validation**:
+++- CI pipeline should validate all docs have required metadata
+++- Missing metadata = failed build
+++
+++---
+++
+++## File Size Limits
+++
+++### Maximum Size: 300 Lines
+++
+++**Rule**: No documentation file may exceed 300 lines.
+++
+++**Rationale**:
+++- Large files are hard to navigate
+++- Large files are hard to review
+++- Large files encourage poor organization
+++- Smaller files are more maintainable
+++
+++### What If More Content Is Needed?
+++
+++**Solution**: Split into multiple files
+++
+++**Example**:
+++```
+++# Instead of one 500-line file:
+++docs/plans/MCP_IMPLEMENTATION_PLAN.md
+++
+++# Use multiple smaller files:
+++docs/plans/MCP_IMPLEMENTATION_PLAN.md          # Overview (200 lines)
+++docs/plans/MCP_IMPLEMENTATION_TECHNICAL.md     # Technical details (150 lines)
+++docs/plans/MCP_IMPLEMENTATION_TIMELINE.md      # Timeline (100 lines)
+++docs/plans/MCP_IMPLEMENTATION_RISKS.md          # Risk analysis (100 lines)
+++```
+++
+++**How to Split**:
+++- By topic (technical, timeline, risks)
+++- By audience (developers, managers, users)
+++- By phase (phase 1, phase 2, phase 3)
+++- By component (frontend, backend, database)
+++
+++---
+++
+++## Cross-Referencing
+++
+++### Internal Links
+++
+++**Format**: Use relative paths with `.md` extension
+++
+++**Examples**:
+++```markdown
+++See [Testing Philosophy](../testing/testing_philosophy.md) for more details.
+++
+++See [Git Checkpoint Rules](../git/git_checkpoint_rules.md) for commit guidelines.
+++
+++See [Python FP Guide](../../code_guidelines/languages/python/fp_style_guide.md) for language-specific patterns.
+++```
+++
+++**Rules**:
+++- Always use relative paths (not absolute)
+++- Include `.md` extension
+++- Use `../` to go up directories
+++- Test links work (CI should check for broken links)
+++
+++### External Links
+++
+++**Format**: Use full URLs with descriptive text
+++
+++**Examples**:
+++```markdown
+++For more information, see [Kimi Documentation](https://kimi.ai/docs).
+++
+++Learn about [fp-ts](https://gcanti.github.io/fp-ts/) for TypeScript FP.
+++```
+++
+++**Rules**:
+++- Use descriptive link text (not "click here")
+++- Link to official documentation when possible
+++- Avoid linking to ephemeral content (tweets, temporary pages)
+++- Check links periodically (automated link checking in CI)
+++
+++---
+++
+++## Documentation Workflow
+++
+++### Creating New Documentation
+++
+++**Steps**:
+++
+++1. **Determine tier** (Strategic, Tactical, or Execution)
+++2. **Choose location** based on tier
+++   - Tier 1: `docs/` root
+++   - Tier 2: `docs/plans/`
+++   - Tier 3: `docs/YYYY_MM_DD/`
+++3. **Copy appropriate template** from `templates/`
+++4. **Fill in metadata** (title, category, type, version, date)
+++5. **Write content** following template structure
+++6. **Add cross-references** to related documents
+++7. **Review and verify** before committing
+++8. **Commit with proper message** (see Git Checkpoint Rules)
+++
+++### Updating Existing Documentation
+++
+++**Steps**:
+++
+++1. **Read current document** to understand context
+++2. **Make changes** incrementally
+++3. **Update metadata** (increment version, update last_updated)
+++4. **Update related documents** if changes affect them
+++5. **Git checkpoint** after significant updates
+++6. **Create new daily work file** if update is substantial
+++
+++### Deprecating Documentation
+++
+++**Process**:
+++
+++1. **Add deprecation notice** at top of file:
+++   ```markdown
+++   > **⚠️ DEPRECATED**: This document is deprecated as of YYYY-MM-DD.
+++   > See [NEW_DOCUMENT.md](./NEW_DOCUMENT.md) for current guidance.
+++   ```
+++2. **Update references** to point to new document
+++3. **Remove from indexes** (if applicable)
+++4. **Schedule deletion** (3-6 months after deprecation)
+++5. **Git checkpoint** with deprecation notice
+++
+++---
+++
+++## Review and Approval
+++
+++### Documentation Review Process
+++
+++**Tier 1 Documents** (Strategic):
+++- Requires: 2+ reviewers
+++- Reviewers: Tech lead + architect + product manager
+++- Approval: All reviewers must approve
+++- Timeline: 3-5 business days
+++
+++**Tier 2 Documents** (Tactical):
+++- Requires: 1-2 reviewers
+++- Reviewers: Senior engineer + tech lead
+++- Approval: Majority approval
+++- Timeline: 1-3 business days
+++
+++**Tier 3 Documents** (Execution):
+++- Requires: 1 reviewer (peer review)
+++- Reviewers: Team member or tech lead
+++- Approval: Single approval sufficient
+++- Timeline: Same day
+++
+++### Review Checklist
+++
+++Reviewers must verify:
+++
+++- [ ] Document follows naming conventions
+++- [ ] Metadata is complete and accurate
+++- [ ] Content is clear and well-structured
+++- [ ] File size is under 300 lines (or properly split)
+++- [ ] Cross-references are correct and working
+++- [ ] Examples are accurate and runnable
+++- [ ] No typos or grammatical errors
+++- [ ] Document is in correct location (tier-appropriate)
+++- [ ] Related documents are updated if needed
+++
+++---
+++
+++## Documentation Quality Metrics
+++
+++### Metrics to Track
+++
+++1. **Documentation Coverage**: % of features with docs
+++   - Target: 100% of public APIs
+++   - Target: 100% of critical user workflows
+++
+++2. **Freshness**: % of docs updated in last 90 days
+++   - Target: 90% of active docs
+++
+++3. **Accuracy**: % of docs with verified examples
+++   - Target: 100% (all examples tested)
+++
+++4. **Broken Links**: Number of dead internal links
+++   - Target: 0 broken links
+++
+++5. **Review Coverage**: % of docs reviewed in last 180 days
+++   - Target: 100% of Tier 1-2 docs
+++
+++### Automated Checks
+++
+++CI pipeline should verify:
+++
+++- All docs have required metadata
+++- No file exceeds 300 lines
+++- No broken internal links
+++- All code examples are valid syntax
+++- Documentation follows naming conventions
+++
+++---
+++
+++## Tool-Specific Notes
+++
+++### For Kimi Users
+++
+++Kimi provides enhanced documentation support:
+++- **Parallel Document Generation**: Create multiple docs simultaneously
+++- **SetTodoList Integration**: Track documentation tasks
+++- **Cross-Reference Validation**: Verify links automatically
+++- **Template Population**: Fill templates with context-aware content
+++
+++**Kimi-Specific Commands**:
+++```bash
+++# Generate documentation from code
+++kimi doc-generate src/module.py --output docs/
+++
+++# Validate all cross-references
+++kimi doc-validate --check-links
+++
+++# Create daily work document
+++kimi doc-daily-work --template daily_session
+++```
+++
+++### For Cursor Users
+++
+++Cursor provides IDE-integrated documentation:
+++- **Inline Editing**: Edit docs directly in editor
+++- **Preview Mode**: See rendered markdown
+++- **Link Validation**: Check links as you type
+++- **Template Integration**: Use templates from IDE
+++
+++### For Claude Users
+++
+++Claude excels at documentation strategy:
+++- **Documentation Architecture**: Design doc hierarchies
+++- **Content Organization**: Structure complex documentation
+++- **Technical Writing**: Generate clear, concise docs
+++- **Review and Refinement**: Improve existing documentation
+++
+++### For Gemini Users
+++
+++Gemini provides comprehensive documentation guidance:
+++- **Best Practices**: Documentation methodologies
+++- **Multi-Language**: Doc patterns across languages
+++- **Accessibility**: Make docs accessible to all audiences
+++- **Search Optimization**: Make docs discoverable
+++
+++---
+++
+++## Related Documents
+++
+++- **Naming Conventions**: `naming_conventions.md` - File and variable naming
+++- **Daily Workflow**: `daily_workflow.md` - Day-to-day documentation process
+++- **Git Checkpoint Rules**: `../git/git_checkpoint_rules.md` - When to commit docs
+++- **Testing Requirements**: `../testing/testing_philosophy.md` - Test documentation
+++
+++---
+++
+++**Last Updated**: 2025-11-19  
+++**Maintained By**: Global AI Rules System  
+++**Status**: Active  
+++**Applies To**: All AI assistants (Cursor, Kimi, Claude, Gemini) and all human developers