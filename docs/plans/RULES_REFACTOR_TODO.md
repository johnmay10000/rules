# Rules Repository Refactoring TODO List

**Project**: Unified Rules Structure Refactoring
**Type**: Task Tracking & Progress Management
**Version**: 1.0.0
**Status**: In Progress
**Date**: 2025-11-19
**Paired With**: [RULES_REFACTOR_PLAN.md](./RULES_REFACTOR_PLAN.md)

---

## 📊 Progress Overview

- **Total Tasks**: 68
- **Completed**: 23 ✅
- **In Progress**: 0 🔄
- **Pending**: 45 ⏳
- **Progress**: 34%

**Estimated Time**: 6 hours (1 day)
**Time Spent**: 2.0 hours
**Time Remaining**: 4.0 hours

**Current Phase**: Phase 2 - Code Guidelines Migration (Ready to start!)

---

## 🏗️ Phase 1: Foundation Setup

**Status**: ⏳ PENDING
**Estimated**: 1 hour
**Priority**: HIGH

### Directory Structure Creation

- [x] **TASK-1.1**: Create `code_guidelines/` root directory
  - Action: `mkdir -p code_guidelines`
  - Verify: Directory created at repository root
  - **Actual**: 0.1 hours ✅

- [x] **TASK-1.2**: Create language subdirectories
  - Action: Create `code_guidelines/languages/`
  - Create subdirs: python, typescript, rust, kotlin, swift, haskell
  - Verify: All 6 language directories exist
  - **Actual**: 0.1 hours ✅

- [x] **TASK-1.3**: Create `universal_rules/` root directory
  - Action: `mkdir -p universal_rules`
  - Create subdirs: git, testing, documentation, project_structure, ai_tool_usage
  - Verify: All 5 category directories exist
  - **Actual**: 0.1 hours ✅

- [x] **TASK-1.4**: Create `ai_tool_specific/` root directory
  - Action: `mkdir -p ai_tool_specific`
  - Create subdirs: cursor, kimi, claude, gemini
  - Verify: All 4 tool directories exist
  - **Actual**: 0.1 hours ✅

- [x] **TASK-1.5**: Create migration script template
  - File: `scripts/migrate_rules.js`
  - Function: Helper functions for moving files
  - Include: Path resolution, metadata extraction
  - **Actual**: 0.2 hours ✅

- [x] **TASK-1.6**: Update `.cursorrules` with new structure
  - Add: Paths to new directories
  - Add: Rules for organizing content
  - Add: Metadata requirements
  - **Actual**: 0.2 hours ✅

- [x] **TASK-1.7**: Update repository README.md
  - Section: New directory structure overview
  - Section: How to navigate the repository
  - Section: Quick start guide
  - **Actual**: 0.2 hours ✅

**Phase 1 Tasks**: 7/7 ✅
**Phase 1 Progress**: 100%
**Phase 1 Actual**: 1 hour
**Phase 1 Status**: COMPLETE

---

## 📚 Phase 2: Code Guidelines Migration

**Status**: ⏳ PENDING
**Estimated**: 1.5 hours
**Priority**: HIGH
**Depends On**: Phase 1 complete

### Python Guidelines Migration

- [x] **TASK-2.1**: Analyze existing Python guidelines across tools
  - Read: `cursor/python-fp-style-guide.md`
  - Read: `kimi/python-fp-style-guide.md`
  - Read: `claude/python-fp-style-guide.md`
  - Read: `gemini/python-fp-style-guide.md`
  - Document: Differences and unique content per tool
  - **Actual**: 0.2 hours ✅

- [x] **TASK-2.2**: Create unified Python guidelines
  - File: `code_guidelines/languages/python/fp_style_guide.md`
  - Merge: All common content into single file
  - Add: Frontmatter metadata
  - Add: Table of contents
  - **Actual**: 0.2 hours ✅

- [x] **TASK-2.3**: Extract tool-specific Python notes
  - File: `ai_tool_specific/kimi/kimi_python_features.md`
  - File: `ai_tool_specific/cursor/cursor_python_features.md`
  - Include: Only truly unique tool features
  - **Actual**: 0.1 hours ✅

### TypeScript Guidelines Migration

- [x] **TASK-2.4**: Analyze existing TypeScript guidelines across tools
  - Read all tool versions
  - Document differences
  - **Actual**: 0.2 hours ✅

- [x] **TASK-2.5**: Create unified TypeScript guidelines
  - File: `code_guidelines/languages/typescript/fp_style_guide.md`
  - Merge common content
  - Add metadata
  - **Actual**: 0.2 hours ✅

- [x] **TASK-2.6**: Extract tool-specific TypeScript notes
  - Create tool-specific files
  - Include unique features only
  - **Actual**: 0.1 hours ✅

### Rust Guidelines Migration

- [x] **TASK-2.7**: Analyze existing Rust guidelines across tools
  - Read all tool versions
  - Document differences
  - **Actual**: 0.2 hours ✅

- [x] **TASK-2.8**: Create unified Rust guidelines
  - File: `code_guidelines/languages/rust/fp_style_guide.md`
  - Merge common content
  - Add metadata
  - **Actual**: 0.2 hours ✅

- [x] **TASK-2.9**: Extract tool-specific Rust notes
  - Create tool-specific files
  - **Actual**: 0.1 hours ✅

### Kotlin, Swift, Haskell Guidelines Migration

- [x] **TASK-2.10**: Migrate Kotlin guidelines
  - Analyze, unify, extract tool-specific notes
  - File: `code_guidelines/languages/kotlin/fp_style_guide.md`
  - **Actual**: 0.2 hours ✅

- [x] **TASK-2.11**: Migrate Swift guidelines
  - Analyze, unify, extract tool-specific notes
  - File: `code_guidelines/languages/swift/fp_style_guide.md`
  - **Actual**: 0.2 hours ✅

- [x] **TASK-2.12**: Migrate Haskell guidelines
  - Analyze, unify, extract tool-specific notes
  - File: `code_guidelines/languages/haskell/fp_style_guide.md`
  - **Actual**: 0.2 hours ✅

### Supporting Files

- [x] **TASK-2.13**: Create language-specific patterns files
  - For each language: `patterns.md`
  - Extract pattern examples from style guides
  - **Actual**: 0.2 hours ✅

- [x] **TASK-2.14**: Create language-specific examples files
  - For each language: `examples.md`
  - Include before/after code examples
  - **Actual**: 0.2 hours ✅

- [x] **TASK-2.15**: Create language-specific libraries files
  - For each language: `libraries.md`
  - List recommended FP libraries
  - **Actual**: 0.2 hours ✅

- [x] **TASK-2.16**: Create code guidelines index
  - File: `code_guidelines/index.json`
  - Include: All languages, file paths, metadata
  - Purpose: Fast MCP server lookup
  - **Actual**: 0.1 hours ✅

**Phase 2 Tasks**: 16/16 ✅
**Phase 2 Progress**: 100%
**Phase 2 Actual**: 1.5 hours
**Phase 2 Status**: COMPLETE

---

## 🌐 Phase 3: Universal Rules Extraction

**Status**: ⏳ PENDING
**Estimated**: 1 hour
**Priority**: HIGH
**Depends On**: Phase 2 complete

### Git Rules Extraction

- [ ] **TASK-3.1**: Extract git checkpoint rules
  - Source: `kimi/KIMI.md`, `cursor/CURSOR.md`
  - Extract: Checkpoint requirements, commit format
  - File: `universal_rules/git/git_checkpoint_rules.md`
  - **Estimated**: 0.1 hours

- [ ] **TASK-3.2**: Extract commit conventions
  - Source: All tool files
  - Extract: Commit message format, conventional commits
  - File: `universal_rules/git/commit_conventions.md`
  - **Estimated**: 0.1 hours

- [ ] **TASK-3.3**: Extract branching strategy
  - File: `universal_rules/git/branching_strategy.md`
  - **Estimated**: 0.1 hours

### Testing Rules Extraction

- [ ] **TASK-3.4**: Extract testing philosophy
  - Source: All tool files
  - Extract: Testing approach, TDD, coverage requirements
  - File: `universal_rules/testing/testing_philosophy.md`
  - **Estimated**: 0.1 hours

- [ ] **TASK-3.5**: Extract unit testing rules
  - File: `universal_rules/testing/unit_testing.md`
  - **Estimated**: 0.1 hours

- [ ] **TASK-3.6**: Extract integration testing rules
  - File: `universal_rules/testing/integration_testing.md`
  0.1 hours

- [ ] **TASK-3.7**: Extract test coverage requirements
  - File: `universal_rules/testing/test_coverage_requirements.md`
  - Include: Coverage thresholds (80%), enforcement
  - **Estimated**: 0.1 hours

### Documentation Rules Extraction

- [ ] **TASK-3.8**: Extract file organization rules
  - Source: All tool files
  - Extract: Daily work folders, naming conventions
  - File: `universal_rules/documentation/file_organization.md`
  - **Estimated**: 0.1 hours

- [ ] **TASK-3.9**: Extract naming conventions
  - File: `universal_rules/documentation/naming_conventions.md`
  - **Estimated**: 0.1 hours

- [ ] **TASK-3.10**: Extract daily workflow rules
  - File: `universal_rules/documentation/daily_workflow.md`
  - **Estimated**: 0.1 hours

### Project Structure Rules Extraction

- [ ] **TASK-3.11**: Extract directory layout rules
  - File: `universal_rules/project_structure/directory_layout.md`
  - **Estimated**: 0.1 hours

- [ ] **TASK-3.12**: Extract file size limit rules
  - File: `universal_rules/project_structure/file_size_limits.md`
  - Include: 250-300 line limit enforcement
  - **Estimated**: 0.1 hours

- [ ] **TASK-3.13**: Extract template standards
  - File: `universal_rules/project_structure/template_standards.md`
  - **Estimated**: 0.1 hours

### AI Tool Usage Rules Extraction

- [ ] **TASK-3.14**: Extract MCP protocol rules
  - File: `universal_rules/ai_tool_usage/mcp_protocol.md`
  - **Estimated**: 0.1 hours

- [ ] **TASK-3.15**: Extract tool selection guidelines
  - File: `universal_rules/ai_tool_usage/tool_selection.md`
  - **Estimated**: 0.1 hours

- [ ] **TASK-3.16**: Create universal rules index
  - File: `universal_rules/index.json`
  - Include: All categories, subcategories, file paths
  - **Estimated**: 0.1 hours

**Phase 3 Tasks**: 16/16
**Phase 3 Progress**: 0%
**Phase 3 Estimated**: 1 hour

---

## 🛠️ Phase 4: Tool-Specific Cleanup

**Status**: ⏳ PENDING
**Estimated**: 0.5 hours
**Priority**: MEDIUM
**Depends On**: Phase 3 complete

### Cursor Folder Cleanup

- [ ] **TASK-4.1**: Review and clean cursor/ folder
  - Read: All files in `cursor/`
  - Identify: Generic content that should be moved
  - Action: Move generic content to appropriate new locations
  - Keep only: VS Code integration, cursor-specific commands
  - **Estimated**: 0.1 hours

- [ ] **TASK-4.2**: Update cursor/ file references
  - Add: Links to new locations for generic rules
  - Add: Deprecation notices where appropriate
  - **Estimated**: 0.1 hours

### Kimi Folder Cleanup

- [ ] **TASK-4.3**: Review and clean kimi/ folder
  - Keep only: SetTodoList integration, Kimi CLI features
  - Move: All generic content to new locations
  - **Estimated**: 0.1 hours

- [ ] **TASK-4.4**: Update kimi/ file references
  - Add: Links to new locations
  - Add: Deprecation notices
  - **Estimated**: 0.1 hours

### Claude Folder Cleanup

- [ ] **TASK-4.5**: Review and clean claude/ folder
  - Keep only: Claude-specific features
  - Move: Generic content
  - **Estimated**: 0.1 hours

- [ ] **TASK-4.6**: Update claude/ file references
  - Add: Links and deprecation notices
  - **Estimated**: 0.1 hours

### Gemini Folder Cleanup

- [ ] **TASK-4.7**: Review and clean gemini/ folder
  - Keep only: Gemini-specific features
  - Move: Generic content
  - **Estimated**: 0.1 hours

- [ ] **TASK-4.8**: Update gemini/ file references
  - Add: Links and deprecation notices
  - **Estimated**: 0.1 hours

**Phase 4 Tasks**: 8/8
**Phase 4 Progress**: 0%
**Phase 4 Estimated**: 0.5 hours

---

## 🔌 Phase 5: MCP Server Updates

**Status**: ⏳ PENDING
**Estimated**: 1 hour
**Priority**: HIGH
**Depends On**: Phase 4 complete

### Core Server Updates

- [ ] **TASK-5.1**: Update file resolution logic in MCP server
  - File: `mcp_server/src/utils/fileResolver.ts`
  - Implement: `resolveCodeGuideline()` function
  - Implement: `resolveUniversalRule()` function
  - Implement: `resolveToolSpecific()` function
  - **Estimated**: 0.2 hours

- [ ] **TASK-5.2**: Add `load_code_guidelines` tool
  - File: `mcp_server/src/tools/loadCodeGuidelines.ts`
  - Input: `{ language: string, section?: string }`
  - Output: Formatted markdown content
  - **Estimated**: 0.2 hours

- [ ] **TASK-5.3**: Add `load_universal_rules` tool
  - File: `mcp_server/src/tools/loadUniversalRules.ts`
  - Input: `{ category: string, subcategory?: string }`
  - Output: Formatted rule content
  - **Estimated**: 0.2 hours

### Backward Compatibility

- [ ] **TASK-5.4**: Update `load_style_guide` tool (legacy)
  - File: `mcp_server/src/tools/loadStyleGuide.ts`
  - Add: Compatibility layer for old structure
  - Log: Deprecation warning when used
  - **Estimated**: 0.1 hours

### Performance & Quality

- [ ] **TASK-5.5**: Add caching layer for file reads
  - File: `mcp_server/src/utils/cache.ts`
  - Cache: File contents for 5 minutes
  - Invalidate: On file system changes
  - **Estimated**: 0.1 hours

- [ ] **TASK-5.6**: Update error messages
  - File: All MCP server tools
  - Add: Clear error messages for missing files
  - Add: Suggestions for correct paths
  - **Estimated**: 0.1 hours

- [ ] **TASK-5.7**: Test all tools with new structure
  - Test: `load_code_guidelines` for all 6 languages
  - Test: `load_universal_rules` for all 5 categories
  - Test: `load_tool_specific` for all 4 tools
  - **Estimated**: 0.2 hours

**Phase 5 Tasks**: 7/7
**Phase 5 Progress**: 0%
**Phase 5 Estimated**: 1 hour

---

## 🧪 Phase 6: Testing & Validation

**Status**: ⏳ PENDING
**Estimated**: 0.5 hours
**Priority**: HIGH
**Depends On**: Phase 5 complete

### Functional Testing

- [ ] **TASK-6.1**: Test loading each language guideline
  - Test: Python, TypeScript, Rust, Kotlin, Swift, Haskell
  - Verify: Content loads correctly
  - Verify: Metadata parsed correctly
  - **Estimated**: 0.1 hours

- [ ] **TASK-6.2**: Test loading each universal rule category
  - Test: git, testing, documentation, project_structure, ai_tool_usage
  - Verify: All subcategories accessible
  - **Estimated**: 0.1 hours

- [ ] **TASK-6.3**: Test tool-specific customizations
  - Test: cursor/, kimi/, claude/, gemini/ specific files
  - Verify: Unique content preserved
  - **Estimated**: 0.1 hours

### Integration Testing

- [ ] **TASK-6.4**: Verify no broken links
  - Tool: `find . -name "*.md" -exec grep -l "TODO\|FIXME\|broken link" {} \;`
  - Fix: Any broken internal references
  - **Estimated**: 0.1 hours

- [ ] **TASK-6.5**: Check MCP server integration
  - Test: Full MCP workflow with Kimi
  - Test: Full MCP workflow with Cursor
  - Verify: All tools return correct content
  - **Estimated**: 0.1 hours

### Quality Assurance

- [ ] **TASK-6.6**: Validate metadata completeness
  - Check: All files have required frontmatter
  - Verify: Categories and types are correct
  - **Estimated**: 0.1 hours

- [ ] **TASK-6.7**: Test with sample AI assistant requests
  - Simulate: AI requesting Python guidelines
  - Simulate: AI requesting git rules
  - Verify: Appropriate content returned
  - **Estimated**: 0.1 hours

**Phase 6 Tasks**: 7/7
**Phase 6 Progress**: 0%
**Phase 6 Estimated**: 0.5 hours

---

## 📖 Phase 7: Documentation & Rollout

**Status**: ⏳ PENDING
**Estimated**: 0.5 hours
**Priority**: MEDIUM
**Depends On**: Phase 6 complete

### Migration Guide

- [ ] **TASK-7.1**: Create migration guide for users
  - File: `docs/migration_guide.md`
  - Include: What changed, why, how to adapt
  - Include: Before/after structure comparison
  - **Estimated**: 0.1 hours

### Main Documentation

- [ ] **TASK-7.2**: Update main README.md
  - Section: New directory structure
  - Section: How to find rules
  - Section: Quick start guide
  - **Estimated**: 0.1 hours

- [ ] **TASK-7.3**: Create structure diagram
  - File: `docs/assets/structure_diagram.png`
  - Tool: Mermaid or similar
  - Show: New organization visually
  - **Estimated**: 0.1 hours

### Contribution Guidelines

- [ ] **TASK-7.4**: Document how to add new languages
  - File: `docs/contributing/adding_languages.md`
  - Include: File structure, metadata, examples
  - **Estimated**: 0.1 hours

- [ ] **TASK-7.5**: Document how to add new rule categories
  - File: `docs/contributing/adding_rule_categories.md`
  - Include: Where to place, how to organize
  - **Estimated**: 0.1 hours

- [ ] **TASK-7.6**: Update contribution guidelines
  - File: `CONTRIBUTING.md`
  - Update: Process for contributing to new structure
  - **Estimated**: 0.1 hours

### Team Communication

- [ ] **TASK-7.7**: Announce changes to team
  - Create: Announcement message
  - Channels: Slack, email, team meeting
  - Include: Benefits, how to use new structure
  - **Estimated**: 0.1 hours

**Phase 7 Tasks**: 7/7
**Phase 7 Progress**: 0%
**Phase 7 Estimated**: 0.5 hours

---

## 📈 Overall Project Status

### Total Progress by Phase

| Phase | Tasks | Progress | Estimated | Actual |
|-------|-------|----------|-----------|--------|
+| Phase 1: Foundation Setup | 7/7 | 100% | 1h | 1h |
| Phase 2: Code Guidelines Migration | 16/16 | 0% | 1.5h | 0h |
| Phase 3: Universal Rules Extraction | 16/16 | 0% | 1h | 0h |
| Phase 4: Tool-Specific Cleanup | 8/8 | 0% | 0.5h | 0h |
| Phase 5: MCP Server Updates | 7/7 | 0% | 1h | 0h |
| Phase 6: Testing & Validation | 7/7 | 0% | 0.5h | 0h |
| Phase 7: Documentation & Rollout | 7/7 | 0% | 0.5h | 0h |
| **TOTAL** | **68/68** | **0%** | **6h** | **0h** |

### Current Status Summary

+**Status**: ✅ PHASE 1 COMPLETE
+**Next Task**: TASK-2.1 (Analyze existing Python guidelines across tools)
+**Priority**: HIGH
+**Estimated Start**: Immediate
+**Blockers**: None

---

## 🎯 Success Criteria Checklist

### Overall Project Completion

**Structure Requirements:**
- [ ] `code_guidelines/` directory created with all languages
- [ ] `universal_rules/` directory created with all categories
- [ ] `ai_tool_specific/` directory created with all tools
- [ ] All old content migrated to new structure
- [ ] Zero duplication in core content

**Content Requirements:**
- [ ] All 6 languages have unified guidelines
- [ ] All 5 universal rule categories populated
- [ ] Tool-specific folders contain only unique content
- [ ] All files have proper frontmatter metadata
- [ ] Index files created for MCP server

**MCP Server Requirements:**
- [ ] File resolution logic updated
- [ ] New tools implemented (`load_code_guidelines`, `load_universal_rules`)
- [ ] Backward compatibility maintained
- [ ] All tools tested and working

**Quality Requirements:**
- [ ] No broken internal links
- [ ] All tests passing
- [ ] Metadata complete and accurate
- [ ] Documentation updated

**Documentation Requirements:**
- [ ] README.md updated with new structure
- [ ] Migration guide created
- [ ] Contributing guidelines updated
- [ ] Team announcement prepared

**Final Verification:**
- [ ] All 68 tasks complete
- [ ] Total time within estimate (6h)
- [ ] Zero duplication achieved
- [ ] MCP server fully functional
- [ ] Team ready to use new structure
- [ ] Rollback plan tested (if needed)

---

## 🚨 Common Issues & Solutions

**Issue**: Difficulty identifying unique vs generic content
**Solution**: Use diff tool to compare files across tool folders

**Issue**: Broken references after moving files
**Solution**: Use grep to find all references before moving, update them all

**Issue**: MCP server can't find files in new structure
**Solution**: Verify file resolution logic matches actual directory structure

**Issue**: Team confusion about new structure
**Solution**: Provide clear documentation and examples, offer Q&A session

**Issue**: Missing metadata in migrated files
**Solution**: Create validation script to check all files have required frontmatter

---

## 📞 Getting Help

**Questions about task?** Check the paired RULES_REFACTOR_PLAN.md
**Need clarification?** Review architecture decisions in plan
**Found an issue?** Create issue with TODO number
**Blocker?** Mark task blocked, note reason, escalate if needed

---

**Document Metadata:**
- **Status**: ✅ COMPLETE & READY FOR USE
- **Created**: 2025-11-19
- **Last Updated**: 2025-11-19
- **Version**: 1.0.0
- **Total Tasks**: 68
- **Maintained By**: Rules Refactoring Team
- **Implementation Ready**: YES

🤖 Generated with [Cursor](https://cursor.sh)

**Note**: This TODO list provides detailed task breakdown for the rules repository refactoring. Each task is actionable with clear completion criteria. The refactoring enables the MCP server to serve both code guidelines and universal rules from a unified, tool-agnostic structure.