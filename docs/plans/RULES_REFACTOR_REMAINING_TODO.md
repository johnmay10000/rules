# Rules Refactor: Remaining Tasks

**Project**: Unified Rules Structure Refactoring
**Status**: In Progress
**Date**: 2025-11-19
**Previous Plan**: [RULES_REFACTOR_PLAN.md](./RULES_REFACTOR_PLAN.md)
**Previous TODO**: [RULES_REFACTOR_TODO.md](./RULES_REFACTOR_TODO.md)

---

## 📊 Status Summary

- **Phase 1 (Foundation)**: ✅ Complete
- **Phase 2 (Code Guidelines)**: ✅ Complete
- **Phase 3 (Universal Rules)**: 🔄 Mostly Complete (Missing index)
- **Phase 4 (Cleanup)**: ⏳ Pending
- **Phase 5 (MCP Server)**: ⏳ Pending
- **Phase 6 (Testing)**: ⏳ Pending
- **Phase 7 (Docs)**: ⏳ Pending

---

## 📝 Remaining Tasks

### Phase 3: Universal Rules (Completion)

- [ ] **TASK-3.16**: Create universal rules index
  - File: `universal_rules/index.json`
  - Include: All categories, subcategories, file paths

### Phase 4: Tool-Specific Cleanup

**Goal**: Remove generic content that has been migrated to `code_guidelines/` and `universal_rules/`.

- [ ] **TASK-4.1**: Clean `cursor/` folder
  - Remove: `*-fp-style-guide.md` (migrated to `code_guidelines/`)
  - Remove: `CURSOR.md` (rules migrated to `universal_rules/`)
  - Remove: `CURSOR_FP_PRINCIPLES.md` (migrated)
  - Remove: `CURSOR_WORKFLOW_GUIDE.md` (migrated)
  - Keep: `cursor_specific_features.md` (if exists) or create it
  - Update: References to point to new locations

- [ ] **TASK-4.3**: Clean `kimi/` folder
  - Remove: `*-fp-style-guide.md`
  - Remove: `KIMI.md`
  - Remove: `KIMI_FP_PRINCIPLES.md`
  - Remove: `KIMI_WORKFLOW_GUIDE.md`
  - Keep: `kimi_specific_features.md` (if exists) or create it
  - Update: References

- [ ] **TASK-4.7**: Clean `gemini/` folder
  - Remove: `*-fp-style-guide.md`
  - Remove: `GEMINI.md`
  - Remove: `GEMINI_FP_PRINCIPLES.md`
  - Remove: `GEMINI_WORKFLOW_GUIDE.md`
  - Keep: `gemini_specific_features.md` (if exists) or create it
  - Update: References

### Phase 5: MCP Server Updates

- [ ] **TASK-5.1**: Update file resolution logic in MCP server
  - File: `mcp_server/src/utils/fileResolver.ts`
- [ ] **TASK-5.2**: Add `load_code_guidelines` tool
- [ ] **TASK-5.3**: Add `load_universal_rules` tool
- [ ] **TASK-5.4**: Update `load_style_guide` tool (legacy compatibility)
- [ ] **TASK-5.5**: Add caching layer
- [ ] **TASK-5.6**: Update error messages
- [ ] **TASK-5.7**: Test all tools with new structure

### Phase 6: Testing & Validation

- [ ] **TASK-6.1**: Test loading each language guideline
- [ ] **TASK-6.2**: Test loading each universal rule category
- [ ] **TASK-6.3**: Test tool-specific customizations
- [ ] **TASK-6.4**: Verify no broken links
- [ ] **TASK-6.5**: Check MCP server integration
- [ ] **TASK-6.6**: Validate metadata completeness
- [ ] **TASK-6.7**: Test with sample AI assistant requests

### Phase 7: Documentation & Rollout

- [ ] **TASK-7.1**: Create migration guide (`docs/migration_guide.md`)
- [ ] **TASK-7.2**: Update main `README.md`
- [ ] **TASK-7.3**: Create structure diagram
- [ ] **TASK-7.4**: Document how to add new languages
- [ ] **TASK-7.5**: Document how to add new rule categories
- [ ] **TASK-7.6**: Update `CONTRIBUTING.md`
- [ ] **TASK-7.7**: Announce changes
