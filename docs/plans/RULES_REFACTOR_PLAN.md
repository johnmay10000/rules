# Rules Repository Refactoring Plan

**Project**: Unified Rules Structure Refactoring
**Type**: Architecture & Migration Planning
**Version**: 1.0.0
**Status**: Planning Phase
**Date**: 2025-11-19

---

## 📋 Executive Summary

This plan outlines the refactoring of the global AI rules repository from a tool-specific structure (separate `cursor/`, `kimi/`, `gemini/`, `claude/` folders) to a unified, tool-agnostic structure. The new architecture separates concerns into two main categories:

1. **Code Guidelines**: Language-specific functional programming patterns and best practices
2. **Universal Rules**: Mandatory, tool-agnostic rules for git, testing, documentation, and project structure

This refactoring enables the MCP server to serve both code and non-code rules, eliminates duplication, and provides a single source of truth for all AI assistants.

---

## 🎯 Goals & Objectives

### Primary Goals
- ✅ Eliminate duplication across AI tool folders (cursor, kimi, gemini, claude)
- ✅ Create single source of truth for code guidelines per language
- ✅ Establish universal mandatory rules applicable to all AI tools
- ✅ Enable MCP server to serve both code and non-code rules
- ✅ Maintain backward compatibility during migration
- ✅ Support tool-specific customizations where necessary

### Success Metrics
- **Zero Duplication**: No duplicated content across tool folders
- **100% Coverage**: All existing rules migrated to new structure
- **MCP Ready**: New structure fully compatible with MCP server
- **No Breaking Changes**: Existing projects continue to work
- **Migration Time**: < 4 hours total

---

## 📂 Current State Analysis

### Existing Structure
```
rules/
├── cursor/
│   ├── CURSOR.md
│   ├── CLAUDE.md
│   ├── python-fp-style-guide.md
│   ├── typescript-fp-style-guide.md
│   └── ...
├── kimi/
│   ├── KIMI.md
│   ├── CLAUDE.md
│   ├── python-fp-style-guide.md
│   ├── typescript-fp-style-guide.md
│   └── ...
├── gemini/
│   ├── GEMINI.md
│   ├── python-fp-style-guide.md
│   ├── typescript-fp-style-guide.md
│   └── ...
├── claude/
│   ├── CLAUDE.md
│   ├── python-fp-style-guide.md
│   └── ...
└── templates/
    └── ...
```

### Problems Identified
1. **Massive Duplication**: 80%+ content identical across tool folders
2. **Maintenance Nightmare**: Update required in 4 places
3. **Inconsistency Risk**: Easy for tools to diverge
4. **No Universal Rules**: Git, testing rules mixed with code guidelines
5. **MCP Complexity**: Server must check multiple locations
6. **Scaling Issues**: Adding new tools multiplies duplication

---

## 🏗️ Proposed New Structure

### Unified Architecture
```
rules/
├── code_guidelines/                      # NEW: Tool-agnostic code guidelines
│   ├── languages/
│   │   ├── python/
│   │   │   ├── fp_style_guide.md        # Single source of truth
│   │   │   ├── patterns.md
│   │   │   ├── examples.md
│   │   │   └── libraries.md
│   │   ├── typescript/
│   │   │   ├── fp_style_guide.md
│   │   │   ├── patterns.md
│   │   │   ├── examples.md
│   │   │   └── libraries.md
│   │   ├── rust/
│   │   ├── kotlin/
│   │   ├── swift/
│   │   └── haskell/
│   └── principles/
│       ├── functional_programming.md
│       ├── immutability.md
│       └── composition.md
│
├── universal_rules/                      # NEW: Mandatory universal rules
│   ├── git/
│   │   ├── commit_conventions.md
│   │   ├── branching_strategy.md
│   │   └── git_checkpoint_rules.md
│   ├── testing/
│   │   ├── testing_philosophy.md
│   │   ├── unit_testing.md
│   │   ├── integration_testing.md
│   │   └── test_coverage_requirements.md
│   ├── documentation/
│   │   ├── file_organization.md
│   │   ├── naming_conventions.md
│   │   └── daily_workflow.md
│   ├── project_structure/
│   │   ├── directory_layout.md
│   │   ├── file_size_limits.md
│   │   └── template_standards.md
│   └── ai_tool_usage/
│       ├── mcp_protocol.md
│       ├── tool_selection.md
│       └── context_management.md
│
├── ai_tool_specific/                     # NEW: Tool-specific customizations
│   ├── cursor/
│   │   ├── cursor_specific_features.md
│   │   └── integrations.md
│   ├── kimi/
│   │   ├── kimi_specific_features.md
│   │   └── settodo_integration.md
│   ├── claude/
│   │   └── claude_specific_features.md
│   └── gemini/
│       └── gemini_specific_features.md
│
├── templates/                            # Existing: Document templates
│   ├── daily_work/
│   ├── plans/
│   └── project_setup/
│
├── examples/                             # Existing: Usage examples
│   ├── python/
│   ├── typescript/
│   └── rust/
│
├── docs/                                 # Existing: Documentation
│   └── plans/
│
└── mcp_server/                           # NEW: MCP server implementation
    ├── src/
    │   ├── index.ts
    │   ├── tools/
    │   └── utils/
    ├── package.json
    └── .do/
        └── app.yaml
```

### Key Design Principles

1. **Separation of Concerns**: Code guidelines vs universal rules
2. **Tool Agnostic**: Core content works across all AI assistants
3. **Single Source of Truth**: One file per language/concern
4. **Extensible**: Easy to add new languages or rule categories
5. **MCP Optimized**: Structure designed for efficient server access
6. **Backward Compatible**: Legacy structure supported during transition

---

## 📊 Content Migration Strategy

### Phase 1: Code Guidelines Consolidation

**For each language (Python, TypeScript, Rust, Kotlin, Swift, Haskell):**

1. **Analyze existing files** across all tool folders
2. **Identify unique content** per tool (usually < 5%)
3. **Create unified version** in `code_guidelines/languages/{lang}/`
4. **Extract tool-specific notes** to `ai_tool_specific/{tool}/`
5. **Add frontmatter metadata** for MCP consumption:
   ```markdown
   ---
   title: Python Functional Programming Style Guide
   language: python
   category: code_guidelines
   applies_to: [cursor, kimi, claude, gemini]
   version: 1.0.0
   last_updated: 2025-11-19
   ---
   ```

### Phase 2: Universal Rules Extraction

**Categories to extract:**

1. **Git Rules** (from CURSOR.md, KIMI.md, etc.)
   - Commit conventions
   - Branching strategy
   - Checkpoint requirements

2. **Testing Rules**
   - Testing philosophy
   - Coverage requirements
   - Test file organization

3. **Documentation Rules**
   - File organization
   - Naming conventions
   - Daily workflow structure

4. **Project Structure Rules**
   - Directory layout
   - File size limits
   - Template standards

5. **AI Tool Usage Rules**
   - MCP protocol usage
   - Tool selection guidelines
   - Context management

### Phase 3: Tool-Specific Customizations

**Preserve only truly unique content:**

- **Cursor**: VS Code integration, specific commands
- **Kimi**: SetTodoList integration, CLI features
- **Claude**: Claude-specific features
- **Gemini**: Gemini-specific features

**Structure:**
```markdown
---
title: Kimi-Specific Features
tool: kimi
category: ai_tool_specific
---
```

---

## 🔧 MCP Server Integration

### Updated Server Architecture

```typescript
// MCP Server Tool: load_code_guidelines
interface LoadCodeGuidelinesArgs {
  language: string;  // python, typescript, rust, etc.
  section?: string;  // overview, patterns, examples, libraries
}

// MCP Server Tool: load_universal_rules
interface LoadUniversalRulesArgs {
  category: string;  // git, testing, documentation, project_structure, ai_tool_usage
  subcategory?: string;
}

// MCP Server Tool: load_tool_specific
interface LoadToolSpecificArgs {
  tool: string;  // cursor, kimi, claude, gemini
  feature?: string;
}
```

### File Resolution Logic

```typescript
// Code Guidelines Resolution
function resolveCodeGuideline(language: string, section?: string): string {
  const basePath = `code_guidelines/languages/${language}/fp_style_guide.md`;
  if (section) {
    return `code_guidelines/languages/${language}/${section}.md`;
  }
  return basePath;
}

// Universal Rules Resolution
function resolveUniversalRule(category: string, subcategory?: string): string {
  if (subcategory) {
    return `universal_rules/${category}/${subcategory}.md`;
  }
  return `universal_rules/${category}/${category}_overview.md`;
}

// Tool-Specific Resolution
function resolveToolSpecific(tool: string, feature?: string): string {
  if (feature) {
    return `ai_tool_specific/${tool}/${feature}.md`;
  }
  return `ai_tool_specific/${tool}/${tool}_specific_features.md`;
}
```

---

## 📋 Implementation Phases

### **Phase 1: Foundation Setup** (Estimated: 1 hour)

**Goal**: Create new directory structure and migration tools

**Tasks:**
- [ ] Create `code_guidelines/` directory structure
- [ ] Create `universal_rules/` directory structure
- [ ] Create `ai_tool_specific/` directory structure
- [ ] Create migration script template
- [ ] Update `.cursorrules` with new structure
- [ ] Update repository README

**Deliverables:**
- New directory tree created
- Migration tooling ready
- Documentation updated

---

### **Phase 2: Code Guidelines Migration** (Estimated: 1.5 hours)

**Goal**: Migrate all language-specific code guidelines

**Tasks:**
- [ ] Migrate Python guidelines (analyze 4 tool folders, create unified)
- [ ] Migrate TypeScript guidelines
- [ ] Migrate Rust guidelines
- [ ] Migrate Kotlin guidelines
- [ ] Migrate Swift guidelines
- [ ] Migrate Haskell guidelines
- [ ] Add frontmatter metadata to all files
- [ ] Create index.json for MCP server

**Per Language Checklist:**
- [ ] Compare all 4 tool versions
- [ ] Identify unique content per tool
- [ ] Create unified version
- [ ] Extract tool-specific notes
- [ ] Add metadata frontmatter
- [ ] Verify MCP server can load it

**Deliverables:**
- All languages in `code_guidelines/languages/`
- Tool-specific notes in `ai_tool_specific/`
- Index file for fast lookup

---

### **Phase 3: Universal Rules Extraction** (Estimated: 1 hour)

**Goal**: Extract and organize universal mandatory rules

**Tasks:**
- [ ] Extract git rules from existing tool files
- [ ] Extract testing rules
- [ ] Extract documentation rules
- [ ] Extract project structure rules
- [ ] Extract AI tool usage rules
- [ ] Organize into `universal_rules/` categories
- [ ] Add cross-references between related rules
- [ ] Create universal_rules_index.json

**Deliverables:**
- 5 categories of universal rules
- Cross-referenced documentation
- Index file for MCP server

---

### **Phase 4: Tool-Specific Cleanup** (Estimated: 0.5 hours)

**Goal**: Preserve only truly unique tool-specific content

**Tasks:**
- [ ] Review cursor/ folder, move generic content
- [ ] Review kimi/ folder, move generic content
- [ ] Review claude/ folder, move generic content
- [ ] Review gemini/ folder, move generic content
- [ ] Leave only truly unique features
- [ ] Update references to new locations
- [ ] Add deprecation notices to old files

**Deliverables:**
- Minimal tool-specific folders
- All generic content moved to appropriate locations
- Backward compatibility maintained

---

### **Phase 5: MCP Server Updates** (Estimated: 1 hour)

**Goal**: Update MCP server to work with new structure

**Tasks:**
- [ ] Update file resolution logic
- [ ] Add `load_code_guidelines` tool
- [ ] Add `load_universal_rules` tool
- [ ] Update `load_style_guide` tool (deprecated, but functional)
- [ ] Add caching layer for file reads
- [ ] Update error messages
- [ ] Test all tools with new structure

**Deliverables:**
- MCP server compatible with new structure
- New tools for accessing universal rules
- Backward compatibility maintained

---

### **Phase 6: Testing & Validation** (Estimated: 0.5 hours)

**Goal**: Ensure everything works correctly

**Tasks:**
- [ ] Test loading each language guideline
- [ ] Test loading each universal rule category
- [ ] Test tool-specific customizations
- [ ] Verify no broken links
- [ ] Check MCP server integration
- [ ] Validate metadata completeness
- [ ] Test with sample AI assistant requests

**Deliverables:**
- All tests passing
- No broken references
- MCP server fully functional

---

### **Phase 7: Documentation & Rollout** (Estimated: 0.5 hours)

**Goal**: Document the new structure and guide users

**Tasks:**
- [ ] Create migration guide for users
- [ ] Update main README with new structure
- [ ] Create diagram of new organization
- [ ] Document how to add new languages
- [ ] Document how to add new rule categories
- [ ] Update contribution guidelines
- [ ] Announce changes to team

**Deliverables:**
- Updated documentation
- Migration guide
- Contribution guidelines
- Team announcement

---

## 📊 Timeline & Estimates

| Phase | Description | Estimated | Priority |
|-------|-------------|-----------|----------|
| 1 | Foundation Setup | 1 hour | High |
| 2 | Code Guidelines Migration | 1.5 hours | High |
| 3 | Universal Rules Extraction | 1 hour | High |
| 4 | Tool-Specific Cleanup | 0.5 hours | Medium |
| 5 | MCP Server Updates | 1 hour | High |
| 6 | Testing & Validation | 0.5 hours | High |
| 7 | Documentation & Rollout | 0.5 hours | Medium |
| **Total** | **All Phases** | **6 hours** | **1 day** |

**Recommended Schedule:**
- **Morning**: Phases 1-3 (Structure & Migration)
- **Afternoon**: Phases 4-6 (Cleanup & Testing)
- **Next Day**: Phase 7 (Documentation)

---

## 🎯 Success Criteria

### Phase Completion Criteria

**Phase 1: Foundation Setup**
- ✅ New directory structure created
- ✅ Migration tools ready
- ✅ Documentation templates updated

**Phase 2: Code Guidelines Migration**
- ✅ All 6 languages migrated
- ✅ Zero duplication in code guidelines
- ✅ Tool-specific notes preserved
- ✅ MCP index files created

**Phase 3: Universal Rules Extraction**
- ✅ 5 universal rule categories created
- ✅ All git rules extracted and organized
- ✅ All testing rules extracted
- ✅ Cross-references added

**Phase 4: Tool-Specific Cleanup**
- ✅ Only unique content remains in tool folders
- ✅ All generic content moved
- ✅ Deprecation notices added

**Phase 5: MCP Server Updates**
- ✅ New tools implemented and working
- ✅ File resolution logic updated
- ✅ Backward compatibility maintained

**Phase 6: Testing & Validation**
- ✅ All languages load correctly
- ✅ All universal rules accessible
- ✅ No broken links or references
- ✅ MCP integration verified

**Phase 7: Documentation & Rollout**
- ✅ README updated
- ✅ Migration guide created
- ✅ Team notified of changes

### Overall Project Success

**Migration Complete When:**
- ✅ Zero duplicated content across tool folders
- ✅ All code guidelines in unified location
- ✅ Universal rules clearly separated
- ✅ MCP server serves both rule types
- ✅ All existing functionality preserved
- ✅ Documentation complete and accurate
- ✅ Team can successfully use new structure

**Production Ready When:**
- ✅ All phases complete
- ✅ Tests passing
- ✅ No critical issues
- ✅ Team feedback positive
- ✅ Rollout plan executed

---

## 🔄 Rollback Plan

### Emergency Rollback Procedure

If critical issues arise:

1. **Immediate**: Restore from git backup
   ```bash
   git revert <migration-commit>
   ```

2. **Short-term**: Maintain old structure in parallel
   - Keep `cursor/`, `kimi/`, etc. folders
   - New structure in `code_guidelines/`, `universal_rules/`
   - MCP server checks both locations

3. **Long-term**: Gradual migration
   - Support both structures for 2 weeks
   - Deprecation warnings in old structure
   - Full cutover after validation period

### Compatibility Layer

```typescript
// MCP server backward compatibility
function loadStyleGuideLegacy(language: string, tool: string): string {
  // Try new structure first
  const newPath = `code_guidelines/languages/${language}/fp_style_guide.md`;
  if (fs.existsSync(newPath)) {
    return newPath;
  }
  
  // Fall back to old structure
  return `${tool}/${language}-fp-style-guide.md`;
}
```

---

## 📚 Reference Documents

**Related Plans:**
- [MCP_IMPLEMENTATION_PLAN.md](./MCP_IMPLEMENTATION_PLAN.md) - MCP server implementation
- [MCP_IMPLEMENTATION_TODO.md](./MCP_IMPLEMENTATION_TODO.md) - Task tracking

**Architecture:**
- [ARCHITECTURE_PLAN_MCP.md](./ARCHITECTURE_PLAN_MCP.md) - System architecture

**Examples:**
- [Existing structure](../../cursor/CURSOR.md) - Current Kimi rules
- [Existing structure](../../kimi/KIMI.md) - Current Cursor rules

---

## 📝 Notes & Guidelines

### Content Organization Principles

**For Code Guidelines:**
- Focus on language-specific functional programming patterns
- Include: syntax, patterns, examples, libraries
- Exclude: Tool-specific instructions, generic rules

**For Universal Rules:**
- Apply to all projects regardless of language
- Include: Process, workflow, git, testing, documentation
- Exclude: Language-specific syntax or patterns

**For Tool-Specific:**
- Only truly unique features per tool
- Reference generic rules, don't duplicate
- Include: Integration instructions, tool-specific commands

### Metadata Standards

```markdown
---
title: [Descriptive Title]
category: [code_guidelines | universal_rules | ai_tool_specific]
type: [language | git | testing | documentation | tool]
applies_to: [list of tools or "all"]
language: [for code guidelines]
version: 1.0.0
last_updated: YYYY-MM-DD
---
```

---

**Document Metadata:**
- **Status**: Planning Complete
- **Created**: 2025-11-19
- **Version**: 1.0.0
- **Estimated Time**: 6 hours
- **Priority**: High (blocks MCP server development)
- **Next Step**: Create detailed TODO list

🤖 Generated with [Cursor](https://cursor.sh)

**Note**: This refactoring is a prerequisite for the MCP server to effectively serve both code guidelines and universal rules. The unified structure eliminates duplication and provides a clean API for the MCP server to access all rule types.