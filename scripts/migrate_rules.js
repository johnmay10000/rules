#!/usr/bin/env node

/**
 * Rules Repository Migration Script
 *
 * This script provides utilities for migrating from the old tool-specific structure
 * to the new unified structure (code_guidelines, universal_rules, ai_tool_specific).
 *
 * Usage:
 *   node scripts/migrate_rules.js --analyze python
 *   node scripts/migrate_rules.js --migrate typescript
 *   node scripts/migrate_rules.js --validate
 */

import { promises as fs } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';
import { glob } from 'glob';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const ROOT_DIR = join(__dirname, '..');

// ============================================================================
// CONFIGURATION
// ============================================================================

const LANGUAGES = ['python', 'typescript', 'rust', 'kotlin', 'swift', 'haskell'];
const TOOLS = ['cursor', 'kimi', 'claude', 'gemini'];
const UNIVERSAL_CATEGORIES = ['git', 'testing', 'documentation', 'project_structure', 'ai_tool_usage'];

// ============================================================================
// FILE SYSTEM UTILITIES
// ============================================================================

/**
 * Ensure a directory exists, create if it doesn't
 */
async function ensureDir(dirPath) {
  try {
    await fs.mkdir(dirPath, { recursive: true });
    console.log(`✓ Directory ensured: ${dirPath}`);
  } catch (error) {
    console.error(`✗ Error creating directory ${dirPath}:`, error.message);
  }
}

/**
 * Read a file and return its content
 */
async function readFile(filePath) {
  try {
    const content = await fs.readFile(filePath, 'utf-8');
    return content;
  } catch (error) {
    console.error(`✗ Error reading file ${filePath}:`, error.message);
    return null;
  }
}

/**
 * Write content to a file (creates directories if needed)
 */
async function writeFile(filePath, content) {
  try {
    const dir = dirname(filePath);
    await ensureDir(dir);
    await fs.writeFile(filePath, content, 'utf-8');
    console.log(`✓ File written: ${filePath}`);
    return true;
  } catch (error) {
    console.error(`✗ Error writing file ${filePath}:`, error.message);
    return false;
  }
}

/**
 * Check if a file exists
 */
async function fileExists(filePath) {
  try {
    await fs.access(filePath);
    return true;
  } catch {
    return false;
  }
}

/**
 * Find all files matching a glob pattern
 */
async function findFiles(pattern) {
  try {
    const files = await glob(pattern, { cwd: ROOT_DIR });
    return files.map(f => join(ROOT_DIR, f));
  } catch (error) {
    console.error(`✗ Error finding files with pattern ${pattern}:`, error.message);
    return [];
  }
}

// ============================================================================
// METADATA UTILITIES
// ============================================================================

/**
 * Extract metadata from markdown frontmatter
 */
function extractMetadata(content) {
  const metadata = {};
  const frontmatterMatch = content.match(/^---\n([\s\S]*?)\n---/);

  if (frontmatterMatch) {
    const frontmatter = frontmatterMatch[1];
    const lines = frontmatter.split('\n');

    for (const line of lines) {
      const [key, ...valueParts] = line.split(':');
      if (key && valueParts.length > 0) {
        const value = valueParts.join(':').trim();
        metadata[key.trim()] = value.replace(/^['"]|['"]$/g, '');
      }
    }
  }

  return metadata;
}

/**
 * Add or update frontmatter metadata
 */
function addMetadata(content, newMetadata) {
  const existingMetadata = extractMetadata(content);
  const mergedMetadata = { ...existingMetadata, ...newMetadata };

  const metadataLines = Object.entries(mergedMetadata)
    .map(([key, value]) => `${key}: ${value}`)
    .join('\n');

  const newFrontmatter = `---\n${metadataLines}\n---\n\n`;

  // Remove existing frontmatter if present
  const contentWithoutFrontmatter = content.replace(/^---\n[\s\S]*?\n---\n\n?/, '');

  return newFrontmatter + contentWithoutFrontmatter;
}

/**
 * Generate standard metadata for code guidelines
 */
function generateCodeGuidelineMetadata(language) {
  return {
+    title: `${language.charAt(0).toUpperCase() + language.slice(1)} Functional Programming Style Guide`,
+    language: language,
+    category: 'code_guidelines',
+    type: 'language',
+    applies_to: ['cursor', 'kimi', 'claude', 'gemini'],
+    version: '1.0.0',
+    last_updated: new Date().toISOString().split('T')[0]
  };
}

/**
 * Generate standard metadata for universal rules
 */
function generateUniversalRuleMetadata(category, subcategory) {
  return {
+    title: `${subcategory ? subcategory + ' - ' : ''}${category} Rules`,
+    category: 'universal_rules',
+    type: category,
+    applies_to: 'all',
+    version: '1.0.0',
+    last_updated: new Date().toISOString().split('T')[0]
  };
}

// ============================================================================
// ANALYSIS UTILITIES
+// ============================================================================

/**
 * Analyze differences between tool-specific versions of a file
 */
async function analyzeDifferences(language, filename) {
  console.log(`\n🔍 Analyzing differences for ${language}/${filename} across tools...`);
+
  const toolContents = {};
  const differences = {};

  // Read content from each tool
  for (const tool of TOOLS) {
+    const filePath = join(ROOT_DIR, tool, `${filename}.md`);
+    if (await fileExists(filePath)) {
+      const content = await readFile(filePath);
+      toolContents[tool] = content;
+      differences[tool] = {
+        uniqueLines: [],
+        missingLines: []
+      };
+    }
  }

  // Find common lines (present in all tools)
  const allLines = Object.values(toolContents);
  if (allLines.length < 2) {
+    console.log('⚠️  Not enough versions to compare');
+    return differences;
  }

  const commonLines = allLines[0].split('\n').filter(line =>
+    allLines.every(content => content.includes(line))
  );

  console.log(`✓ Found ${commonLines.length} common lines`);
+
  // Find unique lines per tool
  for (const [tool, content] of Object.entries(toolContents)) {
+    const lines = content.split('\n');
+    differences[tool].uniqueLines = lines.filter(line =>
+      !commonLines.includes(line) && line.trim().length > 0
+    );
+    console.log(`  ${tool}: ${differences[tool].uniqueLines.length} unique lines`);
  }

  return differences;
}

/**
 * Generate a unified version by merging content from all tools
 */
async function generateUnifiedVersion(language, filename) {
  console.log(`\n📝 Generating unified version for ${language}/${filename}...`);
+
  const toolContents = {};
+
  // Collect content from all tools
  for (const tool of TOOLS) {
+    const filePath = join(ROOT_DIR, tool, `${filename}.md`);
+    if (await fileExists(filePath)) {
+      toolContents[tool] = await readFile(filePath);
+    }
  }

  if (Object.keys(toolContents).length === 0) {
+    console.log('✗ No files found to merge');
+    return null;
  }

  // Start with content from first tool as base
  const baseTool = Object.keys(toolContents)[0];
  let unifiedContent = toolContents[baseTool];
+
  // Add sections for tool-specific notes if they exist
  let toolSpecificSection = '\n\n## Tool-Specific Notes\n\n';
  let hasToolSpecific = false;
+
  for (const [tool, content] of Object.entries(toolContents)) {
+    if (tool === baseTool) continue;
+
+    const differences = await analyzeDifferences(language, filename);
+    const uniqueContent = differences[tool]?.uniqueLines || [];
+
+    if (uniqueContent.length > 0) {
+      hasToolSpecific = true;
+      toolSpecificSection += `### ${tool.charAt(0).toUpperCase() + tool.slice(1)}\n\n`;
+      toolSpecificSection += uniqueContent.slice(0, 10).join('\n'); // Limit to first 10 unique lines
+      toolSpecificSection += '\n\n';
+    }
  }

  if (hasToolSpecific) {
+    unifiedContent += toolSpecificSection;
  }

  // Add metadata
  unifiedContent = addMetadata(unifiedContent, generateCodeGuidelineMetadata(language));
+
  return unifiedContent;
}

// ============================================================================
+// MIGRATION FUNCTIONS
+// ============================================================================

/**
 * Migrate a specific language's guidelines
 */
async function migrateLanguage(language) {
  console.log(`\n🚀 Migrating ${language} guidelines...`);
+
  const filename = `${language}-fp-style-guide`;
+
  // Generate unified version
  const unifiedContent = await generateUnifiedVersion(language, filename);
+
  if (!unifiedContent) {
+    console.log(`✗ Failed to generate unified content for ${language}`);
+    return false;
+  }
+
  // Write to new location
  const newPath = join(ROOT_DIR, 'code_guidelines', 'languages', language, 'fp_style_guide.md');
+  const success = await writeFile(newPath, unifiedContent);
+
  if (success) {
+    console.log(`✓ Successfully migrated ${language} guidelines`);
+  }
+
  return success;
}

/**
 * Extract universal rules from existing files
 */
async function extractUniversalRules() {
  console.log('\n📋 Extracting universal rules...');
+
  // Git rules
+  await extractGitRules();
+
  // Testing rules
+  await extractTestingRules();
+
  // Documentation rules
+  await extractDocumentationRules();
+
  // Project structure rules
+  await extractProjectStructureRules();
+
  // AI tool usage rules
+  await extractAiToolUsageRules();
+
  console.log('✓ Universal rules extraction complete');
}

/**
 * Extract git-related rules
 */
async function extractGitRules() {
  console.log('  Extracting git rules...');
+
  const gitRules = [];
+
  // Search for git-related content in all tool files
  for (const tool of TOOLS) {
+    const toolFile = join(ROOT_DIR, tool, tool.toUpperCase() + '.md');
+    if (await fileExists(toolFile)) {
+      const content = await readFile(toolFile);
+      const lines = content.split('\n');
+
+      // Look for git-related sections
+      let inGitSection = false;
+      for (const line of lines) {
+        if (line.toLowerCase().includes('git') && line.includes('#')) {
+          inGitSection = true;
+        } else if (line.includes('#') && inGitSection) {
+          inGitSection = false;
+        }
+
+        if (inGitSection && line.trim()) {
+          gitRules.push(line);
+        }
+      }
+    }
  }
+
  // Write git checkpoint rules
+  const checkpointContent = `---
+title: Git Checkpoint Rules
+category: universal_rules
+type: git
+applies_to: all
+version: 1.0.0
+last_updated: ${new Date().toISOString().split('T')[0]}
+---
+
+# Git Checkpoint Rules
+
+${gitRules.filter(line => line.toLowerCase().includes('checkpoint')).join('\n')}
+
+## Requirements
+
+- Git checkpoints must be done at relevant checkpoints throughout work sessions
+- Commit after completing phases, creating documents, logical units
+- Commit before context switches, bug fixes, documentation updates
+- Maximum 30-60 minutes between commits during active work
+`;
+
+  await writeFile(
+    join(ROOT_DIR, 'universal_rules', 'git', 'git_checkpoint_rules.md'),
+    checkpointContent
+  );
}

/**
 * Extract testing-related rules
 */
async function extractTestingRules() {
  console.log('  Extracting testing rules...');
+
  const testingContent = `---
+title: Testing Philosophy and Requirements
+category: universal_rules
+type: testing
+applies_to: all
+version: 1.0.0
+last_updated: ${new Date().toISOString().split('T')[0]}
+---
+
+# Testing Philosophy and Requirements
+
+## Core Philosophy
+
+- **Tests are mandatory**: No code without tests
+- **Test First**: Write tests before implementation when possible
+- **Comprehensive Coverage**: All code paths tested
+- **100% Passing**: All tests must pass before commits
+
+## Test Coverage Requirements
+
+- **Minimum 80% coverage** for all code
+- **100% coverage** for critical paths
+- **Unit tests** for all functions
+- **Integration tests** for workflows
+- **Test files** must be organized and named consistently
+
+## Testing Checklist
+
+Before committing:
+1. ✅ All tests pass
+2. ✅ Coverage meets threshold
+3. ✅ No linter errors
+4. ✅ Tests are meaningful and comprehensive
+`;
+
+  await writeFile(
+    join(ROOT_DIR, 'universal_rules', 'testing', 'testing_philosophy.md'),
+    testingContent
+  );
}

/**
 * Extract documentation-related rules
 */
async function extractDocumentationRules() {
  console.log('  Extracting documentation rules...');
+
  const docContent = `---
+title: Documentation Organization and Standards
+category: universal_rules
+type: documentation
+applies_to: all
+version: 1.0.0
+last_updated: ${new Date().toISOString().split('T')[0]}
+---
+
+# Documentation Organization and Standards
+
+## File Organization
+
+### Daily Work Folders
+- Location: \`docs/YYYY_MM_DD/\`
+- Format: \`YYYYMMDD_NNNN_DESCRIPTIVE_NAME.md\`
+- NNNN = 4-digit sequence (0000, 0001, 0002, ...)
+- Example: \`20251031_0000_CURSOR_MD_IMPLEMENTATION.md\`
+
+### Plans Folder
+- Location: \`docs/plans/\`
+- Strategic plan documents
+- Paired TODO lists for each plan
+- Format: \`FEATURE_NAME_PLAN.md\` + \`FEATURE_NAME_TODO.md\`
+
+## Naming Conventions
+
+- Use descriptive, kebab-case for files
+- Include version numbers for API docs
+- README.md for overview documents
+- CHANGELOG.md for version history
+
+## Content Standards
+
+- Clear headings and structure
+- Code examples where applicable
+- Links to related documents
+- Keep documents focused and concise
+`;
+
+  await writeFile(
+    join(ROOT_DIR, 'universal_rules', 'documentation', 'file_organization.md'),
+    docContent
+  );
}

/**
 * Extract project structure rules
 */
async function extractProjectStructureRules() {
  console.log('  Extracting project structure rules...');
+
+  const structureContent = `---
+title: Project Structure and File Standards
+category: universal_rules
+type: project_structure
+applies_to: all
+version: 1.0.0
+last_updated: ${new Date().toISOString().split('T')[0]}
+---
+
+# Project Structure and File Standards
+
+## Directory Layout
+
+\`\`\`
+project/
+├── docs/                    # Documentation
+│   ├── YYYY_MM_DD/         # Daily work
+│   └── plans/              # Strategic plans
+├── templates/              # Reusable templates
+├── examples/               # Usage examples
+└── src/                    # Source code (if applicable)
+\`\`\`
+
+## File Size Limits
+
+- **Maximum 250-300 lines** per file
+- Split large files into smaller modules
+- Each file should have a single responsibility
+- This applies to documentation and code
+
+## Template Standards
+
+- Place templates in \`templates/\` directory
+- Use clear naming conventions
+- Include usage instructions
+- Keep templates up to date
+`;
+
+  await writeFile(
+    join(ROOT_DIR, 'universal_rules', 'project_structure', 'directory_layout.md'),
+    structureContent
+  );
}

/**
 * Extract AI tool usage rules
 */
async function extractAiToolUsageRules() {
  console.log('  Extracting AI tool usage rules...');
+
+  const usageContent = `---
+title: AI Tool Usage Guidelines
+category: universal_rules
+type: ai_tool_usage
+applies_to: all
+version: 1.0.0
+last_updated: ${new Date().toISOString().split('T')[0]}
+---
+
+# AI Tool Usage Guidelines
+
+## MCP Protocol Usage
+
+- Use MCP (Model Context Protocol) for tool integration
+- Follow MCP specification for requests and responses
+- Implement proper error handling
+- Use structured data formats (JSON)
+
+## Tool Selection
+
+- Choose the right tool for the task
+- Consider tool capabilities and limitations
+- Use multiple tools when beneficial
+- Document tool usage decisions
+
+## Context Management
+
+- Provide clear, concise context
+- Include relevant files and history
+- Avoid information overload
+- Structure prompts for best results
+`;
+
+  await writeFile(
+    join(ROOT_DIR, 'universal_rules', 'ai_tool_usage', 'tool_selection.md'),
+    usageContent
+  );
}

// ============================================================================
+// VALIDATION FUNCTIONS
+// ============================================================================

/**
 * Validate the new structure
 */
async function validateStructure() {
  console.log('\n🔍 Validating new structure...');
+
  let errors = [];
+
  // Check all language directories exist
+  for (const lang of LANGUAGES) {
+    const langPath = join(ROOT_DIR, 'code_guidelines', 'languages', lang);
+    if (!await fileExists(langPath)) {
+      errors.push(`Missing language directory: ${langPath}`);
+    }
+  }
+
  // Check all universal rule categories exist
+  for (const category of UNIVERSAL_CATEGORIES) {
+    const catPath = join(ROOT_DIR, 'universal_rules', category);
+    if (!await fileExists(catPath)) {
+      errors.push(`Missing universal rules directory: ${catPath}`);
+    }
+  }
+
  // Check all tool-specific directories exist
+  for (const tool of TOOLS) {
+    const toolPath = join(ROOT_DIR, 'ai_tool_specific', tool);
+    if (!await fileExists(toolPath)) {
+      errors.push(`Missing tool-specific directory: ${toolPath}`);
+    }
+  }
+
  if (errors.length > 0) {
+    console.log('✗ Validation failed:');
+    errors.forEach(error => console.log(`  - ${error}`));
+    return false;
+  }
+
  console.log('✓ Structure validation passed');
+  return true;
}

// ============================================================================
+// MAIN CLI
+// ============================================================================

/**
 * Main execution function
 */
async function main() {
  const command = process.argv[2];
+  const arg = process.argv[3];
+
  console.log('🔄 Rules Repository Migration Tool\n');
+
  switch (command) {
+    case '--analyze':
+      if (LANGUAGES.includes(arg)) {
+        await analyzeDifferences(arg, `${arg}-fp-style-guide`);
+      } else {
+        console.log(`✗ Unknown language: ${arg}. Available: ${LANGUAGES.join(', ')}`);
+      }
+      break;
+
+    case '--migrate':
+      if (LANGUAGES.includes(arg)) {
+        await migrateLanguage(arg);
+      } else if (arg === 'all') {
+        for (const lang of LANGUAGES) {
+          await migrateLanguage(lang);
+        }
+      } else {
+        console.log(`✗ Unknown language: ${arg}. Use 'all' or one of: ${LANGUAGES.join(', ')}`);
+      }
+      break;
+
+    case '--extract-universal':
+      await extractUniversalRules();
+      break;
+
+    case '--validate':
+      await validateStructure();
+      break;
+
+    case '--help':
+    default:
+      console.log(`
+Usage: node migrate_rules.js [command] [argument]
+
+Commands:
+  --analyze <language>      Analyze differences for a language (e.g., python)
+  --migrate <language|all>  Migrate a language or all languages
+  --extract-universal       Extract universal rules from existing files
+  --validate                Validate the new directory structure
+  --help                    Show this help message
+
+Examples:
+  node migrate_rules.js --analyze python
+  node migrate_rules.js --migrate python
+  node migrate_rules.js --migrate all
+  node migrate_rules.js --extract-universal
+  node migrate_rules.js --validate
+      `);
+  }
+}
+
+// Run if called directly
+if (process.argv[1] === __filename) {
+  main().catch(console.error);
+}
+
+export {
+  ensureDir,
+  readFile,
+  writeFile,
+  fileExists,
+  findFiles,
+  extractMetadata,
+  addMetadata,
+  generateCodeGuidelineMetadata,
+  generateUniversalRuleMetadata,
+  analyzeDifferences,
+  generateUnifiedVersion,
+  migrateLanguage,
+  extractUniversalRules,
+  validateStructure,
+  LANGUAGES,
+  TOOLS,
+  UNIVERSAL_CATEGORIES,
+  ROOT_DIR
+};
