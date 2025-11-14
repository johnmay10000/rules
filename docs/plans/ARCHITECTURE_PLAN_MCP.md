# MCP Server Architecture: Dynamic AI Rules & Language Detection

**Project**: Global AI Rules MCP Server  
**Type**: System Architecture & Implementation Strategy  
**Version**: 1.0.0  
**Status**: Architecture Planning Phase  

---

## 📋 Executive Summary

This document outlines the architecture for an MCP (Model Context Protocol) server that will transform our static global AI rules system into a dynamic, intelligent service. The MCP server will automatically detect programming languages, load appropriate FP style guides, validate code against patterns, and integrate seamlessly with AI coding assistants including Cursor, Kimi, Claude, and Gemini.

**Key Innovation**: Zero-configuration functional programming pattern enforcement. Users simply start coding, and their AI assistant automatically applies the correct language-specific functional programming patterns.

---

## 🎯 Project Goals

### Primary Objectives

1. **Automatic Language Detection**: Detect primary programming language from project files
2. **Dynamic Style Guide Loading**: Load appropriate FP style guide based on detected language
3. **Real-time Code Validation**: Validate code snippets against FP patterns
4. **Automatic Rule Application**: Create and apply AI tool rules files without manual intervention
5. **Multi-AI Tool Integration**: Support Cursor, Kimi, Claude, and Gemini simultaneously
6. **SetTodoList Integration**: Native task tracking integration for Kimi
7. **Cross-Tool Consistency**: Maintain same FP patterns across all AI assistants

### Success Metrics

- **Detection Accuracy**: >95% correct language detection
- **Setup Time**: <5 seconds from project start to rule application
- **User Experience**: Zero manual configuration required
- **Tool Coverage**: 4/4 major AI assistants supported
- **Language Coverage**: 6/6 languages (Python, TypeScript, Rust, Kotlin, Swift, Haskell)
- **Platform Coverage**: AWS, GCP auto-detection

---

## 🏗️ System Architecture

### High-Level Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                   AI Assistant (Cursor/Kimi/Claude/Gemini)  │
└────────────────────────┬────────────────────────────────────┘
                         │ MCP Protocol
                         ▼
┌─────────────────────────────────────────────────────────────┐
│  MCP Server (Node.js/TypeScript)                           │
│  Package: @modelcontextprotocol/sdk                        │
└──────────┬──────────────────┬──────────────────┬────────────┘
           │                  │                  │
           │                  │                  ▼
           │                  │         ┌──────────────────┐
           │                  │         │  Resources API   │
           │                  │         │  (Read-only)     │
           │                  │         └──────────────────┘
           │                  │         │ Style Guides     │
           │                  │         │ Templates        │
           │                  │         │ Examples         │
           │                  │         └──────────────────┘
           │                  ▼
           │         ┌──────────────────┐
           │         │   Tools API      │
           │         │  (Read-Write)    │
           │         └────────┬─────────┘
           │                  │
           ▼                  ▼
┌──────────────────┐  ┌──────────────────┐
│ detect_language  │  │ setup_project    │
│ validate_code    │  │ track_todo       │
│ load_style_guide │  │ ...              │
└────────┬─────────┘  └────────┬─────────┘
         │                     │
         ▼                     ▼
┌────────────────────────────────────────┐
│  Local Project Directory               │
│  - .py/.ts/.rs files                   │
│  - requirements.txt/Cargo.toml         │
│  - Git repository                      │
└────────────────────────────────────────┘
```

### Component Breakdown

**1. Core Server (index.ts)**
- MCP server entry point
- Tool registration
- Request routing
- Transport layer (stdio)

**2. Language Detection Engine** 
- File system analysis
- Pattern matching algorithms
- Confidence scoring
- Caching layer

**3. Style Guide Loader**
- Reads from disk: ai-tool/*.md files
- Content formatting
- Pattern extraction
- Cross-reference resolution

**4. Code Validator**
- AST parsing (tree-sitter)
- Pattern matching
- Violation detection
- Suggestion generation

**5. Project Setup Tool**
- Rules file generation
- Template application
- Path resolution
- Validation

**6. Resources API**
- Read-only access to style guides
- Structured data format
- Search capabilities
- Version tracking

### Data Flow

**Workflow 1: New Project Setup**
```
User creates project → Opens AI assistant
    ↓
AI calls: detect_language(".")
    ↓
MCP analyzes files → Returns: { language: "python", confidence: 0.98 }
    ↓
AI calls: load_style_guide({ language: "python", aiTool: "kimi" })
    ↓
MCP reads: kimi/python-fp-style-guide.md
    ↓
AI calls: setup_project_rules({ aiTool: "kimi", platform: "aws" })
    ↓
MCP creates: .kimirules (35 lines)
    ↓
AI: "I've set up Kimi with Python FP patterns. Start coding!"
```

**Workflow 2: Code Validation**
```
User writes code → AI assistant active
    ↓
AI calls: validate_code({ code: "def foo():
    try:
        return x
    except:", language: "python" })
    ↓
MCP analyzes → Finds: try/except (violates Result type pattern)
    ↓
MCP returns: {
  violations: [
    { line: 2, issue: "Uses exceptions", suggestion: "Use returns.Result" }
  ],
  complianceScore: 0.45
}
    ↓
AI suggests refactoring to use Result types
```

---

## 🛠️ Technical Implementation

### Technology Stack

- **Runtime**: Node.js 18+ or Deno
- **Language**: TypeScript 5.0+
- **MCP SDK**: @modelcontextprotocol/sdk
- **File System**: Node fs/promises
- **Pattern Matching**: glob + custom matchers
- **AST Parsing**: tree-sitter (optional, for code validation)
- **Validation**: Zod schemas
- **Testing**: vitest or Jest

### Detection Algorithm

```typescript
// Priority-based language detection
async function detectLanguage(dir: string): Promise<DetectionResult> {
  const checks = [
    // High confidence indicators
    { lang: "python", files: ["requirements.txt", "pyproject.toml", "setup.py"], weight: 1.0 },
    { lang: "typescript", files: ["tsconfig.json"], weight: 1.0 },
    { lang: "rust", files: ["Cargo.toml"], weight: 1.0 },
    
    // Medium confidence (source files)
    { lang: "python", pattern: "**/*.py", minCount: 3, weight: 0.8 },
    { lang: "typescript", pattern: "**/*.ts", minCount: 3, weight: 0.8 },
    
    // Lower confidence (generic indicators)
    { lang: "python", files: ["*.py"], weight: 0.6 }
  ];
  
  let bestMatch = { language: "unknown", confidence: 0 };
  
  for (const check of checks) {
    const matches = await glob(check.files || check.pattern, { cwd: dir });
    
    if (matches.length >= (check.minCount || 1)) {
      const confidence = check.weight * (matches.length / 10); // Normalize
      
      if (confidence > bestMatch.confidence) {
        bestMatch = {
          language: check.lang,
          confidence: Math.min(confidence, 1.0)
        };
      }
    }
  }
  
  return bestMatch;
}
```

### Platform Detection

```typescript
async function detectPlatform(dir: string): Promise<string[]> {
  const platforms = [];
  
  // AWS indicators
  if (await fileExists("serverless.yml") || 
      await fileExists("sam.yaml") ||
      await hasFileWithContent("*.tf", "aws_")) {
    platforms.push("aws");
  }
  
  // GCP indicators  
  if (await fileExists("app.yaml") ||
      await hasFileWithContent("*.tf", "google_")) {
    platforms.push("gcp");
  }
  
  return platforms;
}
```

---

## 📊 Implementation Phases

### **Phase 1: Core Foundation** (Estimated: 4-6 hours)

**Tasks:**
- [ ] Initialize TypeScript Node.js project
- [ ] Install MCP SDK and dependencies
- [ ] Create basic server structure
- [ ] Implement stdio transport
- [ ] Add tool registration framework

**Deliverables:**
- MCP server that responds to `ListToolsRequest`
- Basic tool skeleton structure
- Development environment configured
- CI/CD pipeline setup

### **Phase 2: Language Detection** (Estimated: 3-4 hours)

**Tasks:**
- [ ] Implement `detect_language` tool
- [ ] Add glob pattern matching
- [ ] Create confidence scoring algorithm
- [ ] Add caching layer for performance
- [ ] Write comprehensive tests

**Deliverables:**
- >95% accurate language detection
- Fast detection (<1 second)
- Tests with sample projects

### **Phase 3: Style Guide Loading** (Estimated: 3-4 hours)

**Tasks:**
- [ ] Implement `load_style_guide` tool
- [ ] Read style guides from disk
- [ ] Format content for AI consumption
- [ ] Extract key patterns and examples
- [ ] Add cross-reference resolution

**Deliverables:**
- Correct guide loading per language
- Proper path resolution
- Formatted, AI-ready content

### **Phase 4: Project Setup** (Estimated: 3-4 hours)

**Tasks:**
- [ ] Implement `setup_project_rules` tool
- [ ] Create rules file templates
- [ ] Add AI tool-specific content
- [ ] Handle platform detection
- [ ] Add config file creation

**Deliverables:**
- Automatic .cursorrules/.kimirules creation
- Appropriate content for each AI tool
- Platform-specific additions

### **Phase 5: Resources API** (Estimated: 2-3 hours)

**Tasks:**
- [ ] Implement `ListResources`
- [ ] Implement `ReadResource`
- [ ] Expose all style guides
- [ ] Expose templates and examples
- [ ] Add resource filtering

**Deliverables:**
- All guides accessible via MCP
- Search capabilities
- Structured data format

### **Phase 6: Advanced Features** (Estimated: 4-6 hours)

**Tasks:**
- [ ] Implement `validate_code` tool
- [ ] Add SetTodoList integration
- [ ] Create batch operations support
- [ ] Add platform detection
- [ ] Implement caching layer

**Deliverables:**
- Code validation working
- Task tracking integrated
- Performance optimizations

### **Phase 7: Testing & Polishing** (Estimated: 3-4 hours)

**Tasks:**
- [ ] Write unit tests
- [ ] Create integration tests
- [ ] Test with real projects
- [ ] Add error handling
- [ ] Create documentation

**Deliverables:**
- >80% test coverage
- Working with sample projects
- Comprehensive README

### **Phase 8: Integration** (Estimated: 2-3 hours)

**Tasks:**
- [ ] Create MCP configuration files
- [ ] Test with Kimi CLI
- [ ] Test with Cursor (when MCP available)
- [ ] Create example workflows
- [ ] Document usage patterns

**Deliverables:**
- MCP server working with AI assistants
- Clear usage examples
- Video/gif demonstrations

**Total Estimated Time**: 24-34 hours (~3-4 full work days)

---

## 🎓 User Experience After MCP

### **Scenario 1: New Python Project (30 seconds)**

```bash
# User creates new directory
mkdir data-pipeline && cd data-pipeline
echo "import polars as pl

def process_data(df: pl.DataFrame) -> pl.DataFrame:
    return df.filter(pl.col('status') == 'active')" > main.py

# User opens terminal, starts Kimi
kimi

# Kimi (with MCP):
# 1. Calls: detect_language(".")
# 2. Detects: Python (confidence: 0.98)
# 3. Loads: kimi/python-fp-style-guide.md
# 4. Calls: setup_project_rules({ aiTool: "kimi" })
# 5. Creates: .kimirules (30 lines)

# Kimi says:
"Detected Python project. I've set up Kimi with functional 
programming patterns including Result types, railway-oriented 
programming, and immutable data transformations. Using returns 
library for error handling. Start coding!"

# User asks: "Process this CSV safely"
# Kimi writes:
from returns.result import Result, Success, Failure
from returns.pipeline import flow
from returns.pointfree import map

def load_and_process(filepath: str) -> Result[pl.DataFrame, Exception]:
    return flow(
        validate_path(filepath),
        bind(read_csv),
        bind(validate_schema),
        bind(clean_data),
        bind(process_data)
    )

# User: Zero configuration, immediate FP patterns!
```

### **Scenario 2: TypeScript Web App (45 seconds)**

```bash
mkdir web-app && cd web-app
npm init -y
npm install next@latest typescript @types/node
touch tsconfig.json

# User: Opens in Cursor with MCP

# Cursor (with MCP):
# 1. Calls: detect_language(".")
# 2. Detects: TypeScript (confidence: 0.95)
# 3. Loads: cursor/typescript-fp-style-guide.md
# 4. Also detects: next.config.js → Next.js
# 5. Creates: .cursorrules (45 lines)

# Cursor suggests:
"I'll use fp-ts for railway-oriented programming with 
Next.js. Here are the key patterns: TaskEither for async 
operations, pipe for composition, and exhaustive error 
handling. Should I create the project structure?"

# User: "Yes, create a type-safe API route"
# Cursor generates:
import * as TE from 'fp-ts/TaskEither'
import { pipe } from 'fp-ts/function'

export default async function handler(
  req: NextApiRequest
): Promise<NextApiResponse> {
  return await pipe(
    validateRequest(req),
    TE.fromEither,
    TE.chain(processBusinessLogic),
    TE.chain(saveToDatabase),
    TE.map(createSuccessResponse),
    TE.mapLeft(handleError)
  )()
}
```

### **Scenario 3: Rust Project with Validation**

```bash
cargo new --bin cli-tool
cd cli-tool

# User: Starts coding

# AI with MCP:
# 1. Detects: Rust (confidence: 1.0)
# 2. Loads: rust-fp-style-guide.md
# 3. User writes: unwrap() call

# AI warns:
"I noticed you're using unwrap() which can panic. 
According to our Rust FP patterns, consider using 
Result with proper error handling. Here's the safer approach:"

# Suggests:
use std::fs::File;

// Instead of:
// let file = File::open("data.txt").unwrap();

// Use:
let file: Result<File, std::io::Error> = File::open("data.txt");
match file {
    Ok(f) => process_file(f),
    Err(e) => log_error(e),
}
```

---

## 📈 Impact & Benefits

### **For End Users**

- **Zero Config**: No manual file creation
- **Instant**: Patterns applied within seconds
- **Accurate**: >95% correct detection
- **Consistent**: Same patterns across projects
- **Flexible**: Works with any AI assistant

### **For Teams**

- **Faster Onboarding**: New projects set up automatically
- **Consistent Standards**: Same FP patterns everywhere
- **Reduced Errors**: AI enforces patterns in real-time
- **Better Collaboration**: Shared understanding of patterns

### **For AI Assistants**

- **Better Context**: Rules loaded dynamically
- **More Helpful**: Pattern-aware suggestions
- **Integrated**: SetTodoList, validation, setup in one flow
- **Smarter**: Learns project patterns over time

---

## 🔮 Future Enhancements

### **Phase 9: Machine Learning (Post MVP)**

- Learn project-specific patterns
- Suggest custom rules based on codebase
- Predict next actions
- Personalization layer

### **Phase 10: Multi-Language Projects**

- Detect polyglot projects
- Load multiple style guides
- Handle language interop patterns
- Mixed-language validation

### **Phase 11: Team Patterns**

- Learn from team behavior
- Suggest team-specific additions
- Codify team conventions
- Collaborative rule evolution

### **Phase 12: IDE Integration**

- VS Code extension
- JetBrains plugin
- Real-time validation
- In-editor suggestions

---

## 📋 Next Steps

**Immediate (This Session):**
1. ✅ Architecture plan created (this document)
2. ⏳ Create implementation plan (docs/plans/MCP_IMPLEMENTATION_PLAN.md)
3. ⏳ Create TODO list (docs/plans/MCP_IMPLEMENTATION_TODO.md)
4. ⏳ Get approval for implementation

**Short-term (Next Session):**
1. Create MCP server project structure
2. Implement core server foundation
3. Build language detection tool
4. Test with sample directories

**Medium-term (Next Week):**
1. Complete all phases (1-8)
2. Full integration testing
3. Documentation and examples
4. Beta release to team

**Long-term (Next Month):**
1. Community testing and feedback
2. Performance optimizations
3. Future enhancements (Phase 9-12)
4. Production release

---

## 🎯 Success Criteria

**Implementation Complete When:**
- ✅ MCP server responds to all tool requests
- ✅ Language detection >95% accurate
- ✅ Style guides load correctly
- ✅ Project setup creates appropriate files
- ✅ Works with Kimi CLI
- ✅ Tested with sample projects (Python, TypeScript, Rust)
- ✅ Documentation complete
- ✅ Team successfully uses it

**Production Ready When:**
- ✅ All tests passing
- ✅ Performance acceptable (<1s detection)
- ✅ Error handling robust
- ✅ Community feedback positive
- ✅ No critical bugs

---

**Document Metadata:**
- **Status**: Architecture Planning Complete
- **Created**: 2025-11-14
- **Version**: 1.0.0
- **Next**: Implementation Plan & TODO list
- **Estimated Implementation**: 24-34 hours
- **Priority**: High (game-changing feature)

🤖 Generated with [Kimi](https://kimi.ai)

**Note**: This architecture plan transforms the static global rules system into a dynamic, intelligent MCP service that automatically detects languages and applies appropriate functional programming patterns. This represents a significant evolution from manual file creation to zero-configuration AI-assisted development.
