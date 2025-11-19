---
title: File Size Limits and Organization
+category: universal_rules
+type: project_structure
+applies_to: all
+version: 1.0.0
+last_updated: 2025-11-19
+---
+
+# File Size Limits and Organization
+
+**Status**: MANDATORY - All files must adhere to size limits
+
+Universal file size limits that apply to ALL projects regardless of programming language, technology stack, or AI assistant used (Cursor, Kimi, Claude, Gemini).
+
+---
+
+## Core Principle: Small, Focused Files
+
+### Maximum File Size: 250-300 Lines
+
+**Rule**: No single file may exceed **300 lines of code/content**.
+
+**Recommended Target**: 150-200 lines per file for optimal readability and maintainability.
+
+**Rationale**:
+- **Cognitive Load**: Human brain can only process ~200 lines of context at once
+- **Code Review**: Small files are easier to review thoroughly
+- **Navigation**: Easier to find specific functionality
+- **Testing**: Simpler to test focused modules
+- **Refactoring**: Less risk when modifying small files
+- **AI Assistant Context**: Better fits in context windows
+- **Git History**: Clearer commit history with focused changes
+
+**What Counts Toward Limit**:
+- ✅ Code lines (functions, classes, logic)
+- ✅ Comments (explanatory comments)
+- ✅ Type definitions (TypeScript interfaces, Rust types)
+- ✅ Configuration within code files
+
+**What Does NOT Count**:
+- ❌ Import/require statements at top of file
+- ❌ License headers (if required)
+- ❌ Generated code (marked with `@generated` or similar)
+- ❌ Data files (JSON, CSV, etc. - separate concern)
+
+---
+
+## Why File Size Limits Matter
+
+### Problem: Large Files Are Code Smells
+
+**Large files indicate**:
+- **Multiple responsibilities** (violates Single Responsibility Principle)
+- **Poor abstraction** (opportunity to extract concepts)
+- **Technical debt** (accumulated without refactoring)
+- **Difficulty testing** (too much to mock/isolate)
+- **Merge conflicts** (many developers editing same file)
+- **Slow compilation** (unnecessary recompilation of unchanged code)
+
+### Benefits of Small Files
+
+1. **Improved Readability**
+   - Can read entire file in one screen
+   - Clear purpose and scope
+   - Easier to understand context
+
+2. **Better Maintainability**
+   - Changes are localized
+   - Lower risk of bugs
+   - Easier to update
+
+3. **Enhanced Testability**
+   - Fewer dependencies to mock
+   - Clear test boundaries
+   - Higher test coverage
+
+4. **Easier Code Review**
+   - Reviewers can focus on specific functionality
+   - Faster review cycles
+   - Better feedback quality
+
+5. **Reduced Merge Conflicts**
+   - Fewer developers editing same file
+   - Conflicts are easier to resolve
+   - Parallel development easier
+
+6. **Better AI Assistant Performance**
+   - Fits better in context windows
+   - More focused suggestions
+   - Better understanding of scope
+
+---
+
+## How to Split Large Files
+
+### Strategy 1: Extract by Responsibility
+
+**Before**: Single file with multiple responsibilities
+```typescript
+// user.ts (350 lines) ❌ Too large
+export class User {
+  // Authentication methods
+  login() { /* ... */ }
+  logout() { /* ... */ }
+  validatePassword() { /* ... */ }
+  
+  // Profile methods
+  updateProfile() { /* ... */ }
+  uploadAvatar() { /* ... */ }
+  
+  // Permission methods
+  checkPermission() { /* ... */ }
+  assignRole() { /* ... */ }
+  
+  // Notification methods
+  sendEmail() { /* ... */ }
+  sendSMS() { /* ... */ }
+}
+```
+
+**After**: Multiple focused files
+```typescript
+// user-auth.ts (80 lines) ✅
+export class UserAuth {
+  login() { /* ... */ }
+  logout() { /* ... */ }
+  validatePassword() { /* ... */ }
+}
+
+// user-profile.ts (90 lines) ✅
+export class UserProfile {
+  updateProfile() { /* ... */ }
+  uploadAvatar() { /* ... */ }
+}
+
+// user-permissions.ts (85 lines) ✅
+export class UserPermissions {
+  checkPermission() { /* ... */ }
+  assignRole() { /* ... */ }
+}
+
+// user-notifications.ts (75 lines) ✅
+export class UserNotifications {
+  sendEmail() { /* ... */ }
+  sendSMS() { /* ... */ }
+}
+
+// user/index.ts (20 lines) ✅
+export { UserAuth } from './user-auth'
+export { UserProfile } from './user-profile'
+export { UserPermissions } from './user-permissions'
+export { UserNotifications } from './user-notifications'
+```
+
+### Strategy 2: Extract Utility Functions
+
+**Before**: Utility functions mixed with main logic
+```python
+# data_processor.py (320 lines)
+class DataProcessor:
+    def process(self, data):
+        # Validation logic (50 lines)
+        if not self.validate(data):
+            return None
+        
+        # Transformation logic (80 lines)
+        transformed = self.transform(data)
+        
+        # Enrichment logic (70 lines)
+        enriched = self.enrich(transformed)
+        
+        # Storage logic (100 lines)
+        self.store(enriched)
+```
+
+**After**: Extract utilities to separate files
+```python
+# validators.py (60 lines) ✅
+def validate_data(data):
+    # Validation implementation
+    pass
+
+# transformers.py (85 lines) ✅
+def transform_data(data):
+    # Transformation implementation
+    pass
+
+# enrichers.py (75 lines) ✅
+def enrich_data(data):
+    # Enrichment implementation
+    pass
+
+# storage.py (90 lines) ✅
+def store_data(data):
+    # Storage implementation
+    pass
+
+# data_processor.py (45 lines) ✅
+from validators import validate_data
+from transformers import transform_data
+from enrichers import enrich_data
+from storage import store_data
+
+class DataProcessor:
+    def process(self, data):
+        if not validate_data(data):
+            return None
+        
+        transformed = transform_data(data)
+        enriched = enrich_data(transformed)
+        store_data(enriched)
+        return enriched
+```
+
+### Strategy 3: Extract by Feature
+
+**Before**: Single module handling multiple features
+```rust
+// api.rs (380 lines)
+pub async fn handle_request(req: Request) -> Response {
+    match req.path {
+        "/users" => handle_users(req).await,      // 120 lines
+        "/posts" => handle_posts(req).await,      // 110 lines
+        "/comments" => handle_comments(req).await, // 110 lines
+        _ => Response::not_found(),
+    }
+}
+```
+
+**After**: Feature-based modules
+```rust
+// api/mod.rs (25 lines) ✅
+pub mod users;
+pub mod posts;
+pub mod comments;
+
+pub async fn handle_request(req: Request) -> Response {
+    match req.path {
+        "/users" => users::handle(req).await,
+        "/posts" => posts::handle(req).await,
+        "/comments" => comments::handle(req).await,
+        _ => Response::not_found(),
+    }
+}
+
+// api/users.rs (115 lines) ✅
+pub async fn handle(req: Request) -> Response {
+    // User-specific logic
+}
+
+// api/posts.rs (105 lines) ✅
+pub async fn handle(req: Request) -> Response {
+    // Post-specific logic
+}
+
+// api/comments.rs (105 lines) ✅
+pub async fn handle(req: Request) -> Response {
+    // Comment-specific logic
+}
+```
+
+---
+
+## File Type-Specific Guidelines
+
+### Source Code Files
+
+**Limit**: 250-300 lines maximum
+
+**Structure**:
+```typescript
+// ✅ Good: Focused file (180 lines)
+// user-authentication.ts
+
+// 1. Imports (10-15 lines)
+import { hashPassword } from './crypto'
+import { createToken } from './jwt'
+import type { User } from './types'
+
+// 2. Main class/function (120-150 lines)
+export class UserAuthenticator {
+  async login(email: string, password: string): Promise<Result<AuthToken, AuthError>> {
+    // Implementation
+  }
+  
+  async logout(token: string): Promise<Result<void, AuthError>> {
+    // Implementation
+  }
+  
+  async validate(token: string): Promise<Result<User, AuthError>> {
+    // Implementation
+  }
+}
+
+// 3. Helper functions (30-40 lines)
+function validateEmailFormat(email: string): boolean {
+  // Implementation
+}
+
+// 4. Exports (5 lines)
+export type { AuthToken, AuthError }
+export { UserAuthenticator }
+```
+
+### Test Files
+
+**Limit**: 200-250 lines maximum (tests should be even more focused)
+
+**Structure**:
+```python
+# ✅ Good: Focused test file (160 lines)
+# test_user_authentication.py
+
+# 1. Imports and setup (15 lines)
+import pytest
+from user_authentication import UserAuthenticator
+from fixtures import create_test_user
+
+# 2. Test class with focused tests (130 lines)
+class TestUserAuthentication:
+    def test_login_with_valid_credentials_succeeds(self):
+        # 10-15 lines per test
+        auth = UserAuthenticator()
+        result = auth.login("test@example.com", "Secure123")
+        assert result.is_success()
+    
+    def test_login_with_invalid_password_fails(self):
+        auth = UserAuthenticator()
+        result = auth.login("test@example.com", "WrongPass")
+        assert result.is_error()
+        assert result.error_code == "INVALID_CREDENTIALS"
+    
+    def test_login_with_nonexistent_user_fails(self):
+        # Another focused test
+        pass
+
+# 3. Helper fixtures (15 lines)
+@pytest.fixture
+def authenticator():
+    return UserAuthenticator()
+```
+
+### Documentation Files
+
+**Limit**: 250-300 lines maximum
+
+**Structure**:
+```markdown
+<!-- ✅ Good: Focused documentation (220 lines) -->
+<!-- docs/user-authentication.md -->
+
+# User Authentication
+
+## Overview (20 lines)
+Brief explanation of authentication system
+
+## Setup (40 lines)
+```bash
+# Installation steps
+npm install auth-package
+```
+
+## Usage (80 lines)
+```typescript
+// Code examples
+const auth = new UserAuthenticator()
+await auth.login(email, password)
+```
+
+## API Reference (60 lines)
+### `login(email, password)`
+Parameters, return values, examples
+
+## Troubleshooting (20 lines)
+Common issues and solutions
+```
+
+If documentation needs more detail, split into multiple files:
+- `docs/user-authentication-overview.md`
+- `docs/user-authentication-setup.md`
+- `docs/user-authentication-api.md`
+- `docs/user-authentication-examples.md`
+
+### Configuration Files
+
+**Limit**: 150-200 lines maximum
+
+If configuration grows too large:
+- Split by environment (`config/dev.ts`, `config/prod.ts`, `config/test.ts`)
+- Split by concern (`config/database.ts`, `config/api.ts`, `config/logging.ts`)
+- Use composition pattern
+
+---
+
+## Enforcement Mechanisms
+
+### 1. Manual Code Review
+
+**Checklist for Reviewers**:
+- [ ] No file exceeds 300 lines
+- [ ] Each file has single, clear responsibility
+- [ ] Files are well-organized and focused
+- [ ] Related functions are grouped logically
+- [ ] Extracted modules make sense
+
+### 2. Automated Linting
+
+**ESLint Example**:
+```javascript
+// .eslintrc.js
+module.exports = {
+  rules: {
+    'max-lines': ['error', {
+      max: 300,
+      skipBlankLines: true,
+      skipComments: false
+    }]
+  }
+}
+```
+
+**Python Example**:
+```ini
+# .flake8
+[flake8]
+max-lines = 300
+```
+
+**Rust Example**:
+```rust
+// clippy.toml
+too-many-lines-threshold = 300
+```
+
+### 3. Pre-commit Hooks
+
+```bash
+#!/bin/bash
+# .git/hooks/pre-commit
+
+# Check file sizes
+for file in $(git diff --cached --name-only --diff-filter=ACM | grep -E '\.(ts|py|rs|js|jsx|tsx|go|java|kt|swift)$'); do
+  lines=$(wc -l < "$file")
+  if [ "$lines" -gt 300 ]; then
+    echo "❌ File $file has $lines lines (max 300)"
+    echo "   Please split into smaller, focused files"
+    exit 1
+  fi
+done
+
+echo "✅ All files are within size limits"
+```
+
+### 4. CI/CD Enforcement
+
+```yaml
+# .github/workflows/file-size-check.yml
+name: File Size Check
+
+on: [pull_request]
+
+jobs:
+  check-file-sizes:
+    runs-on: ubuntu-latest
+    steps:
+      - uses: actions/checkout@v3
+      
+      - name: Check file sizes
+        run: |
+          for file in $(find src -name "*.ts" -o -name "*.py" -o -name "*.rs"); do
+            lines=$(wc -l < "$file")
+            if [ "$lines" -gt 300 ]; then
+              echo "::error file=$file::File exceeds 300 lines ($lines)"
+              exit 1
+            fi
+          done
+```
+
+---
+
+## Exceptions (Rare)
+
+### When to Allow Larger Files
+
+**Valid Exceptions** (must be documented):
+1. **Generated files** (clearly marked with `@generated`)
+2. **Data files** (JSON, CSV, etc. - not source code)
+3. **Third-party code** (vendored dependencies)
+4. **Configuration schemas** (OpenAPI, GraphQL schemas)
+
**Invalid Exceptions**:
+- "This file is just complex" → Refactor instead
+- "It's always been this way" → Technical debt, fix it
+- "I don't have time" → Make time, or create ticket to address later
+
+### Exception Documentation
+
+If a file must exceed 300 lines, document why:
+
+```typescript
+// api-types.ts
+// EXCEPTION: This file contains generated TypeScript types from OpenAPI spec
+// Size: 450 lines
+// Reason: Automatically generated from api-spec.yaml, should not be manually edited
+// Generation: Run `npm run generate-types` to regenerate
+// @generated
+
+export interface User { /* ... */ }
+// ... many types
+```
+
+---
+
+## Migration Strategy for Large Files
+
+### Step 1: Identify Large Files
+
+```bash
+# Find files over 300 lines
+find src -name "*.ts" -o -name "*.py" -o -name "*.rs" | xargs wc -l | sort -n
+
+# Or use cloc
+cloc src --by-file | sort -k4 -n
+```
+
+### Step 2: Analyze File Structure
+
+Identify natural boundaries:
+- Groups of related functions
+- Different classes/modules
+- Different responsibilities
+- Different abstraction levels
+
+### Step 3: Extract New Modules
+
+1. Create new file for extracted functionality
+2. Move code to new file
+3. Update imports/references
+4. Run tests to ensure nothing broke
+5. Commit the refactoring
+
+### Step 4: Update Imports
+
+```typescript
+// Old: Everything in one file
+import { UserManager } from './user-manager'
+
+// New: Split into focused modules
+import { UserAuthenticator } from './user-authenticator'
+import { UserProfile } from './user-profile'
+import { UserPermissions } from './user-permissions'
+```
+
+### Step 5: Verify and Commit
+
+```bash
+# Run all tests
+npm test
+
+# Check for circular dependencies
+madge --circular src/
+
# Commit the refactoring
+git add .
+git commit -m "Refactor: Split user-manager.ts into focused modules
+
+Split monolithic 350-line file into 4 focused modules:
+- user-authenticator.ts (90 lines)
+- user-profile.ts (85 lines)
+- user-permissions.ts (80 lines)
+- user-notifications.ts (75 lines)
+
+Rationale: File exceeded 300-line limit and had multiple responsibilities.
+Each module now has a single, clear purpose.
+
+Impact: Improves maintainability, testability, and code review.
+All tests pass, no breaking changes.
+
+Status: Refactoring complete ✅
+
+🤖 Generated with [AI Assistant]
+Co-Authored-By: AI <noreply@anthropic.com>"
+```
+
+---
+
+## Tool-Specific Notes
+
+### For Kimi Users
+
+Kimi provides enhanced support for file organization:
+- **Parallel Analysis**: Analyze multiple files simultaneously for size violations
+- **Refactoring Suggestions**: Recommend optimal file splits
+- **Dependency Analysis**: Ensure extracted modules don't create circular dependencies
+- **SetTodoList Integration**: Track refactoring tasks
+
+**Kimi-Specific Workflow**:
+```bash
+# Check for large files
+kimi analyze --file-sizes --threshold=300
+
+# Get refactoring suggestions
+kimi refactor --file=user-manager.ts --strategy=by-responsibility
+
+# Verify no circular dependencies
+kimi check-dependencies
+```
+
+### For Cursor Users
+
+Cursor provides IDE-integrated file management:
+- **Inline Size Indicators**: See file size in editor
+- **Refactoring Tools**: Automated extraction to new files
+- **Import Management**: Automatic import updates
+- **Code Lens**: See which functions contribute most to file size
+
+### For Claude Users
+
+Claude excels at architectural refactoring:
+- **Responsibility Analysis**: Identify natural boundaries
+- **Extraction Strategy**: Recommend optimal split points
+- **Dependency Management**: Ensure clean module structure
+- **Best Practices**: Detailed refactoring guidance
+
+### For Gemini Users
+
+Gemini provides comprehensive code organization guidance:
+- **Multi-Language**: File organization patterns across languages
+- **Architecture Patterns**: Module design best practices
+- **Anti-Pattern Detection**: Identify god objects and fat files
+- **Metrics**: Code complexity and size analysis
+
+---
+
+## Related Documents
+
+- **Directory Layout**: `directory_layout.md` - Where to place files
+- **Naming Conventions**: `../documentation/naming_conventions.md` - How to name files
+- **Testing**: `../testing/unit_testing.md` - Testing small, focused modules
+- **Code Guidelines**: `../../code_guidelines/` - Language-specific patterns
+
+---
+
+**Last Updated**: 2025-11-19  
+**Maintained By**: Global AI Rules System  
+**Status**: Active  
+**Applies To**: All AI assistants (Cursor, Kimi, Claude, Gemini) and all programming languages