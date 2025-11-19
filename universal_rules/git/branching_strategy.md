---
title: Git Branching Strategy
+category: universal_rules
+type: git
+applies_to: all
+version: 1.0.0
+last_updated: 2025-11-19
+---
+
+# Git Branching Strategy
+
+Universal branching strategy for all projects, applicable across all AI assistants and development workflows.
+
+---
+
+## Core Principles
+
+1. **Main Branch is Sacred**: `main` (or `master`) is always deployable
+2. **Branch per Feature**: Isolate work in dedicated branches
+3. **Descriptive Naming**: Branch names communicate intent clearly
+4. **Short-Lived Branches**: Merge frequently, avoid long-running branches
+5. **Clean History**: Rebase to maintain linear history before merging
+
+---
+
+## Branch Types and Naming Conventions
+
+### 1. Main Branch
+
+- **Name**: `main` (preferred) or `master`
+- **Purpose**: Production-ready code, always deployable
+- **Protection**: 
+  - Direct pushes prohibited
+  - Requires pull request review
+  - Status checks must pass
+  - Up-to-date with base branch required
+
+### 2. Feature Branches
+
+- **Pattern**: `feature/description` or `feat/description`
+- **Purpose**: New features, enhancements, additions
+- **Examples**:
+  - `feature/user-authentication`
+  - `feature/payment-processing`
+  - `feat/mcp-server-integration`
+  - `feature/2025-11-19-unified-rules-structure`
+
+### 3. Bugfix Branches
+
+- **Pattern**: `bugfix/description` or `fix/description`
+- **Purpose**: Bug fixes, defect corrections
+- **Examples**:
+  - `bugfix/login-validation-error`
+  - `fix/memory-leak-in-parser`
+  - `bugfix/2025-11-19-typo-in-docs`
+
+### 4. Hotfix Branches
+
+- **Pattern**: `hotfix/description` or `hotfix/issue-id`
+- **Purpose**: Critical production fixes (branch from `main`)
+- **Examples**:
+  - `hotfix/security-vulnerability`
+  - `hotfix/critical-payment-bug`
+  - `hotfix/HOTFIX-1234`
+
+### 5. Documentation Branches
+
+- **Pattern**: `docs/description`
+- **Purpose**: Documentation updates, README changes, guides
+- **Examples**:
+  - `docs/update-readme`
+  - `docs/add-api-documentation`
+  - `docs/2025-11-19-migration-guide`
+
+### 6. Refactoring Branches
+
+- **Pattern**: `refactor/description`
+- **Purpose**: Code restructuring without behavior changes
+- **Examples**:
+  - `refactor/extract-auth-module`
+  - `refactor/simplify-validation-logic`
+  - `refactor/2025-11-19-remove-duplication`
+
+### 7. Experimental/WIP Branches
+
+- **Pattern**: `experiment/description` or `wip/description`
+- **Purpose**: Spikes, prototypes, experimental work
+- **Note**: May never be merged, for learning only
+- **Examples**:
+  - `experiment/rust-fp-patterns`
+  - `wip/try-new-architecture`
+
+---
+
+## Branch Lifecycle Workflow
+
+### Step 1: Create Branch
+
+```bash
+# Always start from updated main
+git checkout main
+git pull origin main
+
+# Create feature branch
+git checkout -b feature/description
+
+# Or create bugfix
+git checkout -b bugfix/issue-description
+```
+
+### Step 2: Work on Branch
+
+```bash
# Make changes
+git add .
+git commit -m "Brief summary (50-72 chars)
+
+Detailed description of changes
+
Rationale for changes
+
+Files affected: list of files
+
+Status: Task X complete
+
+🤖 Generated with [AI Assistant]
+Co-Authored-By: AI <noreply@anthropic.com>"
+
+# Keep branch up-to-date with main (rebase frequently)
+git fetch origin
+git rebase origin/main
+```
+
+**Important**: Rebase instead of merge to maintain clean history:
+- `git rebase origin/main` (preferred)
+- NOT `git merge origin/main` (creates merge commits)
+
+### Step 3: Prepare for Merge
+
+```bash
# Ensure branch is up-to-date
+git fetch origin
+git rebase origin/main
+
+# Run tests and checks
+cargo test          # Rust
+npm test            # TypeScript/JavaScript
+pytest              # Python
+mypy .              # Type checking
+
# Review commits
+git log --oneline -n 10
+
# Push branch to remote
+git push origin feature/description
+```
+
+### Step 4: Create Pull Request
+
+Pull request requirements:
+- **Title**: Clear, descriptive (50-72 chars)
+- **Description**: 
+  - What changes were made
+  - Why they were needed
+  - How to test/verify
+  - Any breaking changes
+- **Reviewers**: At least one required reviewer
+- **Status Checks**: All must pass
+- **Linked Issues**: Reference any related issues
+
+Example PR description template:
+```markdown
+## Changes
+- Added user authentication module
+- Implemented JWT token generation
+- Added login/logout endpoints
+
+## Why
+Required for user management feature (Issue #123)
+
+## Testing
+1. Run `cargo test auth`
+2. Test login endpoint: POST /api/login
+3. Verify token is returned
+
+## Breaking Changes
+None
+
+## Checklist
+- [x] Tests pass
+- [x] Documentation updated
+- [x] Code reviewed
+```
+
+### Step 5: Merge
+
+**Preferred: Squash and Merge**
+- Keeps main branch history clean
+- One commit per feature/bugfix
+- Commit message follows standard format
+
+**Alternative: Rebase and Merge**
+- Preserves individual commits
+- Only if commits are well-structured and atomic
+
+**NOT Recommended: Merge Commit**
+- Creates unnecessary merge commits
+- Makes history harder to follow
+
+### Step 6: Cleanup
+
+```bash
# Delete local branch
+git checkout main
+git pull origin main
+git branch -d feature/description
+
# Delete remote branch
+git push origin --delete feature/description
+
# Verify cleanup
+git branch  # Should not show deleted branch
+git fetch --prune  # Clean up remote tracking branches
+```
+
+---
+
+## Branch Management Best Practices
+
+### Keep Branches Short-Lived
+
+- **Maximum lifetime**: 2-3 days for feature branches
+- **Maximum commits**: 10-15 commits per branch
+- **If longer**: Break into smaller branches or rebase frequently
+
+### Rebase Frequently
+
+```bash
# Rebase at least once per day
+git fetch origin
+git rebase origin/main
+
# If conflicts occur:
+git status                    # See conflicted files
+# Edit files to resolve conflicts
+git add <resolved-files>
+git rebase --continue
+```
+
+### Atomic Commits
+
+Each commit should:
+- Pass all tests
+- Represent one logical change
+- Have a clear, descriptive message
+- Be reviewable independently
+
+### Branch Naming Examples by Scenario
+
+**Feature Development**:
+```bash
+git checkout -b feature/user-profile-page
+git checkout -b feature/api-rate-limiting
+git checkout -b feature/2025-11-19-mcp-integration
+```
+
+**Bug Fixes**:
+```bash
+git checkout -b bugfix/login-form-validation
+git checkout -b fix/memory-leak-in-parser
+git checkout -b bugfix/HOTFIX-1234-critical-bug
+```
+
+**Documentation**:
+```bash
+git checkout -b docs/update-api-documentation
+git checkout -b docs/add-migration-guide
+git checkout -b docs/2025-11-19-readme-update
+```
+
+**Refactoring**:
+```bash
+git checkout -b refactor/extract-validation-logic
+git checkout -b refactor/simplify-error-handling
+git checkout -b refactor/remove-code-duplication
+```
+
+---
+
+## Working with Long-Running Branches
+
+### Release Branches
+
+For projects with scheduled releases:
+
+- **Pattern**: `release/v1.2.0`
+- **Purpose**: Prepare for production release
+- **Lifecycle**: 
+  1. Create from `main` when ready to stabilize
+  2. Apply only bugfixes (no new features)
+  3. Merge to `main` and tag release
+  4. Delete after release
+
+### Support/Maintenance Branches
+
+For supporting older versions:
+
+- **Pattern**: `support/v1.x`
+- **Purpose**: Backport bugfixes to older versions
+- **Lifecycle**: Long-lived, merged to from bugfix branches
+
+---
+
+## Common Scenarios
+
+### Scenario 1: Quick Documentation Fix
+
+```bash
+git checkout main
+git pull origin main
+git checkout -b docs/fix-typo-in-readme
+# Edit README.md
+git add README.md
+git commit -m "Fix typo in README.md
+
+Corrected spelling error in installation section.
+
+Improves documentation accuracy.
+
+Files: README.md
+
+Status: Quick fix ✅
+
+🤖 Generated with [AI Assistant]
+Co-Authored-By: AI <noreply@anthropic.com>"
+git push origin docs/fix-typo-in-readme
+# Create PR, merge, delete branch
+```
+
+### Scenario 2: Multi-Day Feature Development
+
+```bash
+# Day 1
+git checkout main
+git pull origin main
+git checkout -b feature/mcp-server-implementation
+# Work and commit throughout day
+git commit -m "..."
+git push origin feature/mcp-server-implementation
+
+# Day 2
+git fetch origin
+git rebase origin/main  # Get latest changes
+# Continue work
+git commit -m "..."
+git push origin feature/mcp-server-implementation --force-with-lease
+
# Day 3 (ready to merge)
+git fetch origin
+git rebase origin/main
+cargo test  # Run tests
+git push origin feature/mcp-server-implementation
+# Create PR, get review, merge
+```
+
+### Scenario 3: Emergency Hotfix
+
+```bash
# Create hotfix from main
+git checkout main
+git pull origin main
+git checkout -b hotfix/security-vulnerability
+
# Fix critical issue
+# Edit files...
+git add .
+git commit -m "HOTFIX: Patch security vulnerability CRITICAL
+
+Fixed SQL injection vulnerability in user input validation.
+
+Immediate production fix required. No breaking changes.
+
+Files: src/auth.rs, src/validation.rs
+
+Status: Emergency hotfix ✅
+
+🤖 Generated with [AI Assistant]
+Co-Authored-By: AI <noreply@anthropic.com>"
+
# Fast-track review and merge
+git push origin hotfix/security-vulnerability
+# Create PR, expedited review, merge to main
+
# Deploy immediately
+git checkout main
+git pull origin main
+# Deploy to production
+```
+
+---
+
+## Branch Protection Rules
+
+### Required for Main Branch
+
+Enable these protection rules:
+
+1. **Require pull request reviews**: At least 1 approval
+2. **Require status checks**: All CI checks must pass
+3. **Require branches to be up to date**: Enforce rebasing
+4. **Restrict pushes**: Only maintainers can push to main
+5. **Require linear history**: No merge commits
+6. **Include administrators**: Rules apply to everyone
+
+### Recommended for Feature Branches
+
+Optional but helpful:
+
+1. **Require issue linking**: PR must reference issue
+2. **Require signed commits**: GPG signature verification
+3. **Auto-delete after merge**: Clean up merged branches
+
+---
+
+## Troubleshooting
+
### Problem: Branch is out of date
+
+```bash
+git fetch origin
+git rebase origin/main
+```
+
### Problem: Merge conflicts during rebase
+
+```bash
+# Git will pause and show conflicted files
+git status  # See conflicted files
+
# Edit files to resolve conflicts
+git add <resolved-files>
+git rebase --continue
+
# If you need to abort
+git rebase --abort
+```
+
### Problem: Accidentally committed to main
+
+```bash
# Create branch from current state
+git checkout -b feature/my-changes
+
# Reset main to origin
+git checkout main
+git fetch origin
+git reset --hard origin/main
+
# Now work on feature branch
+git checkout feature/my-changes
+git push origin feature/my-changes
+```
+
### Problem: Branch name was wrong
+
+```bash
# Rename local branch
+git checkout old-branch-name
+git branch -m new-branch-name
+
# Update remote
+git push origin -u new-branch-name
+git push origin --delete old-branch-name
+```
+
+---
+
+## Summary
+
+**Key Takeaways**:
+- Always branch from updated `main`
+- Use descriptive, consistent naming
+- Rebase frequently to stay current
+- Keep branches short-lived (2-3 days max)
+- Clean up after merging
+- Protect main branch with rules
+
**Branch Naming Quick Reference**:
+- Features: `feature/description`
+- Bugs: `bugfix/description`
+- Hotfixes: `hotfix/description`
+- Docs: `docs/description`
+- Refactor: `refactor/description`
+
+---
+
+**Last Updated**: 2025-11-19  
+**Maintained By**: Global AI Rules System  
+**Status**: Active  
+**Applies To**: All AI assistants (Cursor, Kimi, Claude, Gemini)
