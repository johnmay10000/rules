# USER_PROFILE_TODO.md - Task Tracking

**Paired With**: [USER_PROFILE_PLAN.md](USER_PROFILE_PLAN.md) ⭐  
**Status**: ⏳ IN PROGRESS  
**Last Updated**: 2025-10-31 14:30  
**Progress**: 13/24 tasks (54%)  

> **🤖 Cursor Updates**: Cursor automatically updates this file as tasks complete. Mark tasks [x] and add actual time spent.

---

## Phase 1: Data Model and API Endpoints ✅

**Status**: ✅ COMPLETE  
**Progress**: 6/6 tasks (100%)  
**Time**: Est: 4h | Actual: 4.2h  

- [x] **1.1**: Design database schema for user profiles (Est: 30min, Actual: 35min)
  - Created Prisma schema with all required fields
  - Added JSONB for preferences flexibility
  - Files: `prisma/schema.prisma`
  - Commit: `a1b2c3d - Add user profile schema`

- [x] **1.2**: Create and run Prisma migrations (Est: 15min, Actual: 20min)
  - Generated migration files
  - Tested on dev database
  - Files: `prisma/migrations/20251029_add_profile/`
  - Commit: `e4f5g6h - Add profile migration`

- [x] **1.3**: Implement GET /api/profile endpoint (Est: 1h, Actual: 1.1h)
  - Uses TaskEither for error handling
  - Returns 404 if profile not found
  - All errors properly typed
  - Files: `src/app/api/profile/route.ts`
  - Commit: `i7j8k9l - Implement GET /api/profile`

- [x] **1.4**: Implement PUT /api/profile endpoint (Est: 1h, Actual: 55min)
  - Validates input with zod
  - Updates profile fields
  - Returns updated profile
  - Files: `src/app/api/profile/route.ts`
  - Commit: `m0n1o2p - Implement PUT /api/profile`

- [x] **1.5**: Create zod validation schemas (Est: 30min, Actual: 25min)
  - Schemas for all profile fields
  - Email validation
  - Bio length limits (500 chars)
  - Files: `src/lib/validation/profile.ts`
  - Commit: `q3r4s5t - Add profile validation`

- [x] **1.6**: Write integration tests for API (Est: 1h, Actual: 1.1h)
  - Tests for GET endpoint (happy path + errors)
  - Tests for PUT endpoint (validation + updates)
  - 100% coverage for API routes
  - Files: `tests/api/profile.test.ts`
  - Commit: `u6v7w8x - Add profile API tests`

**Phase Totals**:  
Est: 4h | Actual: 4.2h | Variance: +5%

---

## Phase 2: Avatar Upload System ⏳

**Status**: ⏳ IN PROGRESS (Backend ✅, Frontend ⏳)  
**Progress**: 4/7 tasks (57%)  
**Time**: Est: 4h | Actual: 2.5h | Remaining: ~1.5h  

- [x] **2.1**: Set up S3 bucket and configure CORS (Est: 30min, Actual: 45min)
  - Created S3 bucket `user-avatars-prod`
  - Configured CORS policy
  - Set up bucket lifecycle rules (delete after 30 days if orphaned)
  - Files: `infrastructure/aws/s3.tf`
  - Commit: `y9z0a1b - Set up S3 for avatars`
  - Note: CORS issues resolved after updating policy

- [x] **2.2**: Create IAM policy for S3 access (Est: 20min, Actual: 30min)
  - Created least-privilege policy
  - Attached to Lambda execution role
  - Tested upload/delete permissions
  - Files: `infrastructure/aws/iam.tf`
  - Commit: `c2d3e4f - Add S3 IAM policy`
  - Note: Initial policy too restrictive, fixed in follow-up commit

- [x] **2.3**: Implement presigned URL generation (Est: 45min, Actual: 40min)
  - POST /api/profile/avatar/upload endpoint
  - Generates presigned URL with 15min expiry
  - Validates file type (jpg, png, webp only)
  - Files: `src/app/api/profile/avatar/upload/route.ts`
  - Commit: `g5h6i7j - Add presigned URL generation`

- [x] **2.4**: Add image validation (size, type, dimensions) (Est: 30min, Actual: 35min)
  - Max size: 4MB
  - Allowed types: jpg, png, webp
  - Min dimensions: 128x128
  - Max dimensions: 4096x4096
  - Files: `src/lib/validation/image.ts`
  - Commit: `k8l9m0n - Add image validation`

- [ ] **2.5**: Build frontend upload UI with drag-and-drop (Est: 1h, Actual: -)
  - **IN PROGRESS** (Started: 2025-10-31 09:00)
  - Drag-and-drop zone component
  - File selection button
  - Preview before upload
  - Progress indicator
  - Files: `src/components/ProfilePage/AvatarUpload.tsx`
  - Current status: Basic UI complete, testing drag-and-drop

- [ ] **2.6**: Implement image resizing pipeline (Est: 45min, Actual: -)
  - **NOT STARTED**
  - Resize to 512x512 (profile view)
  - Resize to 128x128 (thumbnail)
  - Convert to WebP for efficiency
  - Options: Sharp library or Lambda
  - Blocked by: Need to decide on approach (Sharp vs Lambda)

- [ ] **2.7**: Create avatar display component (Est: 30min, Actual: -)
  - **NOT STARTED**
  - Fallback to initials if no avatar
  - Loading state
  - Error state (broken image)
  - Files: `src/components/ui/Avatar.tsx`
  - Dependencies: Task 2.5 complete

**Phase Totals**:  
Est: 4h | Actual: 2.5h | Remaining: ~1.5h

**Blockers**:
- Task 2.6: Need to decide on image resizing approach (Lambda adds latency, Sharp adds bundle size)

---

## Phase 3: User Preferences System 🔄

**Status**: 🔄 NOT STARTED  
**Progress**: 0/11 tasks (0%)  
**Planned Start**: 2025-11-01  

- [ ] **3.1**: Define preferences TypeScript types (Est: 30min, Actual: -)
  - Appearance preferences type
  - Notification preferences type
  - Privacy preferences type
  - Files: `src/lib/types/preferences.ts`

- [ ] **3.2**: Create preferences API endpoints (Est: 45min, Actual: -)
  - GET /api/profile/preferences
  - PUT /api/profile/preferences
  - Validate with zod
  - Files: `src/app/api/profile/preferences/route.ts`

- [ ] **3.3**: Build preferences UI shell (tabs/sections) (Est: 30min, Actual: -)
  - Tab navigation (Appearance, Notifications, Privacy)
  - Save/Cancel buttons
  - Dirty state indicator
  - Files: `src/components/ProfilePage/PreferencesPanel.tsx`

- [ ] **3.4**: Implement theme switcher (light/dark/auto) (Est: 45min, Actual: -)
  - Theme selector component
  - Real-time preview
  - Persist to database
  - Apply on page load
  - Files: `src/components/ProfilePage/ThemeSwitcher.tsx`

- [ ] **3.5**: Add font size preference (small/medium/large) (Est: 20min, Actual: -)
  - Font size selector
  - Apply CSS variables
  - Files: `src/components/ProfilePage/FontSizePicker.tsx`

- [ ] **3.6**: Add compact mode toggle (Est: 15min, Actual: -)
  - Checkbox for compact mode
  - Adjust spacing in UI
  - Files: `src/components/ProfilePage/CompactModeToggle.tsx`

- [ ] **3.7**: Build notification preferences (Est: 30min, Actual: -)
  - Email notifications toggle
  - Push notifications toggle
  - Notification types checklist
  - Files: `src/components/ProfilePage/NotificationPreferences.tsx`

- [ ] **3.8**: Add privacy settings (Est: 30min, Actual: -)
  - Profile visibility (public/private)
  - Show email toggle
  - Show activity toggle
  - Files: `src/components/ProfilePage/PrivacySettings.tsx`

- [ ] **3.9**: Implement save/cancel functionality (Est: 20min, Actual: -)
  - Detect changes (dirty state)
  - Save button (TaskEither for API call)
  - Cancel button (revert changes)
  - Confirmation for unsaved changes
  - Files: `src/components/ProfilePage/PreferencesPanel.tsx`

- [ ] **3.10**: Add real-time preview for preferences (Est: 30min, Actual: -)
  - Theme changes apply immediately
  - Font size changes apply immediately
  - Compact mode changes apply immediately
  - Files: `src/hooks/usePreferencesPreview.ts`

- [ ] **3.11**: Write tests for preferences (Est: 1h, Actual: -)
  - Unit tests for preference components
  - Integration tests for API
  - E2E test for save flow
  - Files: `tests/preferences/`

**Phase Totals**:  
Est: 5h | Remaining: 5h

---

## Overall Progress

**Tasks**: 13 completed / 24 total (54%)  
**Time**: Est 13h | Actual 6.7h | Remaining ~6.3h  
**Completion Rate**: Ahead of schedule (7% faster than estimated)  

**Velocity**: 0.97x (slightly faster than estimates)

---

## Upcoming Tasks (Next 3)

1. **Task 2.5**: Complete frontend upload UI (in progress)
   - Priority: High
   - Estimated remaining: 30min
   - Blocker: None

2. **Task 2.6**: Implement image resizing
   - Priority: High
   - Estimated: 45min
   - Blocker: Need architecture decision (Sharp vs Lambda)

3. **Task 2.7**: Create avatar display component
   - Priority: Medium
   - Estimated: 30min
   - Depends on: Task 2.5

---

## Decisions Needed

### Image Resizing Approach (for Task 2.6)
**Options**:

A. **Sharp library** (client-side or API route)
   - Pros: Fast, no cold start, full control
   - Cons: Bundle size increase (~9MB)

B. **AWS Lambda**
   - Pros: No bundle size impact, scalable
   - Cons: Cold start latency, additional AWS cost

**Recommendation**: Sharp in API route (bundle size acceptable for backend)

---

## Update History

### 2025-10-31 14:30 (Cursor Auto-Update)
- ✅ Marked tasks 2.1-2.4 complete
- ⏳ Updated task 2.5 status (in progress)
- 📊 Updated progress: 54% complete
- ⏱️ Added actual times for completed tasks
- 🔄 Added blocker note for task 2.6
- 📝 Added decision needed section

### 2025-10-30 16:00 (Cursor Auto-Update)
- ✅ Marked task 2.2 complete
- 📝 Added note about IAM policy fix
- ✅ Marked task 2.3 complete
- 📊 Updated Phase 2 progress: 43%

### 2025-10-29 18:00 (Cursor Auto-Update)
- ✅ Marked all Phase 1 tasks complete
- 📊 Phase 1 complete (100%)
- ⏱️ Added actual time: 4.2h (slightly over estimate)
- 🎯 Phase 1 velocity: 0.95x (5% slower than estimated)

### 2025-10-29 10:00
- 📝 Created initial TODO list
- 🎯 Set estimates for all 24 tasks
- 📁 Organized into 3 phases

---

## Cursor Update Instructions

**Cursor should**:
1. Mark tasks [x] when complete
2. Add actual time spent
3. Add commit hash and files changed
4. Update progress percentages
5. Add timestamped update to history
6. Update "Upcoming Tasks" section
7. Flag blockers immediately

**Format for completed task**:
```markdown
- [x] **X.Y**: Task description (Est: Xh, Actual: Yh)
  - Implementation notes
  - Files: path/to/files
  - Commit: hash - message
  - Note: Any relevant context
```

---

**This TODO is automatically maintained by Cursor!** ✨

