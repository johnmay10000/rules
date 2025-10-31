# USER_PROFILE_PLAN.md - User Profile Management Feature

**Parent**: [ARCHITECTURE_PLAN.md](../../ARCHITECTURE_PLAN.md)  
**Paired TODO**: [USER_PROFILE_TODO.md](USER_PROFILE_TODO.md) ⭐  
**Status**: ⏳ IN PROGRESS  
**Priority**: P1 (Current Sprint)  
**Last Updated**: 2025-10-31 14:30  
**Estimated Time**: 12 hours (across 3 phases)  

> **📋 Living Document**: This plan evolves as we implement. Cursor updates the paired TODO automatically.

---

## Overview

### What
User profile management system allowing users to:
- View their profile information
- Edit personal details (name, bio, location)
- Upload and manage profile avatars
- Configure user preferences (theme, notifications, privacy)

### Why
**Business Value**:
- Improve user engagement (personalized experience)
- Increase retention (users invested in profiles)
- Enable future social features (user discovery, mentions)

**Technical Value**:
- Foundation for user-generated content
- Testing ground for S3 integration
- Practice for complex state management

### Success Criteria
- [ ] Users can edit all profile fields
- [ ] Avatar upload works with drag-and-drop
- [ ] Changes persist across sessions
- [ ] Page loads in < 2s
- [ ] 80%+ test coverage
- [ ] Zero linter errors

---

## Phases

### Phase 1: Data Model and API Endpoints (4 hours) ✅

**Status**: ✅ COMPLETE  
**Completed**: 2025-10-29  

**Deliverables**:
- [x] Database schema for user profiles
- [x] Prisma migrations
- [x] API endpoint: `GET /api/profile`
- [x] API endpoint: `PUT /api/profile`
- [x] Validation with zod
- [x] Integration tests

**Files Created**:
- `prisma/schema.prisma` - User profile schema
- `src/app/api/profile/route.ts` - GET/PUT handlers
- `src/lib/types/profile.ts` - TypeScript types
- `src/lib/validation/profile.ts` - Zod schemas
- `tests/api/profile.test.ts` - API tests

**Key Decisions**:
- Store preferences as JSONB in Postgres
- Use TaskEither for all database operations
- Validate on both client and server

---

### Phase 2: Avatar Upload System (4 hours) ⏳

**Status**: ⏳ IN PROGRESS (75% complete)  
**Started**: 2025-10-30  
**Expected Completion**: 2025-11-01  

**Tasks**:
- [x] S3 bucket setup and IAM policies
- [x] Presigned URL generation endpoint
- [x] Backend upload handling
- [x] Image validation (size, type)
- [ ] Frontend upload UI (in progress)
- [ ] Image resizing pipeline (not started)
- [ ] Avatar display component (not started)

**Current Focus**:
Working on frontend upload component with drag-and-drop.

**Technical Approach**:
```typescript
// Backend: Generate presigned URL
export const POST = async (req: NextRequest) => {
  return pipe(
    validateUploadRequest(req),
    TE.flatMap(generatePresignedUrl),
    TE.map(url => NextResponse.json({ url }))
  )()
}

// Frontend: Upload to S3
const uploadAvatar = (file: File): TE.TaskEither<UploadError, string> =>
  pipe(
    getPresignedUrl(),
    TE.flatMap(url => uploadToS3(url, file)),
    TE.flatMap(saveAvatarUrlToProfile)
  )
```

**Challenges**:
- S3 CORS configuration issues (resolved)
- IAM policy too restrictive (resolved)
- File size limits (4MB max)

**Next Steps**:
1. Complete upload UI with progress indicator
2. Add image preview before upload
3. Implement image resizing (Lambda or Sharp)

---

### Phase 3: User Preferences System (4 hours) 🔄

**Status**: 🔄 NOT STARTED  
**Planned Start**: 2025-11-01  
**Expected Completion**: 2025-11-04  

**Tasks**:
- [ ] Define preferences schema
- [ ] Create preferences API endpoints
- [ ] Build preferences UI (tabs or sections)
- [ ] Implement theme switcher
- [ ] Add notification preferences
- [ ] Privacy settings
- [ ] Save/cancel functionality
- [ ] Real-time preview

**Preferences to Implement**:

1. **Appearance**
   - Theme (light/dark/auto)
   - Font size
   - Compact mode

2. **Notifications**
   - Email notifications (on/off)
   - Push notifications (on/off)
   - Notification types (comments, mentions, etc.)

3. **Privacy**
   - Profile visibility (public/private)
   - Show email (yes/no)
   - Show activity (yes/no)

**Technical Approach**:
```typescript
// Store preferences as JSONB
interface UserPreferences {
  appearance: {
    theme: 'light' | 'dark' | 'auto'
    fontSize: 'small' | 'medium' | 'large'
    compactMode: boolean
  }
  notifications: {
    email: boolean
    push: boolean
    types: string[]
  }
  privacy: {
    profileVisibility: 'public' | 'private'
    showEmail: boolean
    showActivity: boolean
  }
}
```

---

## Implementation Details

### Database Schema

```prisma
model User {
  id        String   @id @default(uuid())
  email     String   @unique
  name      String
  bio       String?
  location  String?
  avatarUrl String?
  preferences Json?  // JSONB for flexibility
  createdAt DateTime @default(now())
  updatedAt DateTime @updatedAt
}
```

### API Endpoints

**GET /api/profile**
- Returns current user's profile
- Uses TaskEither for error handling
- Returns 404 if not found

**PUT /api/profile**
- Updates profile fields
- Validates with zod
- Returns updated profile

**POST /api/profile/avatar/upload**
- Generates presigned S3 URL
- Returns URL for client upload

**PUT /api/profile/preferences**
- Updates user preferences
- Validates preference structure

### Frontend Components

```
components/
├── ProfilePage/
│   ├── ProfileHeader.tsx        # Avatar + basic info
│   ├── ProfileEditForm.tsx      # Edit form
│   ├── AvatarUpload.tsx         # Upload component
│   └── PreferencesPanel.tsx     # Preferences UI
```

### State Management

Using React Context + TaskEither:

```typescript
interface ProfileState {
  profile: RemoteData<Profile, ProfileError>
  preferences: RemoteData<Preferences, ProfileError>
}

const ProfileContext = createContext<ProfileState>(...)
```

---

## Testing Strategy

### Unit Tests
- Validation functions (zod schemas)
- Pure business logic
- Component rendering

**Target**: 3+ tests per function

### Integration Tests
- API endpoints (all CRUD operations)
- Database operations
- S3 upload flow

**Target**: All happy paths + error cases

### E2E Tests (Playwright)
- Complete profile edit flow
- Avatar upload end-to-end
- Preferences persistence

**Critical Flows**:
1. User logs in → edits profile → saves → reloads → sees changes
2. User uploads avatar → sees preview → confirms → sees new avatar
3. User changes theme → sees immediate UI change → persists

---

## Dependencies

### External
- AWS S3 (avatar storage)
- Prisma (database ORM)
- Zod (validation)
- fp-ts (functional patterns)

### Internal
- Authentication system (complete)
- User session management (complete)

### Blockers
- None currently

---

## Risks and Mitigations

| Risk | Likelihood | Impact | Mitigation |
|------|------------|--------|------------|
| S3 cost overrun | Medium | Medium | Implement image resizing, set size limits |
| Avatar upload fails | Medium | High | Presigned URLs, retry logic, error messages |
| CORS issues | Low | Medium | Proper S3 bucket policy |
| Preference schema changes | High | Low | Use JSONB for flexibility |

---

## Performance Considerations

### Avatar Upload
- Max file size: 4MB
- Resize to 512x512 (profile), 128x128 (thumbnail)
- Use WebP format for efficiency
- Lazy load images

### Database
- Index on userId for profile queries
- Cache preferences in Redis (optional)

### Frontend
- Optimistic UI updates
- Debounce form inputs
- Show progress indicators

---

## Related Documents

### This Feature (Tier 2)
- **Plan**: USER_PROFILE_PLAN.md (this file)
- **TODO**: [USER_PROFILE_TODO.md](USER_PROFILE_TODO.md) ⭐

### Parent (Tier 1)
- [ARCHITECTURE_PLAN.md](../../ARCHITECTURE_PLAN.md)

### Daily Work Docs (Tier 3)
- [2025-10-31 Avatar Upload Frontend](../../2025_10_31/20251031_0900_AVATAR_UPLOAD_FRONTEND.md)
- [2025-10-30 S3 IAM Configuration](../../2025_10_30/20251030_1500_S3_IAM_CONFIG.md)
- [2025-10-29 Profile API Implementation](../../2025_10_29/20251029_1000_PROFILE_API.md)

---

## Update History

### 2025-10-31 14:30
- Updated Phase 2 progress to 75%
- Backend upload complete, starting frontend
- Added technical approach examples
- Updated success criteria

### 2025-10-30 16:00
- Completed S3 bucket setup
- Resolved IAM policy issues
- Started frontend upload component
- Phase 1 marked complete

### 2025-10-29 10:00
- Completed all Phase 1 tasks
- All tests passing
- Ready for Phase 2

### 2025-10-28 14:00
- Initial plan created
- Database schema designed
- API endpoints planned

---

## Team Notes

**Owner**: Alice (Backend) + Bob (Frontend)  
**Reviewers**: Charlie (Security), Diana (UX)  
**Estimated Completion**: 2025-11-04  

**Daily Standup Topics**:
- Phase 2 progress (avatar upload)
- Image resizing approach decision
- Phase 3 planning

---

**Remember**: This is a living plan. Update as implementation evolves. Cursor automatically updates the paired TODO list!

