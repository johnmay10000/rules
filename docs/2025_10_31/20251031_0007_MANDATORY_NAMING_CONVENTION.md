# Mandatory Documentation Naming Convention

**Date**: 2025-10-31
**Sequence**: 0007
**Priority**: 🔥 MANDATORY
**User Requirement**: "adjust the naming convention to match your existing format with sequential numbers instead of timestamps and have it mandatory"

---

## 📋 Summary

Updated all planning documents to enforce **sequential number format** for daily work documentation filenames instead of timestamp format. This matches the existing convention used throughout the repository.

---

## 🎯 Naming Convention (MANDATORY)

### Format

```
YYYYMMDD_NNNN_DESCRIPTIVE_NAME.md
```

**Components**:
1. **YYYYMMDD**: Date with NO separators (e.g., `20251031`)
2. **NNNN**: 4-digit sequential number starting at 0000 (e.g., `0000`, `0001`, `0002`, ... `0010`, `0011`, ...)
3. **DESCRIPTIVE_NAME**: ALL_CAPS with underscores (e.g., `PHASE_1_COMPLETE`)

### Examples

**✅ CORRECT**:
```
20251031_0000_MANDATORY_GIT_CHECKPOINTS.md
20251031_0001_FILE_LOCATIONS_USER_GUIDE_PORTABLE.md
20251031_0002_PHASE_0_COMPLETE_SUMMARY.md
20251031_0003_RULE_TAXONOMY.md
20251031_0004_MANDATORY_RULES_EXTRACTION.md
20251031_0005_PHASE_1_COMPLETE_SUMMARY.md
20251031_0006_CLAUDE_PLANNING_COMPLETE.md
20251031_0007_MANDATORY_NAMING_CONVENTION.md
```

**❌ INCORRECT**:
```
20251031_0930_SUMMARY.md              # Using timestamp instead of sequential number
2025_10_31_0000_SUMMARY.md            # Has date separators
20251031_SUMMARY.md                   # Missing sequential number
SUMMARY.md                            # Missing date and number
20251031_07_SUMMARY.md                # Only 2 digits instead of 4
```

---

## 📂 Location Rules

### Daily Work Documents

**Location**: `docs/YYYY_MM_DD/` (dated folders)

**Examples**:
- `docs/2025_10_30/` - October 30, 2025 work
- `docs/2025_10_31/` - October 31, 2025 work (today)
- `docs/2025_11_01/` - November 1, 2025 work

### Sequential Numbering Within Each Day

**Each day starts fresh at 0000**:
- `docs/2025_10_31/20251031_0000_*.md` (first doc of the day)
- `docs/2025_10_31/20251031_0001_*.md` (second doc of the day)
- `docs/2025_10_31/20251031_0002_*.md` (third doc of the day)
- ... continues through 9999 if needed

**Next day resets**:
- `docs/2025_11_01/20251101_0000_*.md` (first doc of next day)
- `docs/2025_11_01/20251101_0001_*.md` (second doc of next day)

---

## 🔍 Rationale

### Why Sequential Numbers Instead of Timestamps?

1. **Follows Existing Convention**
   - All docs in `docs/2025_10_30/` use sequential numbers (0000-0014)
   - All docs in `docs/2025_10_31/` use sequential numbers (0000-0007)
   - Consistency with established pattern

2. **Chronological Sorting**
   - Files automatically sort in creation order
   - No ambiguity about document sequence
   - Easy to see "this was the 5th document created today"

3. **Clear Document Order**
   - 0000 = first document of the day
   - 0001 = second document of the day
   - 0002 = third document of the day
   - Sequence is explicit and unambiguous

4. **No Time-of-Day Confusion**
   - Timestamps (0930, 1430) can be ambiguous across timezones
   - Sequential numbers are timezone-independent
   - No confusion about "when" vs "what order"

5. **Easy References**
   - Can reference as "doc 6 from Oct 31" instead of "doc from 09:30 on Oct 31"
   - Simpler mental model

6. **4-Digit Format**
   - Supports up to 10,000 documents per day (0000-9999)
   - Consistent width for sorting and display
   - Professional appearance

---

## 📝 What Changed

### Files Renamed

**Before** (incorrect timestamp format):
```
docs/2025_10_31/20251031_0930_CLAUDE_PLANNING_COMPLETE.md
```

**After** (correct sequential format):
```
docs/2025_10_31/20251031_0006_CLAUDE_PLANNING_COMPLETE.md
```

### Documents Updated

1. **CLAUDE_IMPLEMENTATION_PLAN.md**
   - Updated all filename examples
   - Changed `YYYYMMDD_HHMM_` → `YYYYMMDD_NNNN_`
   - Added mandatory naming convention section
   - Updated all phase deliverables

2. **CLAUDE_IMPLEMENTATION_TODO.md**
   - Updated all deliverable filename examples
   - Changed `20251031_XXXX_` → `20251031_NNNN_`
   - Clarified sequential number requirement

3. **This Document**
   - Created to explain the mandatory convention
   - Provides clear examples and rationale

---

## ⚠️ Enforcement

### This Rule is MANDATORY

- **ALL daily work documents** MUST follow this format
- **NO exceptions** allowed
- **NO timestamp format** (0930, 1430, etc.)
- **NO date separators** (2025_10_31)
- **MUST use 4-digit sequential numbers** (0000-9999)

### How to Determine Next Number

1. List files in current day's folder: `ls docs/2025_10_31/`
2. Find highest number (e.g., `20251031_0006_...`)
3. Increment by 1 (e.g., next is `0007`)
4. Use that number for new document

**Command to find next number**:
```bash
# List files, extract numbers, find max, add 1
ls docs/2025_10_31/ | grep -o '_[0-9]\{4\}_' | sort -r | head -1
# Then increment manually
```

---

## 📚 Where This Applies

### Tier 3: Execution Documents (MANDATORY)

**Location**: `docs/YYYY_MM_DD/`
**Purpose**: Daily work logs, decisions, summaries, phase completions
**Naming**: `YYYYMMDD_NNNN_DESCRIPTIVE_NAME.md`

### Tier 1 & 2: Strategic/Tactical Documents (NO NUMBERS)

**Tier 1** (Strategic):
- Location: Project root
- Format: `ARCHITECTURE_PLAN.md` (NO date, NO number)

**Tier 2** (Tactical):
- Location: `docs/plans/`
- Format: `FEATURE_NAME_PLAN.md`, `FEATURE_NAME_TODO.md` (NO date, NO number)

---

## ✅ Verification Checklist

Before committing any daily work document:

- [ ] Filename starts with `YYYYMMDD_` (8 digits, no separators)
- [ ] Next is `NNNN_` (4 digits, sequential number)
- [ ] Descriptive name is `ALL_CAPS_WITH_UNDERSCORES`
- [ ] File is in correct dated folder (`docs/YYYY_MM_DD/`)
- [ ] Number is next in sequence (checked with `ls`)
- [ ] File extension is `.md`

**Example verification**:
```
✅ docs/2025_10_31/20251031_0007_MANDATORY_NAMING_CONVENTION.md

Breaking it down:
✅ docs/2025_10_31/          - Correct dated folder
✅ 20251031                  - Date with no separators
✅ 0007                      - 4-digit sequential number (follows 0006)
✅ MANDATORY_NAMING_CONVENTION - ALL_CAPS descriptive name
✅ .md                       - Correct extension
```

---

## 🔄 Git History

### Files Affected

1. Renamed file:
   - `docs/2025_10_31/20251031_0930_CLAUDE_PLANNING_COMPLETE.md` → `20251031_0006_CLAUDE_PLANNING_COMPLETE.md`

2. Updated documents:
   - `docs/plans/CLAUDE_IMPLEMENTATION_PLAN.md` (updated all filename examples)
   - `docs/plans/CLAUDE_IMPLEMENTATION_TODO.md` (updated all deliverable examples)

3. New document:
   - `docs/2025_10_31/20251031_0007_MANDATORY_NAMING_CONVENTION.md` (this file)

### Next Steps

1. Update `cursor/CURSOR.md` documentation section (Tier 3 filename format)
2. Update `.cursorrules` with naming convention requirement
3. Git checkpoint: "Enforce mandatory sequential numbering for daily docs"

---

## 📖 References

### Existing Files Following This Convention

**October 30, 2025** (`docs/2025_10_30/`):
- 0000 through 0014 (15 documents)

**October 31, 2025** (`docs/2025_10_31/`):
- 0000: MANDATORY_GIT_CHECKPOINTS
- 0001: FILE_LOCATIONS_USER_GUIDE_PORTABLE
- 0002: PHASE_0_COMPLETE_SUMMARY
- 0003: RULE_TAXONOMY
- 0004: MANDATORY_RULES_EXTRACTION
- 0005: PHASE_1_COMPLETE_SUMMARY
- 0006: CLAUDE_PLANNING_COMPLETE
- 0007: MANDATORY_NAMING_CONVENTION (this document)

---

## 🎯 Impact

### Positive Changes

1. ✅ **Consistency**: All daily work docs follow same pattern
2. ✅ **Clarity**: Explicit creation order within each day
3. ✅ **Simplicity**: Easy to determine next number
4. ✅ **Professional**: Consistent 4-digit format
5. ✅ **Maintainable**: Clear rules, easy to enforce

### What Users Must Do

- Check existing files in day's folder before creating new document
- Use next sequential number (increment highest by 1)
- Never use timestamps (0930, 1430, etc.)
- Always use 4-digit format (0000, not 0, 00, or 000)

---

## ✅ Status

**Mandatory Naming Convention**: ✅ ENFORCED
**Planning Documents**: ✅ UPDATED
**Example Files**: ✅ RENAMED
**Documentation**: ✅ COMPLETE

**Next**: Update cursor/CURSOR.md and commit changes

---

**End of Document**
