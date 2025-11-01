# MANDATORY_NAMING_CONVENTION.md - Sequential Numbering Update

**Date**: 2025-10-31
**Status**: ✅ COMPLETE  
**Impact**: All future Tier 3 documents  

---

## 🎯 Change Summary

Updated mandatory naming convention for daily work documents (Tier 3) from timestamp-based to sequence-based.

---

## 📝 Change Details

### Old Format (Deprecated)

```
❌ YYYYMMDD_HHMM_NAME.md
❌ 20251031_0900_SETUP.md
❌ 20251031_1430_IMPLEMENTATION.md
❌ 20251031_1700_SUMMARY.md
```

**Problems**:
- Time collisions (multiple docs same hour)
- Timezone confusion
- Harder to reference ("the 14:30 doc")
- No clear ordering within same time

### New Format (MANDATORY)

```
✅ YYYYMMDD_NNNN_NAME.md
✅ 20251031_0000_SETUP.md
✅ 20251031_0001_IMPLEMENTATION.md
✅ 20251031_0002_SUMMARY.md
```

**Benefits**:
- ✅ Clear sequential ordering
- ✅ No collisions (unlimited docs per day)
- ✅ Easy to reference ("doc 0002")
- ✅ Simple automation (just increment)
- ✅ Better sorting (alphabetical = chronological)
- ✅ No timezone issues

---

## 📋 Sequence Rules

1. **Start each day at 0000**
   - First doc: `20251031_0000_*.md`

2. **Increment for each new doc**
   - Second doc: `20251031_0001_*.md`
   - Third doc: `20251031_0002_*.md`
   - etc.

3. **Use 4-digit zero-padding**
   - ✅ 0000, 0001, 0002, ..., 0099, 0100, ...
   - ❌ 0, 1, 2, ... (not zero-padded)

4. **No gaps in sequence**
   - Use next available number
   - Don't skip numbers

5. **Reset each day**
   - Each day starts at 0000
   - Independent from previous days

---

## 📚 Updated Documentation

### Files Modified

1. **cursor/CURSOR.md**
   - Section 2: Documentation Structure
   - Updated Tier 3 naming convention
   - Added mandatory examples

2. **cursor/CURSOR_WORKFLOW_GUIDE.md**
   - Daily workflow section
   - Updated examples
   - Added sequence number instructions

3. **cursor/FILE_LOCATIONS_USER_GUIDE.md**
   - Tier 3 section (needs manual review)

4. **.cursorrules** (this repo)
   - Daily Work Folders section
   - Updated format and examples

5. **cursor/examples/plan_with_todo/**
   - ARCHITECTURE_PLAN.md
   - USER_PROFILE_PLAN.md
   - Updated cross-references

6. **README.md**
   - Mandatory rules section
   - Updated description

### Files Created

1. **cursor/NAMING_CONVENTION.md** ⭐ NEW
   - Complete guide to new format
   - Migration instructions
   - Examples and anti-patterns
   - Automation helpers
   - Quick reference

---

## ✅ This Repository Compliance

**Status**: ✅ ALREADY COMPLIANT!

This repository's existing documents already follow the new format:

```
docs/2025_10_30/
├── 20251030_0000_GLOBAL_RULESET_IMPLEMENTATION_PLAN.md
├── 20251030_0001_TODO_LIST.md
├── 20251030_0002_ANALYSIS_SUMMARY.md
├── 20251030_0003_DOCUMENTATION_HIERARCHY_ADDITION.md
└── ... (all following NNNN format)

docs/2025_10_31/
├── 20251031_0000_MANDATORY_GIT_CHECKPOINTS.md
├── 20251031_0001_FILE_LOCATIONS_USER_GUIDE_PORTABLE.md
├── 20251031_0002_PHASE_0_COMPLETE_SUMMARY.md
└── ... (all following NNNN format)
```

**No migration needed for this repo!** ✅

---

## 🔧 Implementation for New Projects

### Creating New Document

```bash
# Find last sequence number
LAST=$(ls docs/$(date +%Y_%m_%d)/ 2>/dev/null | tail -1 | cut -d'_' -f2)

# Calculate next
if [ -z "$LAST" ]; then
  NEXT="0000"
else
  NEXT=$(printf "%04d" $((10#$LAST + 1)))
fi

# Create file
touch docs/$(date +%Y_%m_%d)/$(date +%Y%m%d)_${NEXT}_YOUR_NAME.md
```

### Starting New Day

```bash
# Create folder
mkdir -p docs/$(date +%Y_%m_%d)

# First doc of the day
touch docs/$(date +%Y_%m_%d)/$(date +%Y%m%d)_0000_DAILY_KICKOFF.md
```

---

## 📖 For Teams

### Onboarding New Members

1. **Read**: `cursor/NAMING_CONVENTION.md`
2. **See examples**: This repo's `docs/` folder
3. **Practice**: Create a test document
4. **Use**: In your daily work

### Common Questions

**Q: What if I delete a document?**  
A: Don't reuse that sequence number. Just skip it.

**Q: What's the maximum sequence?**  
A: 9999 (more than enough for any day)

**Q: Can I use timestamps in the content?**  
A: Yes! Content can have timestamps. Only the filename uses sequences.

**Q: What if two people create docs simultaneously?**  
A: Use the next available number. Git will catch conflicts.

---

## 🎯 Enforcement

### Mandatory Status

This naming convention is **MANDATORY** for:
- ✅ All Cursor projects
- ✅ All Tier 3 documents
- ✅ Daily work folders (docs/YYYY_MM_DD/)

### Cursor Automation

Cursor AI should:
- ✅ Auto-detect next sequence number
- ✅ Create files with correct naming
- ✅ Update cross-references automatically
- ✅ Enforce format in commits

---

## 📊 Impact

### Affected Documents

- **Tier 3 only**: Daily work documents
- **Not affected**: 
  - Tier 1 (ARCHITECTURE_PLAN.md)
  - Tier 2 (docs/plans/*.md)
  - README files
  - Code files

### Breaking Change

⚠️ Projects using old timestamp format must migrate:
1. Rename existing files to sequence format
2. Update cross-references
3. Update ARCHITECTURE_PLAN.md links
4. Commit changes

---

## ✨ Benefits Recap

1. **Clarity**: Sequence is clearer than timestamp
2. **Simplicity**: Easy to understand and automate
3. **Consistency**: Same across all timezones
4. **Scalability**: Unlimited docs per day
5. **Sortability**: Alphabetical = chronological
6. **Reference-ability**: Easy to cite ("doc 0042")

---

## 🔗 See Also

- **Full Guide**: `cursor/NAMING_CONVENTION.md`
- **Workflow**: `cursor/CURSOR_WORKFLOW_GUIDE.md`
- **Main Rules**: `cursor/CURSOR.md` Section 2
- **Examples**: This repo's `docs/` folders

---

## ✅ Status

**Change**: ✅ COMPLETE  
**Documentation**: ✅ UPDATED  
**This Repo**: ✅ COMPLIANT  
**Enforcement**: ✅ MANDATORY  

**From now on, all Tier 3 documents MUST use sequential NNNN format!**

---

**Version**: 1.1.0  
**Last Updated**: 2025-10-31  
**Status**: MANDATORY ✅
