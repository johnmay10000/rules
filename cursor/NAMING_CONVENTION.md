# NAMING_CONVENTION.md - Daily Work Document Naming

**Version**: 1.1.0  
**Last Updated**: 2025-10-31  
**Status**: MANDATORY  

---

## 📝 Tier 3 Document Naming (MANDATORY)

### Format

```
docs/YYYY_MM_DD/YYYYMMDD_NNNN_DESCRIPTIVE_NAME.md
```

### Components

1. **Folder**: `YYYY_MM_DD` (with underscores)
   - Example: `2025_10_31`

2. **Date Prefix**: `YYYYMMDD` (no underscores)
   - Example: `20251031`

3. **Sequence Number**: `NNNN` (4 digits, zero-padded)
   - Start at: `0000`
   - Increment: `0001`, `0002`, `0003`, ...
   - Reset each day

4. **Name**: `DESCRIPTIVE_NAME` (UPPER_SNAKE_CASE)
   - Brief, descriptive
   - Example: `PHASE_1_COMPLETE`, `SESSION_SUMMARY`

---

## ✅ Correct Examples

```
docs/2025_10_31/20251031_0000_SETUP_GUIDE.md
docs/2025_10_31/20251031_0001_PHASE_1_KICKOFF.md
docs/2025_10_31/20251031_0002_PHASE_1_COMPLETE.md
docs/2025_10_31/20251031_0003_SESSION_SUMMARY.md

docs/2025_11_01/20251101_0000_PHASE_2_KICKOFF.md
docs/2025_11_01/20251101_0001_BUG_INVESTIGATION.md
```

---

## ❌ Incorrect Examples

```
❌ docs/2025_10_31/20251031_0900_setup.md
   (Timestamp instead of sequence, lowercase name)

❌ docs/2025_10_31/20251031_14:30_SETUP.md
   (Timestamp with colon, invalid filename)

❌ docs/2025_10_31/setup-guide.md
   (No date prefix, kebab-case)

❌ docs/2025_10_31/20251031_1_SETUP.md
   (Sequence not zero-padded)

❌ docs/2025_10_31/20251031_SETUP_GUIDE.md
   (Missing sequence number)
```

---

## 🎯 Why Sequential Numbers?

### Problems with Timestamps

❌ **Time Collisions**:
- Multiple docs in same hour/minute
- Hard to determine order within same time

❌ **Timezone Confusion**:
- Team in different timezones
- Ambiguous creation time

❌ **Harder to Reference**:
- "The 14:30 document" vs "Document 0003"
- No clear ordering

### Benefits of Sequential Numbers

✅ **Clear Ordering**:
- 0000 comes before 0001
- No ambiguity

✅ **No Collisions**:
- Can create unlimited docs per day
- Just increment sequence

✅ **Easy to Reference**:
- "Document 0003" is clearer
- Easy to find "next" document

✅ **Simple Automation**:
- Just find last sequence and +1
- No timezone logic needed

✅ **Better Sorting**:
- Alphabetical sort = chronological sort
- Works in any file browser

---

## 🔧 How to Use

### Creating New Documents

1. **Check last document** in today's folder:
   ```bash
   ls docs/$(date +%Y_%m_%d)/ | tail -1
   # Output: 20251031_0002_SESSION_SUMMARY.md
   ```

2. **Increment sequence**:
   - Last was `0002`
   - Next is `0003`

3. **Create file**:
   ```bash
   touch docs/$(date +%Y_%m_%d)/$(date +%Y%m%d)_0003_NEW_FEATURE.md
   ```

### Starting a New Day

1. **Create folder**:
   ```bash
   mkdir -p docs/$(date +%Y_%m_%d)
   ```

2. **Start at 0000**:
   ```bash
   touch docs/$(date +%Y_%m_%d)/$(date +%Y%m%d)_0000_DAILY_KICKOFF.md
   ```

---

## 📋 Common Sequences

### Typical Day

```
20251031_0000_DAILY_KICKOFF.md         # Start of day
20251031_0001_FEATURE_A_DESIGN.md      # Design work
20251031_0002_FEATURE_A_IMPL.md        # Implementation
20251031_0003_BUG_FIX.md               # Bug fix
20251031_0004_CODE_REVIEW_NOTES.md     # Review notes
20251031_0005_SESSION_SUMMARY.md       # End of day
```

### Phase Completion

```
20251031_0000_PHASE_1_KICKOFF.md
20251031_0001_PHASE_1_TASK_1.md
20251031_0002_PHASE_1_TASK_2.md
20251031_0003_PHASE_1_COMPLETE.md
20251031_0004_PHASE_2_KICKOFF.md
```

---

## 🤖 Cursor Automation

Cursor should:

1. **Auto-detect** next sequence number
2. **Create** file with correct naming
3. **Reference** in plans using sequence numbers
4. **Update** cross-references automatically

### Example in TODO List

```markdown
### 2025-10-31 14:30 (Cursor Auto-Update)
- ✅ Completed: Tasks 2.1-2.4
- 📝 Created: docs/2025_10_31/20251031_0003_TASK_SUMMARY.md
- 📊 Updated progress: 54% complete
```

---

## 📚 Cross-Referencing

### In Plans

```markdown
### Daily Work Docs (Tier 3)
- [Phase 1 Kickoff](../../2025_10_31/20251031_0000_PHASE_1_KICKOFF.md)
- [Phase 1 Complete](../../2025_10_31/20251031_0003_PHASE_1_COMPLETE.md)
- [Session Summary](../../2025_10_31/20251031_0005_SESSION_SUMMARY.md)
```

### In Architecture Plan

```markdown
## Related Documents

### Recent Work (Tier 3)
- [2025-10-31 Setup](docs/2025_10_31/20251031_0000_SETUP.md)
- [2025-10-31 Phase 1](docs/2025_10_31/20251031_0001_PHASE_1.md)
- [2025-10-31 Summary](docs/2025_10_31/20251031_0002_SUMMARY.md)
```

---

## ⚠️ Migration from Old Format

### Old Format (Deprecated)

```
❌ docs/2025_10_31/20251031_0900_SETUP.md
❌ docs/2025_10_31/20251031_1430_PHASE_1.md
❌ docs/2025_10_31/20251031_1700_SUMMARY.md
```

### New Format (Mandatory)

```
✅ docs/2025_10_31/20251031_0000_SETUP.md
✅ docs/2025_10_31/20251031_0001_PHASE_1.md
✅ docs/2025_10_31/20251031_0002_SUMMARY.md
```

### Migration Steps

1. **Rename existing files** to use sequence numbers
2. **Update cross-references** in plans
3. **Update ARCHITECTURE_PLAN.md** links
4. **Commit** with clear message

---

## 🔍 Quick Reference

### Creating Document

```bash
# Find next sequence
LAST=$(ls docs/$(date +%Y_%m_%d)/ 2>/dev/null | tail -1 | cut -d'_' -f2)
NEXT=$(printf "%04d" $((10#$LAST + 1)))

# Create file
touch docs/$(date +%Y_%m_%d)/$(date +%Y%m%d)_${NEXT}_YOUR_NAME.md
```

### Listing Documents

```bash
# List today's documents in order
ls docs/$(date +%Y_%m_%d)/

# Output:
# 20251031_0000_SETUP.md
# 20251031_0001_PHASE_1.md
# 20251031_0002_SUMMARY.md
```

---

## 📖 Summary

### Mandatory Format

```
YYYYMMDD_NNNN_DESCRIPTIVE_NAME.md
```

### Key Rules

1. ✅ Use 4-digit zero-padded sequence (0000-9999)
2. ✅ Start each day at 0000
3. ✅ Increment for each new document
4. ✅ Use UPPER_SNAKE_CASE for names
5. ✅ No timestamps, no time-based naming
6. ✅ Keep sequential order

### Why It's Better

- Clear ordering
- No collisions
- Easy to reference
- Simple automation
- Better sorting

---

**This naming convention is MANDATORY for all Tier 3 documents!**

**Version**: 1.1.0  
**Last Updated**: 2025-10-31  
**Status**: MANDATORY ✅

