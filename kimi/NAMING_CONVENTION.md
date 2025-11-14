# NAMING_CONVENTION.md - Daily Work Document Naming for Kimi

**Version**: 1.1.0  
**Last Updated**: 2025-11-14  
**Status**: MANDATORY  
**Applies To**: All Kimi CLI documentation

---

## 📝 Tier 3 Document Naming (MANDATORY)

### Format

```
docs/YYYY_MM_DD/YYYYMMDD_NNNN_DESCRIPTIVE_NAME.md
```

### Components

1. **Folder**: `YYYY_MM_DD` (with underscores)
   - Example: `2025_11_14`

2. **Date Prefix**: `YYYYMMDD` (no underscores)
   - Example: `20251114`

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
docs/2025_11_14/20251114_0000_SETUP_GUIDE.md
docs/2025_11_14/20251114_0001_PHASE_1_KICKOFF.md
docs/2025_11_14/20251114_0002_PHASE_1_COMPLETE.md
docs/2025_11_14/20251114_0003_SESSION_SUMMARY.md

docs/2025_10_31/20251031_0000_KIMI_GUIDELINES_IMPLEMENTATION.md
docs/2025_10_31/20251031_0001_PHASE_1_KICKOFF.md
docs/2025_10_31/20251031_0002_SESSION_SUMMARY.md
```

---

## ❌ Incorrect Examples

```
❌ docs/2025_11_14/20251114_0900_setup.md
   (Timestamp instead of sequence, lowercase name)

❌ docs/2025_11_14/20251114_14:30_SETUP.md
   (Timestamp with colon, invalid filename)

❌ docs/2025_11_14/setup-guide.md
   (No date prefix, kebab-case)

❌ docs/2025_11_14/20251114_1_SETUP.md
   (Sequence not zero-padded)

❌ docs/2025_11_14/20251114_SETUP_GUIDE.md
   (Missing sequence number)

❌ docs/2025_11_14/20251114_01_SETUP.md
   (Wrong sequence length, should be 0001)
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

❌ **Brittle File Names**:
- Special characters in filenames (colons)
- Some OS/filesystems don't support

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

✅ **More Robust**:
- No special characters
- Works on all filesystems

✅ **Kimi-Friendly**:
- Predictable pattern
- Easy to generate programmatically
- Works with parallel file operations

---

## 📋 How to Determine Next Number

### Manual Method

```bash
# 1. List files in today's folder
ls docs/2025_11_14/

# Output:
# 20251114_0000_SETUP.md
# 20251114_0001_PHASE_1.md
# 20251114_0002_SESSION_SUMMARY.md

# 2. Find highest number (0002)
# 3. Add 1 → That's your next number: 0003

# Example: Create next file
date=$(date +%Y%m%d)
next_num="0003"
cat > docs/2025_11_14/${date}_${next_num}_YOUR_TOPIC.md << 'EOF'
# Your Document Title

**Date**: 2025-11-14
**Status**: In Progress

## Content

EOF
```

### Automatic Method (Using Shell)

```bash
#!/bin/bash
# Create next numbered file automatically

TODAY=$(date +%Y%m%d)
TODAY_FOLDER=$(date +%Y_%m_%d)
FOLDER="docs/${TODAY_FOLDER}"

# Create folder if doesn't exist
mkdir -p "$FOLDER"

# Find next sequence number
if [ -d "$FOLDER" ]; then
    LAST=$(ls "$FOLDER" | grep "${TODAY}_[0-9]" | sort | tail -1)
    if [ -n "$LAST" ]; then
        # Extract number and increment
        NUM=$(echo "$LAST" | sed 's/.*_\([0-9]\{4\}\)_.*/\1/')
        NEXT=$(printf "%04d" $((10#$NUM + 1)))
    else
        NEXT="0000"
    fi
else
    NEXT="0000"
fi

# Create file
echo "Creating: ${FOLDER}/${TODAY}_${NEXT}_YOUR_TOPIC.md"
echo "Next number: $NEXT"
```

### Kimi CLI Integration

Kimi can help generate the next file name:

```bash
# Ask Kimi to determine next number
# (Kimi will read directory and calculate)

kimi next-doc-number docs/2025_11_14/
# Returns: 0003
```

---

## 📁 Directory Structure

### Correct Structure

```
project/
├── docs/
│   ├── 2025_11_14/
│   │   ├── 20251114_0000_IMPLEMENTATION.md
│   │   ├── 20251114_0001_SESSION_SUMMARY.md
│   │   └── 20251114_0002_BUG_FIX.md
│   │
│   ├── 2025_11_15/
│   │   ├── 20251115_0000_FEATURE_COMPLETE.md
│   │   └── 20251115_0001_CODE_REVIEW.md
│   │
│   ├── plans/
│   │   ├── FEATURE_PLAN.md
│   │   └── FEATURE_TODO.md
│   │
│   └── ARCHITECTURE_PLAN.md
│
└── src/
    └── ...
```

---

## 🏷️ Name Guidelines

### Good Names

✅ **Descriptive**:
- `20251114_0000_KIMI_IMPLEMENTATION_START.md`
- `20251114_0001_PHASE_0_COMPLETE.md`
- `20251114_0002_SESSION_SUMMARY.md`
- `20251114_0003_BUG_FIX_PAYMENT_PROCESSING.md`

✅ **Concise**:
- `20251114_0004_REVIEW_COMMENTS.md`
- `20251114_0005_DEPLOYMENT_CHECKLIST.md`

✅ **Action-Oriented**:
- `20251114_0006_IMPLEMENT_USER_AUTH.md`
- `20251114_0007_TEST_API_ENDPOINTS.md`

### Bad Names

❌ **Too Vague**:
- `20251114_0000_WORK.md`
- `20251114_0001_STUFF.md`

❌ **Too Long**:
- `20251114_0000_IMPLEMENTATION_OF_COMPREHENSIVE_USER_AUTHENTICATION_SYSTEM.md`

❌ **Unclear**:
- `20251114_0000_ABC123.md`
- `20251114_0001_XYZ789.md`

❌ **Wrong Format**:
- `20251114_0000_my-file.md` (kebab-case)
- `20251114_0000_myFile.md` (camelCase)

---

## 🔧 Kimi CLI Integration

### Setting Up for New Day

```bash
# Automated daily folder setup
setup_daily_work() {
    local today=$(date +%Y_%m_%d)
    local folder="docs/${today}"
    
    mkdir -p "$folder"
    
    echo "Created: $folder"
    echo "Next number: 0000"
}

setup_daily_work
```

### Creating Next Document

```bash
# Function to create next document with auto-numbering
next_doc() {
    local topic=$1
    local today=$(date +%Y%m%d)
    local folder="docs/$(date +%Y_%m_%d)"
    
    mkdir -p "$folder"
    
    # Find next number
    local last=$(ls "$folder" 2>/dev/null | grep "${today}_[0-9]" | sort | tail -1)
    local next="0000"
    
    if [ -n "$last" ]; then
        local num=$(echo "$last" | sed 's/.*_\([0-9]\{4\}\)_.*/\1/')
        next=$(printf "%04d" $((10#$num + 1)))
    fi
    
    local filename="${folder}/${today}_${next}_${topic}.md"
    
    # Create file with template
    cat > "$filename" << EOF
# $topic

**Date**: $(date +%Y-%m-%d)  
**Document**: ${today}_${next}  
**Status**: In Progress

## Objective



## Progress



## Next Steps



---

**Created**: $(date)
EOF

    echo "Created: $filename"
    echo "Document number: $next"
    echo "Topic: $topic"
}

# Usage
next_doc "PHASE_1_IMPLEMENTATION"
# Creates: docs/2025_11_14/20251114_0005_PHASE_1_IMPLEMENTATION.md
```

### Kimi Parallel File Creation

Kimi can create multiple sequential documents efficiently:

```bash
# Kimi can determine next numbers and create batch
# Using parallel tool calls for efficiency

# Example: Create 3 related documents
# Kimi internally:
# - Reads docs/2025_11_14/ to find current sequence
# - Creates files 0003, 0004, 0005
# - Generates appropriate content for each
```

---

## Common Mistakes

### ❌ Using Timestamps Instead of Sequence

```bash
# WRONG
docs/2025_11_14/20251114_1430_setup.md

# RIGHT
docs/2025_11_14/20251114_0000_SETUP.md
```

### ❌ Wrong Case in Description

```bash
# WRONG
docs/2025_11_14/20251114_0000_my-setup.md

# RIGHT
docs/2025_11_14/20251114_0000_MY_SETUP.md
```

### ❌ Missing Sequence Numbers

```bash
# WRONG
docs/2025_11_14/20251114_setup.md

# RIGHT
docs/2025_11_14/20251114_0000_SETUP.md
```

### ❌ Using Special Characters

```bash
# WRONG
docs/2025_11_14/20251114_0000_setup:final.md

# RIGHT
docs/2025_11_14/20251114_0000_SETUP_FINAL.md
```

---

## Cross-Reference with Other Rules

- **KIMI.md sections 2-3**: Git checkpoints and documentation structure
- **KIMI.md section 4**: File size limits (250-300 lines per doc)
- **KIMI_WORKFLOW_GUIDE.md**: Workflow templates for creating docs
- **FILE_LOCATIONS_USER_GUIDE.md**: File placement in project structure

---

## References and Tools

- **Full Reference**: [KIMI.md sections 1-3](../KIMI.md)
- **Workflow Guide**: [KIMI_WORKFLOW_GUIDE.md](KIMI_WORKFLOW_GUIDE.md)
- **File Organization**: [FILE_LOCATIONS_USER_GUIDE.md](FILE_LOCATIONS_USER_GUIDE.md)

---

## Summary

Use sequential numbers, not timestamps, for daily work documents. Follow the exact format: `YYYYMMDD_NNNN_DESCRIPTIVE_NAME.md`

**Key Takeaways**:
- Sequential numbers: 0000, 0001, 0002...
- Date format: YYYYMMDD (no separators)
- Description: UPPER_SNAKE_CASE
- Use functions/automation to generate next numbers
- Kimi can help determine next sequence number

**Why This Matters**: Enables clear chronological ordering, prevents collisions, works across timezones, and integrates seamlessly with Kimi's parallel file operations.

---

**Last Updated**: 2025-11-14  
**Maintained By**: Kimi CLI Global Rules System  
**Status**: MANDATORY - No exceptions
