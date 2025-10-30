# Deployment Plan - Solar Data Augmentation Module

**Status**: 📋 PLANNED
**Priority**: 2 (Deployment & Production Testing)
**Branch**: solar-data-augmentation
**Estimated Effort**: 1-2 days
**Prerequisites**: ✅ Priority 1 complete (all 368 tests passing)

---

## Overview

Deploy the solar data augmentation module to Google Cloud Functions and validate with production data. This includes both the augmentation function (calibration + pattern application) and the analysis function (comprehensive analysis + reporting).

### Success Criteria

- ✅ Both functions deploy successfully to GCP
- ✅ End-to-end workflow completes successfully
- ✅ All 3 scenarios working correctly (INSTALL, UPGRADE, FORECAST)
- ✅ Calibration analysis reports generated
- ✅ Timestamp alignment working with real partial-year data
- ✅ No critical errors in production logs
- ✅ Performance acceptable (<5 min per user)

---

## Phase 1: Pre-Deployment Verification ✅ COMPLETE

**Status**: ✅ Already complete
**Duration**: N/A

### Tasks Completed

- [x] All 368 tests passing (106 augmentation + 184 analysis + 43 business + 35 reporting)
- [x] Code style compliant (all files <300 lines, pure functional, Result types)
- [x] Git commits clean with Claude attribution format
- [x] Documentation complete (ARCHITECTURE_TODO.md, CLAUDE.md updates)
- [x] No uncommitted changes

### Verification

```bash
# Test suite verification
bash solar_data_augmentation/tests/run_all_tests.sh
# Result: 368 passed, 0 failed, 0 errors

# Git status verification
git status
# Result: working tree clean, 17 commits ahead of origin
```

---

## Phase 2: Deploy Augmentation Function

**Status**: ⏳ PENDING
**Estimated Duration**: 1-2 hours
**Owner**: User (requires GCP access)

### 2.1: Pre-Deployment Checks

```bash
# Verify function directory structure
ls -la solar_data_augmentation/gc/augmentation/

# Check requirements.txt exists and is complete
cat solar_data_augmentation/gc/augmentation/requirements.txt

# Verify no __init__.py files (Cloud Functions requirement)
find solar_data_augmentation/gc/augmentation/ -name "__init__.py"
# Expected: no results

# Verify import paths use folder.module pattern
grep -r "from solar_data_augmentation" solar_data_augmentation/gc/augmentation/
# Expected: no results (package imports not allowed)
```

### 2.2: Deploy to GCP

**Function Details**:
- **Name**: `energy_simulations_solar_augmentation` (or similar)
- **Entry Point**: `main` (in main.py)
- **Runtime**: Python 3.11 (or 3.13)
- **Memory**: 512MB (adjust based on testing)
- **Timeout**: 300s (5 minutes)
- **Environment Variables**: None required (uses default GCP project)

**Deployment Command** (example):
```bash
cd solar_data_augmentation/gc/augmentation

gcloud functions deploy energy_simulations_solar_augmentation \
  --gen2 \
  --runtime=python311 \
  --region=australia-southeast1 \
  --source=. \
  --entry-point=main \
  --memory=512MB \
  --timeout=300s \
  --trigger-http \
  --allow-unauthenticated
```

### 2.3: Verify Deployment

```bash
# Check deployment status
gcloud functions describe energy_simulations_solar_augmentation \
  --region=australia-southeast1 \
  --gen2

# Test with simple invocation (minimal payload)
curl -X POST \
  -H "Content-Type: application/json" \
  -d '{
    "workflow_context": {...},
    "simulation_stages": [{
      "type": "FORECAST_NEW_SOLAR",
      "pvwatts_params": {...}
    }]
  }' \
  https://<FUNCTION_URL>
```

### 2.4: Smoke Test

- [ ] Function deploys without errors
- [ ] Function responds to HTTP requests
- [ ] Logs show successful initialization
- [ ] No import errors in logs
- [ ] Basic calibration calculation executes

---

## Phase 3: Deploy Analysis Function

**Status**: ⏳ PENDING
**Estimated Duration**: 1-2 hours
**Owner**: User (requires GCP access)

### 3.1: Pre-Deployment Checks

```bash
# Verify function directory structure
ls -la solar_data_augmentation/gc/analysis/

# Check requirements.txt
cat solar_data_augmentation/gc/analysis/requirements.txt

# Verify no __init__.py files
find solar_data_augmentation/gc/analysis/ -name "__init__.py"
# Expected: no results

# Verify import paths
grep -r "from solar_data_augmentation" solar_data_augmentation/gc/analysis/
# Expected: no results
```

### 3.2: Deploy to GCP

**Function Details**:
- **Name**: `energy_simulations_solar_analysis` (or similar)
- **Entry Point**: `main` (in main.py)
- **Runtime**: Python 3.11 (or 3.13)
- **Memory**: 1GB (analysis requires more memory)
- **Timeout**: 300s (5 minutes)
- **Environment Variables**: None required

**Deployment Command** (example):
```bash
cd solar_data_augmentation/gc/analysis

gcloud functions deploy energy_simulations_solar_analysis \
  --gen2 \
  --runtime=python311 \
  --region=australia-southeast1 \
  --source=. \
  --entry-point=main \
  --memory=1GB \
  --timeout=300s \
  --trigger-http \
  --allow-unauthenticated
```

### 3.3: Verify Deployment

```bash
# Check deployment status
gcloud functions describe energy_simulations_solar_analysis \
  --region=australia-southeast1 \
  --gen2

# Test with simple invocation
curl -X POST \
  -H "Content-Type: application/json" \
  -d '{
    "analysis_data_path": "gs://..../final/augmented_data.csv",
    "simulation_stages": [{...}],
    "output_base_path": "gs://..../analysis"
  }' \
  https://<FUNCTION_URL>
```

### 3.4: Smoke Test

- [ ] Function deploys without errors
- [ ] Function responds to HTTP requests
- [ ] Logs show successful initialization
- [ ] Can load data from GCS
- [ ] JSON serialization works (no sentinel object errors)
- [ ] Analysis orchestrator runs successfully

---

## Phase 4: End-to-End Integration Testing

**Status**: ⏳ PENDING
**Estimated Duration**: 2-4 hours
**Owner**: User + Claude

### 4.1: Scenario 1 - BACKCAST_SOLAR_INSTALL

**Test Data**: User with recent solar installation (partial year of data)
**Objective**: Verify auto-detection, calibration, and backfill logic

**Test Steps**:
1. Create workflow payload with BACKCAST_SOLAR_INSTALL scenario
2. Run augmentation function
3. Verify:
   - Auto-detection finds installation date
   - Calibration uses post-install data only
   - Pattern applied ONLY to pre-install hours (backfill)
   - Energy conservation validated
   - Timestamp alignment handles partial year correctly
4. Run analysis function
5. Verify:
   - Calibration analysis report generated
   - Weather classification shows day-by-day patterns
   - Confidence assessment provided
   - Scenario insights show "backfill quality: REALISTIC/CONSERVATIVE/OPTIMISTIC"

**Expected Results**:
- ✅ Auto-detection confidence >0.8
- ✅ Calibration factor 0.8-1.2 (reasonable range)
- ✅ Pre-install hours show PVWatts pattern
- ✅ Post-install hours unchanged (actual data)
- ✅ Energy conservation: PASS
- ✅ Calibration analysis: HIGH/MEDIUM confidence

### 4.2: Scenario 2 - BACKCAST_SOLAR_UPGRADE

**Test Data**: User with existing solar, upgraded capacity
**Objective**: Verify calibration from all data (no backfill)

**Test Steps**:
1. Create workflow payload with BACKCAST_SOLAR_UPGRADE scenario
2. Run augmentation function
3. Verify:
   - Calibration uses all available data
   - Pattern applied uniformly to all hours (no backfill)
   - Timestamp alignment handles full year
4. Run analysis function
5. Verify:
   - Calibration analysis shows complete data coverage
   - Weather diversity assessed
   - High confidence score (actual data throughout)

**Expected Results**:
- ✅ Calibration factor reasonable for upgrade scenario
- ✅ All hours modified uniformly
- ✅ Energy conservation: PASS
- ✅ Calibration analysis: HIGH confidence (actual data)

### 4.3: Scenario 3 - FORECAST_NEW_SOLAR

**Test Data**: User with no solar, forecasting new installation
**Objective**: Verify no calibration, pure PVWatts

**Test Steps**:
1. Create workflow payload with FORECAST_NEW_SOLAR scenario
2. Run augmentation function
3. Verify:
   - No calibration applied (factor = 1.0)
   - PVWatts pattern applied uniformly
   - Timestamp alignment handles input date range
4. Run analysis function
5. Verify:
   - Analysis reports show TMY-based predictions
   - No actual data comparison (forecast only)
   - Weather patterns from PVWatts

**Expected Results**:
- ✅ Calibration factor = 1.0
- ✅ Pure PVWatts generation pattern
- ✅ Energy conservation: PASS
- ✅ Analysis shows "forecast" scenario type

### 4.4: Calibration Method Testing

Test each calibration method with real data:

**Methods to Test**:
- PEAK_RATIO (default)
- ENERGY_RATIO
- PERCENTILE_90 (outlier resistant)
- REGRESSION (most accurate)
- TEMPORAL_PATTERN (hourly/daily/monthly)
- AUTO (intelligent selection)

**Test Matrix**:
| User | Data Period | Method | Expected Selection (AUTO) |
|------|-------------|--------|---------------------------|
| User A | 9 months | AUTO | REGRESSION (sufficient data, high quality) |
| User B | 2 months | AUTO | ENERGY_RATIO (medium data, good quality) |
| User C | 2 weeks | AUTO | PERCENTILE_90 (short data, outliers) |
| User D | 8 months | AUTO | TEMPORAL_PATTERN (long data, seasonal variation) |

**Verification**:
- [ ] Each method produces reasonable calibration factor
- [ ] AUTO selection rationale logged clearly
- [ ] TEMPORAL_PATTERN handles hourly/daily/monthly factors
- [ ] Metadata includes confidence scores and method details

### 4.5: Edge Case Testing

**Test Cases**:
1. **Leap Year Data (Feb 29)**
   - Input: Data with Feb 29 (8784 hours)
   - PVWatts: 8760 hours (non-leap year)
   - Expected: Timestamp alignment fills Feb 29 with interpolation
   - Verify: No missing values, energy conservation passes

2. **Short Post-Install Period (<1 month)**
   - Input: Solar installed 3 weeks ago
   - Expected: LOW confidence warning, PEAK_RATIO recommended
   - Verify: Calibration analysis warns about limited sample size

3. **Missing Dates in Timeline**
   - Input: Data with gaps (missing days)
   - Expected: Timestamp alignment fills with forward/backward fill
   - Verify: No null values, logs show interpolation

4. **Noisy Export Data**
   - Input: Export with outliers/spikes
   - Expected: PERCENTILE_90 or AUTO selects robust method
   - Verify: Calibration factor not skewed by outliers

5. **Partial Year Data (March-November)**
   - Input: 9 months of data (not full year)
   - PVWatts: 8760 hours (Jan-Dec)
   - Expected: Timestamp alignment joins by month/day/hour
   - Verify: Only March-November hours populated, others skipped

---

## Phase 5: Performance & Resource Monitoring

**Status**: ⏳ PENDING
**Estimated Duration**: 1-2 hours
**Owner**: User + Claude

### 5.1: Execution Time Monitoring

**Target**: <5 minutes per user workflow

**Metrics to Track**:
- Augmentation function execution time
- Analysis function execution time
- PVWatts API call latency
- GCS read/write times
- Total workflow duration

**Monitoring**:
```bash
# Download logs for specific execution
gcloud logging read "resource.labels.function_name=energy_simulations_solar_augmentation" \
  --limit=100 \
  --format=json \
  --project=solvingzero-bat

# Search for execution time logs
grep "Execution time:" logs.txt
```

**Acceptance Criteria**:
- Augmentation: <2 minutes
- Analysis: <3 minutes
- Total: <5 minutes

### 5.2: Memory Usage Monitoring

**Targets**:
- Augmentation: <512MB
- Analysis: <1GB

**Monitoring**:
```bash
# Check function memory usage in GCP Console
# Navigate to: Cloud Functions → Metrics → Memory utilization

# Or via gcloud
gcloud functions describe <FUNCTION_NAME> --gen2 --region=australia-southeast1
```

**Actions if exceeded**:
- Profile memory usage with memory_profiler
- Identify large DataFrame operations
- Optimize or increase memory allocation

### 5.3: Error Rate Monitoring

**Target**: <1% error rate in production

**Monitoring**:
```bash
# Check error logs
gcloud logging read "resource.labels.function_name=energy_simulations_solar_augmentation AND severity>=ERROR" \
  --limit=50 \
  --format=json

# Count errors vs total invocations
# Error rate = (errors / total invocations) * 100
```

**Common Errors to Watch**:
- PVWatts API timeout/rate limiting
- GCS read/write failures
- Timestamp alignment failures (invalid data)
- JSON serialization errors (unexpected types)
- Memory exceeded errors

---

## Phase 6: Production Monitoring Setup

**Status**: ⏳ PENDING
**Estimated Duration**: 1-2 hours
**Owner**: User

### 6.1: Cloud Monitoring Alerts

**Alerts to Create**:
1. **Error Rate Alert**
   - Condition: Error rate >5% over 10 minutes
   - Notification: Email/Slack

2. **Execution Time Alert**
   - Condition: P95 execution time >5 minutes
   - Notification: Email

3. **Memory Usage Alert**
   - Condition: Memory >90% for 5 minutes
   - Notification: Email

### 6.2: Logging Dashboard

**Dashboard Panels**:
1. Total invocations (last 24h)
2. Error rate (%)
3. P50/P95/P99 execution times
4. Memory utilization
5. Recent errors (list)

### 6.3: Runbook for Common Issues

**Issue 1: PVWatts API Timeout**
- **Symptom**: "PVWatts API timeout" in logs
- **Cause**: NREL API slow or rate limited
- **Fix**: Retry with exponential backoff (already implemented)
- **Escalation**: If persistent, check NREL API status

**Issue 2: Timestamp Alignment Failure**
- **Symptom**: "Alignment failed: ..." in logs
- **Cause**: Invalid input data (missing Date and Time column, wrong dtype)
- **Fix**: Validate input data format before alignment
- **Escalation**: Fix data ingestion pipeline

**Issue 3: JSON Serialization Error**
- **Symptom**: "Object of type ... is not JSON serializable"
- **Cause**: Unexpected type not handled in to_json_safe()
- **Fix**: Add type handler to json_utils.py
- **Escalation**: Report to development team

**Issue 4: Memory Exceeded**
- **Symptom**: "Exceeded memory limit" in logs
- **Cause**: Large DataFrame operations
- **Fix**: Optimize DataFrame operations or increase memory
- **Escalation**: Profile and refactor code

---

## Phase 7: Documentation Updates

**Status**: ⏳ PENDING
**Estimated Duration**: 1-2 hours
**Owner**: Claude

### 7.1: Update ARCHITECTURE_TODO.md

Mark Priority 2 as complete:
```markdown
### Priority 2: Deployment & Production Testing ✅ COMPLETE
**Status**: ✅ COMPLETE (YYYY-MM-DD)
**Deployment**: Augmentation + Analysis functions live in GCP
**Verification**: All 3 scenarios tested successfully
```

### 7.2: Create Deployment Summary Document

**File**: `docs/YYYY_MM_DD/YYYYMMDD_HHMM_DEPLOYMENT_SUMMARY.md`

**Content**:
- Deployment details (function names, URLs, memory, timeout)
- Test results (all 3 scenarios, calibration methods, edge cases)
- Performance metrics (execution time, memory usage, error rate)
- Known issues and workarounds
- Next steps

### 7.3: Update CLAUDE.md

Add deployment context to "Recent Development" section:
```markdown
### YYYY-MM-DD - ✅ Solar Data Augmentation Deployed (Branch: solar-data-augmentation → main)

**Status**: Production deployment complete ✅

**Functions Deployed**:
- energy_simulations_solar_augmentation (australia-southeast1)
- energy_simulations_solar_analysis (australia-southeast1)

**Verification**: All 3 scenarios tested, performance acceptable
**Documentation**: See docs/YYYY_MM_DD/YYYYMMDD_HHMM_DEPLOYMENT_SUMMARY.md
```

---

## Rollback Plan

If critical issues are discovered in production:

### Step 1: Assess Impact
- Check error rate and affected users
- Review logs for error patterns
- Determine if rollback necessary or hotfix possible

### Step 2: Immediate Rollback (if needed)
```bash
# Redeploy previous version from git tag
git checkout <PREVIOUS_TAG>
cd solar_data_augmentation/gc/augmentation
gcloud functions deploy energy_simulations_solar_augmentation ...

cd ../analysis
gcloud functions deploy energy_simulations_solar_analysis ...
```

### Step 3: Fix and Redeploy
- Create hotfix branch from main
- Fix critical issue
- Test thoroughly
- Redeploy with fix

### Step 4: Postmortem
- Document what went wrong
- Identify gaps in testing
- Add tests to prevent recurrence
- Update deployment checklist

---

## Success Metrics

### Deployment Success
- ✅ Both functions deployed without errors
- ✅ Smoke tests pass
- ✅ No import errors in logs
- ✅ Functions respond to HTTP requests

### Functional Success
- ✅ All 3 scenarios work correctly (INSTALL, UPGRADE, FORECAST)
- ✅ All 6 calibration methods work
- ✅ AUTO selection logic works as expected
- ✅ Timestamp alignment handles edge cases
- ✅ Energy conservation validated
- ✅ Analysis reports generated correctly

### Performance Success
- ✅ Execution time <5 minutes per user
- ✅ Memory usage within limits
- ✅ Error rate <1%
- ✅ No critical errors in production

### Quality Success
- ✅ All 368 tests still passing after deployment
- ✅ Production data validates successfully
- ✅ Calibration analysis provides useful insights
- ✅ Confidence scoring accurate

---

## Timeline

| Phase | Duration | Owner | Dependencies |
|-------|----------|-------|--------------|
| Phase 1: Pre-Deployment | ✅ Complete | Claude | None |
| Phase 2: Deploy Augmentation | 1-2 hours | User | Phase 1 |
| Phase 3: Deploy Analysis | 1-2 hours | User | Phase 1 |
| Phase 4: Integration Testing | 2-4 hours | User + Claude | Phase 2, 3 |
| Phase 5: Performance Monitoring | 1-2 hours | User + Claude | Phase 4 |
| Phase 6: Monitoring Setup | 1-2 hours | User | Phase 5 |
| Phase 7: Documentation | 1-2 hours | Claude | Phase 6 |

**Total Estimated Time**: 8-15 hours (1-2 days)

---

## Checklist

### Pre-Deployment
- [x] All 368 tests passing
- [x] Code style compliant
- [x] Documentation complete
- [x] Git clean (no uncommitted changes)

### Deployment
- [ ] Augmentation function deployed
- [ ] Analysis function deployed
- [ ] Smoke tests pass
- [ ] No errors in logs

### Integration Testing
- [ ] Scenario 1 (INSTALL) works
- [ ] Scenario 2 (UPGRADE) works
- [ ] Scenario 3 (FORECAST) works
- [ ] All calibration methods work
- [ ] Edge cases handled

### Performance
- [ ] Execution time acceptable
- [ ] Memory usage acceptable
- [ ] Error rate acceptable

### Monitoring
- [ ] Alerts configured
- [ ] Dashboard created
- [ ] Runbook documented

### Documentation
- [ ] ARCHITECTURE_TODO.md updated
- [ ] Deployment summary created
- [ ] CLAUDE.md updated

---

## Notes

- **GCP Access Required**: User must have deployment permissions
- **NREL API Key**: Required for PVWatts (should be in Secret Manager)
- **GCS Buckets**: Must exist before deployment
- **Workflow Integration**: May need to update workflow YAML to call new function URLs

## Related Documents

- [ARCHITECTURE_TODO.md](../ARCHITECTURE_TODO.md) - Master priority tracking
- [CLAUDE.md](../../CLAUDE.md) - Project guidelines
- [2025_10_28 Session Docs](../2025_10_28/) - Recent implementation fixes
