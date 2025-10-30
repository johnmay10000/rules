# Advanced Calibration Methods Implementation Plan

**Status**: 📋 PLANNED
**Priority**: 3 (Enhancement - depends on Priority 2 deployment validation)
**Estimated Effort**: 3-4 weeks
**Dependencies**: Priority 2 (production validation of existing calibration methods)

---

## Overview

Expand the solar calibration system with advanced methods that improve accuracy for different data scenarios. This includes implementing pure temporal extrapolation, ensemble methods, machine learning approaches, and hybrid physics-data models.

### Current State (Production)

**6 Calibration Methods** (implemented in Priority 1):
1. **PEAK_RATIO**: Simple peak-to-peak ratio (robust, fast)
2. **ENERGY_RATIO**: Total energy ratio (more accurate than peak)
3. **PERCENTILE_90**: Outlier-resistant using 90th percentile
4. **REGRESSION**: Linear regression with R² metric (most accurate for stable systems)
5. **TEMPORAL_PATTERN**: Time-varying calibration factors applied to PVWatts (hourly/daily/monthly)
6. **AUTO**: Intelligent selection based on data characteristics

**Architecture**:
- All methods calibrate PVWatts (physics-based model)
- Alignment happens BEFORE calibration (Fix #10 corrected)
- Pattern matching for method routing
- Result types throughout
- Comprehensive metadata and confidence scoring

### Goals for Advanced Methods

1. **Pure Data-Driven**: Methods that don't depend on PVWatts
2. **Hybrid Approaches**: Combine physics and data for best accuracy
3. **Machine Learning**: Learn complex patterns from user data
4. **Ensemble Methods**: Combine multiple methods for robustness
5. **Weather-Aware**: Account for actual weather vs TMY data
6. **Performance**: Maintain <3 min execution time per user

---

## Proposed Advanced Methods

### Method 7: TEMPORAL_DIRECT (Pure Temporal Extrapolation)

**Motivation**: User questioned why PVWatts is needed for temporal patterns. This method extracts patterns directly from user data without physics models.

**Approach**:
- Extract actual generation pattern from post-install hours
- Normalize by time-of-day, day-of-week, season
- Handle weather variation with statistical modeling
- Apply directly to pre-install period (no PVWatts)

**Algorithm**:
```
1. Extract Post-Install Data
   - Hourly generation values
   - Weather normalization (if available)
   - Remove outliers (IQR method)

2. Build Temporal Pattern
   - Group by hour-of-day (0-23)
   - Group by day-of-week (Mon-Sun)
   - Group by month (Jan-Dec)
   - Calculate percentile ranges (P10, P25, P50, P75, P90)

3. Weather Normalization (optional)
   - If cloud cover data available: normalize by cloudiness
   - If temperature data available: account for temperature effects
   - Creates "typical day" patterns

4. Interpolate Missing Patterns
   - Seasonal interpolation for months with no data
   - Similar-day interpolation for missing weekdays

5. Apply to Pre-Install Period
   - For each pre-install hour:
     - Extract hour-of-day, day-of-week, month
     - Look up pattern from temporal model
     - Apply with confidence bounds

6. Confidence Scoring
   - Based on post-install data volume
   - Weather diversity in post-install period
   - Seasonal coverage (prefer full year)
```

**Advantages**:
- No PVWatts API dependency
- Uses actual user system performance
- Captures real shading, soiling, system inefficiencies

**Disadvantages**:
- Requires significant post-install data (≥3 months recommended)
- Vulnerable to non-stationary weather patterns
- May not generalize well to different seasons

**Data Requirements**:
- Minimum: 1 month post-install (low confidence)
- Recommended: 3-6 months (medium confidence)
- Ideal: 12 months (high confidence, full seasonal coverage)

**Implementation**:
- Module: `temporal_direct_calibration.py` (~250 lines)
- Tests: `test_temporal_direct_calibration.py` (~15 tests)
- Estimated effort: 1 week

---

### Method 8: ENSEMBLE_WEIGHTED (Weighted Ensemble)

**Motivation**: Different methods excel in different scenarios. Combine multiple methods with confidence-based weighting.

**Approach**:
- Run 3-5 compatible methods in parallel
- Weight predictions by method confidence scores
- Provides robustness against method-specific failures

**Algorithm**:
```
1. Select Compatible Methods
   - PEAK_RATIO (always included, baseline)
   - ENERGY_RATIO (if sufficient data)
   - REGRESSION (if ≥3 months data and good R²)
   - TEMPORAL_PATTERN (if ≥2 months data)
   - TEMPORAL_DIRECT (if ≥3 months data)

2. Run Methods in Parallel
   - Each method returns: (factor, confidence, metadata)
   - Collect all results

3. Normalize Confidence Scores
   - Sum of all confidences = 1.0
   - Higher confidence = higher weight

4. Calculate Weighted Average
   - factor_ensemble = Σ(factor_i × confidence_i)
   - metadata includes: individual factors, weights, methods used

5. Ensemble Confidence
   - Agreement metric: variance of predictions
   - Low variance + high individual confidence = high ensemble confidence
   - High variance = low ensemble confidence (methods disagree)
```

**Advantages**:
- Robust to individual method failures
- Captures different aspects (peak vs energy vs temporal)
- High confidence when methods agree

**Disadvantages**:
- 3-5x computational cost (parallel execution helps)
- Complex to interpret (which method was "right"?)
- May mask systematic biases if all methods share them

**Implementation**:
- Module: `ensemble_weighted_calibration.py` (~200 lines)
- Tests: `test_ensemble_weighted_calibration.py` (~12 tests)
- Estimated effort: 1 week

---

### Method 9: MACHINE_LEARNING (ML-Based Calibration)

**Motivation**: Learn complex non-linear patterns from user data that simple statistical methods miss.

**Approach**:
- Train lightweight ML model (Random Forest or XGBoost)
- Features: hour, day, month, recent weather, system state
- Target: generation value
- Apply trained model to pre-install period

**Algorithm**:
```
1. Feature Engineering (Post-Install Data)
   - Temporal features: hour, day-of-week, day-of-year
   - Lag features: generation at t-1, t-24 (yesterday same hour)
   - Rolling stats: 7-day moving average, std dev
   - Seasonal features: sin/cos encoding of hour, day-of-year
   - PVWatts prediction (as one feature among many)

2. Train Model
   - Algorithm: Random Forest or XGBoost (fast, interpretable)
   - Target: Actual generation (kW)
   - Validation: Time-series cross-validation
   - Hyperparameters: Grid search with CV

3. Model Evaluation
   - R² score on validation set
   - RMSE (root mean squared error)
   - Feature importance (what drives predictions?)

4. Apply to Pre-Install
   - Generate features for each pre-install hour
   - Predict generation using trained model
   - Confidence bounds from model uncertainty

5. Confidence Scoring
   - Based on validation R²
   - Penalize if training data < 3 months
   - Penalize extrapolation beyond training distribution
```

**Advantages**:
- Captures complex non-linear patterns
- Can learn system-specific quirks (shading, soiling curves)
- Feature importance reveals what matters

**Disadvantages**:
- Requires ≥3 months data (preferably 6)
- More complex to interpret than simple methods
- Risk of overfitting with limited data
- Requires scikit-learn or xgboost dependency

**Data Requirements**:
- Minimum: 3 months (risky, prone to overfit)
- Recommended: 6 months
- Ideal: 12 months (full seasonal cycle)

**Implementation**:
- Module: `ml_calibration.py` (~300 lines)
- Tests: `test_ml_calibration.py` (~15 tests)
- Dependencies: `scikit-learn` (Random Forest)
- Estimated effort: 2 weeks

---

### Method 10: HYBRID_PHYSICS_ML (Physics-Informed ML)

**Motivation**: Combine physics model (PVWatts) with ML correction for best of both worlds.

**Approach**:
- Use PVWatts as baseline
- Train ML model to predict *correction factor* (not absolute generation)
- Leverages physics knowledge while learning system-specific deviations

**Algorithm**:
```
1. Generate PVWatts Baseline (Post-Install)
   - Align PVWatts to post-install period
   - Calculate residuals: actual - pvwatts

2. Feature Engineering
   - Temporal features: hour, day, month
   - PVWatts prediction (kW)
   - PVWatts error in last 7 days (recent system state)
   - Cumulative deviation from PVWatts (soiling effect proxy)

3. Train Correction Model
   - Target: Correction factor (actual / pvwatts)
   - Algorithm: Gradient Boosting (XGBoost)
   - Regularization: Prevent learning noise

4. Apply to Pre-Install
   - Generate PVWatts for pre-install period
   - Predict correction factor using trained model
   - Final generation = pvwatts × correction_factor

5. Physics Constraints
   - Correction factor clipped to [0.5, 2.0] (reasonable range)
   - Energy conservation check
   - Peak power cannot exceed system capacity × 1.2
```

**Advantages**:
- Physics model provides reliable baseline (works with limited data)
- ML learns system-specific deviations
- More robust than pure ML (can't predict impossible values)
- Requires less data than pure ML

**Disadvantages**:
- Still requires PVWatts API
- More complex than simple methods
- Needs ≥2 months data for reasonable ML

**Data Requirements**:
- Minimum: 2 months (ML learns corrections)
- Recommended: 4 months
- Ideal: 12 months

**Implementation**:
- Module: `hybrid_physics_ml_calibration.py` (~280 lines)
- Tests: `test_hybrid_physics_ml_calibration.py` (~18 tests)
- Dependencies: `xgboost` or `lightgbm`
- Estimated effort: 2 weeks

---

### Method 11: WEATHER_ADJUSTED (Weather-Normalized Calibration)

**Motivation**: Account for actual weather vs TMY (typical meteorological year) data used by PVWatts.

**Approach**:
- Fetch actual weather data for post-install period
- Compare actual vs TMY weather
- Adjust calibration factor to account for weather differences

**Algorithm**:
```
1. Fetch Actual Weather Data
   - Source: NREL NSRDB, OpenWeatherMap, or similar
   - Metrics: Solar irradiance (GHI), cloud cover, temperature
   - Period: Post-install dates

2. Compare Actual vs TMY Weather
   - PVWatts uses TMY data (typical year)
   - Calculate weather bias:
     - Irradiance ratio: actual_GHI / TMY_GHI
     - Cloud cover difference
     - Temperature coefficient

3. Weather-Adjusted Calibration
   - Base calibration: PEAK_RATIO or ENERGY_RATIO
   - Adjust for weather bias:
     factor_adjusted = factor_base × (TMY_GHI / actual_GHI)
   - Account for temperature effects (if significant)

4. Confidence Adjustment
   - Higher confidence if actual weather close to TMY
   - Lower confidence if post-install period unusually sunny/cloudy
   - Warn user if weather unrepresentative

5. Apply to Pre-Install
   - Pre-install uses TMY weather (PVWatts standard)
   - Weather-adjusted factor provides realistic expectation
```

**Advantages**:
- Accounts for non-typical weather in post-install period
- More accurate than raw calibration
- Warns users when post-install weather unrepresentative

**Disadvantages**:
- Requires weather data API (NREL NSRDB or commercial)
- API rate limits / costs
- Complex to implement correctly

**Data Requirements**:
- Minimum: 2 weeks post-install
- Recommended: 1 month
- Requires weather API access

**Implementation**:
- Module: `weather_adjusted_calibration.py` (~250 lines)
- Tests: `test_weather_adjusted_calibration.py` (~12 tests)
- Dependencies: Weather API client (NREL NSRDB)
- Estimated effort: 1.5 weeks

---

## Implementation Phases

### Phase 1: TEMPORAL_DIRECT (3 weeks)

**Priority**: HIGH (user explicitly requested this)

**Tasks**:
1. **Week 1: Research & Design**
   - [ ] Research temporal extrapolation algorithms
   - [ ] Design ADT types (TemporalPattern, TemporalBucket, etc.)
   - [ ] Define confidence scoring algorithm
   - [ ] Create test data scenarios

2. **Week 2: Implementation**
   - [ ] Implement `temporal_direct_calibration.py` (~250 lines)
   - [ ] Pattern extraction from post-install data
   - [ ] Temporal bucket grouping (hourly/daily/monthly)
   - [ ] Weather normalization (optional)
   - [ ] Seasonal interpolation for missing data
   - [ ] Integration with method selector

3. **Week 3: Testing & Validation**
   - [ ] Write 15 comprehensive tests
   - [ ] Test with 4 user scenarios (1 month, 3 months, 6 months, 12 months data)
   - [ ] Compare accuracy vs TEMPORAL_PATTERN
   - [ ] Compare accuracy vs REGRESSION
   - [ ] Document trade-offs and use cases

**Success Criteria**:
- All 15 tests passing
- Accuracy within 10% of TEMPORAL_PATTERN for 6+ month data
- Better accuracy than PVWatts-based methods when system has unique characteristics
- Clear documentation of when to use vs TEMPORAL_PATTERN

**Deliverables**:
- `temporal_direct_calibration.py` (production code)
- `test_temporal_direct_calibration.py` (tests)
- Documentation: `TEMPORAL_DIRECT_METHOD_GUIDE.md`
- Comparison analysis: `TEMPORAL_DIRECT_VS_TEMPORAL_PATTERN.md`

---

### Phase 2: ENSEMBLE_WEIGHTED (2 weeks)

**Priority**: MEDIUM (improves robustness)

**Tasks**:
1. **Week 1: Implementation**
   - [ ] Implement `ensemble_weighted_calibration.py` (~200 lines)
   - [ ] Parallel method execution
   - [ ] Confidence normalization
   - [ ] Weighted averaging
   - [ ] Agreement metric (variance)
   - [ ] Ensemble confidence scoring

2. **Week 2: Testing & Validation**
   - [ ] Write 12 comprehensive tests
   - [ ] Test with different method combinations
   - [ ] Validate agreement metric accuracy
   - [ ] Compare vs individual methods
   - [ ] Edge case: all methods disagree

**Success Criteria**:
- All 12 tests passing
- Ensemble confidence accurately reflects method agreement
- Weighted average performs better than individual methods (on average)

**Deliverables**:
- `ensemble_weighted_calibration.py` (production code)
- `test_ensemble_weighted_calibration.py` (tests)
- Documentation: `ENSEMBLE_METHOD_GUIDE.md`

---

### Phase 3: MACHINE_LEARNING (3 weeks)

**Priority**: MEDIUM (research/experimental)

**Tasks**:
1. **Week 1: Research & Feature Engineering**
   - [ ] Research appropriate ML algorithms (Random Forest, XGBoost)
   - [ ] Design feature engineering pipeline
   - [ ] Create validation strategy (time-series CV)
   - [ ] Define hyperparameter search space

2. **Week 2: Implementation**
   - [ ] Implement `ml_calibration.py` (~300 lines)
   - [ ] Feature extraction
   - [ ] Model training pipeline
   - [ ] Time-series cross-validation
   - [ ] Model persistence (save/load)
   - [ ] Integration with method selector

3. **Week 3: Testing & Validation**
   - [ ] Write 15 comprehensive tests
   - [ ] Test with 4 user scenarios
   - [ ] Compare vs REGRESSION and TEMPORAL_DIRECT
   - [ ] Feature importance analysis
   - [ ] Overfitting detection

**Success Criteria**:
- All 15 tests passing
- R² > 0.9 on validation set (6+ month data)
- No overfitting detected
- Feature importance makes physical sense

**Deliverables**:
- `ml_calibration.py` (production code)
- `test_ml_calibration.py` (tests)
- Documentation: `ML_METHOD_GUIDE.md`
- Analysis: `ML_FEATURE_IMPORTANCE_ANALYSIS.md`

---

### Phase 4: HYBRID_PHYSICS_ML (3 weeks)

**Priority**: HIGH (best of both worlds)

**Tasks**:
1. **Week 1: Design & Research**
   - [ ] Design physics-informed feature engineering
   - [ ] Research correction factor modeling
   - [ ] Define physics constraints
   - [ ] Create validation strategy

2. **Week 2: Implementation**
   - [ ] Implement `hybrid_physics_ml_calibration.py` (~280 lines)
   - [ ] PVWatts baseline generation
   - [ ] Correction factor prediction
   - [ ] Physics constraint enforcement
   - [ ] Integration with method selector

3. **Week 3: Testing & Validation**
   - [ ] Write 18 comprehensive tests
   - [ ] Test with 4 user scenarios
   - [ ] Compare vs pure ML and pure physics
   - [ ] Validate physics constraints work
   - [ ] Low-data scenario testing (2 months)

**Success Criteria**:
- All 18 tests passing
- Outperforms pure ML with limited data (<3 months)
- Outperforms pure physics with sufficient data (≥6 months)
- Physics constraints prevent unrealistic predictions

**Deliverables**:
- `hybrid_physics_ml_calibration.py` (production code)
- `test_hybrid_physics_ml_calibration.py` (tests)
- Documentation: `HYBRID_METHOD_GUIDE.md`
- Comparison: `HYBRID_VS_PURE_ML_VS_PURE_PHYSICS.md`

---

### Phase 5: WEATHER_ADJUSTED (2 weeks)

**Priority**: LOW (requires weather API)

**Tasks**:
1. **Week 1: Implementation**
   - [ ] Implement weather API client (NREL NSRDB)
   - [ ] Implement `weather_adjusted_calibration.py` (~250 lines)
   - [ ] Actual vs TMY weather comparison
   - [ ] Weather bias calculation
   - [ ] Integration with existing methods

2. **Week 2: Testing & Validation**
   - [ ] Write 12 comprehensive tests
   - [ ] Test with real weather data
   - [ ] Mock weather API for testing
   - [ ] Compare vs non-weather-adjusted

**Success Criteria**:
- All 12 tests passing
- Improves accuracy when post-install weather atypical
- Warns users appropriately

**Deliverables**:
- `weather_adjusted_calibration.py` (production code)
- `test_weather_adjusted_calibration.py` (tests)
- Documentation: `WEATHER_ADJUSTED_METHOD_GUIDE.md`

---

## Architecture Considerations

### Module Organization

```
solar_data_augmentation/gc/augmentation/core/solar_calibration/
├── calibration_methods.py          # Existing basic methods
├── regression_calibration.py       # Existing REGRESSION
├── temporal_pattern_calibration.py # Existing TEMPORAL_PATTERN
├── auto_selector.py                # Existing AUTO selection
├── method_selector.py              # Existing routing
├── calibration_calculator.py       # Main entry point
├── temporal_direct_calibration.py  # NEW: Method 7
├── ensemble_weighted_calibration.py # NEW: Method 8
├── ml_calibration.py               # NEW: Method 9
├── hybrid_physics_ml_calibration.py # NEW: Method 10
├── weather_adjusted_calibration.py  # NEW: Method 11
└── calibration_types.py            # Shared types (updated)
```

### Type System Updates

```python
# calibration_types.py

CalibrationMethodType = Literal[
    "PEAK_RATIO",
    "ENERGY_RATIO",
    "PERCENTILE_90",
    "REGRESSION",
    "TEMPORAL_PATTERN",
    "TEMPORAL_DIRECT",      # NEW
    "ENSEMBLE_WEIGHTED",    # NEW
    "MACHINE_LEARNING",     # NEW
    "HYBRID_PHYSICS_ML",    # NEW
    "WEATHER_ADJUSTED",     # NEW
    "AUTO"
]

@dataclass(frozen=True)
class TemporalDirectMetadata:
    """Metadata for TEMPORAL_DIRECT method."""
    hourly_patterns: dict[int, float]      # Hour (0-23) → generation (kW)
    daily_patterns: dict[int, float]       # Day (0-6) → generation (kW)
    monthly_patterns: dict[int, float]     # Month (0-11) → generation (kW)
    post_install_months: int               # Number of post-install months
    weather_diversity_score: float         # 0.0-1.0
    seasonal_coverage: list[str]           # Months covered: ["Jan", "Feb", ...]
    confidence_breakdown: dict[str, float] # Component confidence scores

@dataclass(frozen=True)
class EnsembleMetadata:
    """Metadata for ENSEMBLE_WEIGHTED method."""
    methods_used: list[CalibrationMethodType]
    individual_factors: dict[str, float]   # Method → factor
    individual_confidences: dict[str, float]
    normalized_weights: dict[str, float]   # Method → weight
    agreement_metric: float                # Variance of predictions
    ensemble_confidence: float             # Combined confidence
```

### AUTO Selector Updates

The AUTO selector will need to be updated to consider new methods:

```python
def select_best_method_auto(
    actual_export: pl.Series,
    pvwatts_generation: pl.Series,
    timestamps: pl.Series,
    config: CalibrationConfig
) -> CalibrationMethodType:
    """
    Intelligent method selection based on data characteristics.

    Decision tree (updated):
    1. Check data volume
    2. Check weather diversity (if available)
    3. Check temporal variation
    4. Check data quality
    5. Select best method for scenario
    """
    months_of_data = estimate_months_of_data(timestamps)
    temporal_variation = calculate_temporal_variation(actual_export, timestamps)
    data_quality = assess_data_quality(actual_export)

    # NEW: Check if weather data available
    weather_available = check_weather_api_available()

    # Decision tree
    if months_of_data >= 12 and data_quality > 0.9:
        # Full year, high quality → best methods
        return "HYBRID_PHYSICS_ML"  # Best of physics + ML

    elif months_of_data >= 6 and data_quality > 0.8:
        # Half year, good quality
        if temporal_variation > 0.3:
            return "TEMPORAL_DIRECT"  # Strong seasonal patterns
        else:
            return "ENSEMBLE_WEIGHTED"  # Robust combination

    elif months_of_data >= 3 and data_quality > 0.7:
        # Quarter year, decent quality
        if weather_available and check_weather_atypical(timestamps):
            return "WEATHER_ADJUSTED"  # Adjust for unusual weather
        else:
            return "REGRESSION"  # Standard linear fit

    elif months_of_data >= 1:
        # 1-3 months
        return "ENERGY_RATIO"  # Simple, works with limited data

    else:
        # <1 month
        return "PEAK_RATIO"  # Fallback, most robust
```

### Backward Compatibility

All existing workflows will continue to work:
- Default method remains PEAK_RATIO
- AUTO selection prioritizes proven methods for limited data
- New methods only used when AUTO determines they're appropriate
- Explicit method selection in workflow still supported

### Performance Targets

- **TEMPORAL_DIRECT**: <30 seconds (similar to TEMPORAL_PATTERN)
- **ENSEMBLE_WEIGHTED**: <2 minutes (parallel execution of 3-5 methods)
- **MACHINE_LEARNING**: <1 minute (training) + <5 seconds (inference)
- **HYBRID_PHYSICS_ML**: <1 minute (training) + <10 seconds (inference)
- **WEATHER_ADJUSTED**: <45 seconds (includes weather API call)

**Overall target**: <3 minutes per user (within budget)

---

## Testing Strategy

### Unit Tests

Each method gets comprehensive unit tests (12-18 tests):
- Basic functionality
- Edge cases (sparse data, outliers, missing values)
- Confidence scoring
- Metadata generation
- Error handling

### Integration Tests

Test method selection and routing:
- AUTO selector chooses correct method
- Method switching works correctly
- Metadata propagates correctly

### Comparison Tests

Compare new methods vs existing:
- Accuracy comparison (MAE, RMSE, R²)
- Confidence calibration (do confidence scores match actual accuracy?)
- Execution time benchmarks

### User Scenario Tests

Test with real user data patterns:
- User A: 1 month post-install (limited data)
- User B: 3 months post-install (medium data)
- User C: 6 months post-install (good data)
- User D: 12 months post-install (ideal data)

Each scenario tests all methods and compares results.

---

## Success Metrics

### Accuracy Metrics

- **MAE** (Mean Absolute Error): <10% of actual generation
- **RMSE** (Root Mean Squared Error): <15% of actual generation
- **R²**: >0.85 for 6+ month data, >0.95 for 12 month data

### Confidence Calibration

- Confidence scores should correlate with actual accuracy
- 90% confidence → 90% of predictions within 10% of actual
- Low confidence → user warned appropriately

### Performance

- All methods complete within 3 minutes
- No memory leaks
- Handles edge cases gracefully

### User Experience

- Clear method recommendations in AUTO mode
- Transparent confidence scoring
- Actionable warnings when data insufficient

---

## Dependencies

### Python Packages (New)

```toml
[project]
dependencies = [
    "polars>=0.19.0",           # Existing
    "requests>=2.31.0",         # Existing
    "scikit-learn>=1.3.0",      # NEW: For MACHINE_LEARNING method
    "xgboost>=2.0.0",           # NEW: For HYBRID_PHYSICS_ML method
]
```

### External APIs (Optional)

- **NREL NSRDB**: Weather data for WEATHER_ADJUSTED method
- **OpenWeatherMap**: Alternative weather source
- **PVWatts v8**: Existing dependency

---

## Risks & Mitigation

### Risk 1: ML Methods Overfit with Limited Data

**Mitigation**:
- Strict validation using time-series cross-validation
- Regularization in all ML models
- AUTO selector only uses ML with ≥3 months data
- Confidence scoring penalizes limited training data

### Risk 2: Performance Degradation

**Mitigation**:
- Parallel method execution for ENSEMBLE
- Cached ML models (save/load)
- Timeout limits on all methods (30-120 seconds)
- Graceful degradation (fallback to simpler methods)

### Risk 3: Increased Complexity

**Mitigation**:
- Clear documentation for each method
- AUTO selector abstracts complexity for most users
- Comprehensive testing (400+ tests total)
- Gradual rollout (one method at a time)

### Risk 4: Weather API Costs/Limits

**Mitigation**:
- WEATHER_ADJUSTED is optional (not in AUTO by default)
- Cache weather data (avoid repeated API calls)
- Fallback to non-weather-adjusted methods
- Document API costs upfront

---

## Timeline

### Total Estimated Time: 13 weeks (3-4 months)

| Phase | Method | Duration | Dependencies |
|-------|--------|----------|--------------|
| Phase 1 | TEMPORAL_DIRECT | 3 weeks | Priority 2 complete |
| Phase 2 | ENSEMBLE_WEIGHTED | 2 weeks | Phase 1 complete |
| Phase 3 | MACHINE_LEARNING | 3 weeks | Phase 1 complete |
| Phase 4 | HYBRID_PHYSICS_ML | 3 weeks | Phase 3 complete |
| Phase 5 | WEATHER_ADJUSTED | 2 weeks | Phase 1 complete |

**Phases 1, 2, 5 can run in parallel** (different developers)
**Phases 3, 4 must be sequential** (Phase 4 builds on Phase 3)

**Aggressive timeline**: 6 weeks (parallel execution, multiple developers)
**Conservative timeline**: 13 weeks (sequential, single developer)

---

## Documentation Deliverables

### User-Facing Documentation

1. **Method Selection Guide**: When to use each method
2. **Accuracy Comparison**: Benchmark results across all methods
3. **Troubleshooting Guide**: Common issues and solutions

### Technical Documentation

1. **Architecture Overview**: How methods integrate
2. **Method Implementation Guides**: For each new method
3. **Testing Strategy**: How to validate new methods
4. **Performance Benchmarks**: Execution time and memory usage

### Research Documentation

1. **Comparison Analysis**: TEMPORAL_DIRECT vs TEMPORAL_PATTERN
2. **ML Feature Importance**: What drives ML predictions?
3. **Ensemble Weighting Analysis**: How agreement affects confidence
4. **Weather Impact Study**: Effect of atypical weather on calibration

---

## Future Enhancements (Beyond Phase 5)

### Potential Method 12: SATELLITE_IMAGERY

- Use satellite solar irradiance data
- Account for actual cloud cover over user's location
- Requires satellite API (expensive)

### Potential Method 13: PEER_COMPARISON

- Compare against similar systems in same area
- Requires database of peer systems
- Privacy concerns

### Potential Method 14: DEGRADATION_AWARE

- Model panel degradation over time
- Requires long-term historical data
- Improves accuracy for aging systems

---

## References

### Existing Implementation

- **Priority 1 Calibration**: `solar_data_augmentation/gc/augmentation/core/solar_calibration/`
- **Method Selector**: `method_selector.py`
- **AUTO Selector**: `auto_selector.py`
- **Calibration Calculator**: `calibration_calculator.py`

### Research Documents

- `docs/2025_10_23/2025_10_23_1015_ADVANCED_CALIBRATION_RESEARCH.md`
- `docs/2025_10_23/2025_10_23_1016_ADVANCED_CALIBRATION_IMPLEMENTATION_PLAN.md`
- `docs/2025_10_22/2025_10_22_2015_CALIBRATION_METHODS_GUIDE.md`

### Related Plans

- `docs/plans/DEPLOYMENT_PLAN.md` (Priority 2)
- `docs/ARCHITECTURE_TODO.md` (Priority 3, Task 3.4)

---

## Approval & Sign-Off

**Created**: 2025-10-28
**Status**: 📋 PLANNED (awaiting Priority 2 completion)
**Next Steps**:
1. Complete Priority 2 (deployment & production validation)
2. Review this plan with stakeholders
3. Prioritize phases (which methods first?)
4. Begin Phase 1 (TEMPORAL_DIRECT) implementation

**Questions for Stakeholders**:
1. Which advanced methods are highest priority?
2. Is ML dependency (scikit-learn/xgboost) acceptable?
3. Are weather API costs acceptable for WEATHER_ADJUSTED?
4. Timeline preference: aggressive (6 weeks) vs conservative (13 weeks)?
