# Testing Rule Clarification

**Created**: 2025-10-30 00:35  
**Priority**: 🔥 CRITICAL - Mandatory Requirement  
**User Question**: "Regarding tests, what is the rule here?"

---

## Executive Summary

**The Testing Rule** (from user rules):
> "Ensure there are comprehensive tests that pass for any code and modules created."

This document clarifies what "comprehensive" means and how it applies across all contexts.

---

## Core Testing Rule (MANDATORY)

### Rule Statement

**ALL production code MUST have comprehensive tests that pass before committing.**

### What "Comprehensive" Means

"Comprehensive" is context-dependent but MUST include:

1. **Happy Path Coverage**
   - Test primary use case(s)
   - Verify expected behavior works correctly
   - Confirm return types/values are correct

2. **Error Path Coverage**
   - Test failure scenarios
   - Verify `Failure()` cases in `Result[T, E]` types
   - Confirm error messages are helpful

3. **Edge Cases**
   - Boundary conditions (empty lists, zero values, etc.)
   - Null/None handling (if applicable)
   - Type validation (if using `Any` or dynamic types)

4. **Integration Points**
   - Test interactions between modules
   - Verify external dependencies (mocked or stubbed)
   - Confirm data flows correctly through pipeline

### Minimum Test Coverage

| Code Type | Minimum Tests | Rationale |
|-----------|---------------|-----------|
| **Pure Functions** | 3+ tests | Happy path + 2 edge/error cases |
| **ADT Types** | 1+ test per variant | Ensure pattern matching works |
| **Integration Functions** | 2+ tests | Success + failure scenarios |
| **Cloud Functions (Entry Point)** | 4+ tests | Valid request, invalid request, error handling, integration |
| **Business Logic Modules** | 80%+ coverage | Critical business rules must be thoroughly tested |

**Note**: These are MINIMUMS. Complex functions may need 10+ tests.

---

## Testing Framework

### Python

**Tool**: `pytest`

**Command**:
```bash
# Run all tests
uv run pytest tests/

# Run specific test file
uv run pytest tests/test_module.py

# Run with verbose output
uv run pytest -v tests/

# Run with coverage report
uv run pytest --cov=module tests/
```

**From user rules**:
> "In python, always use `uv run` to run a test module"

**From code-style.mdc**:
> "Always use `pytest` to run your tests"

### TypeScript

**Tool**: Jest, Vitest, or AVA (project-specific)

**Command**:
```bash
# Run all tests
npm test

# Run specific test file
npm test -- path/to/test.spec.ts

# Run with coverage
npm test -- --coverage
```

### Swift

**Tool**: XCTest

**Command**:
```bash
swift test

# Run specific test
swift test --filter TestName
```

### Kotlin

**Tool**: JUnit 5

**Command**:
```bash
./gradlew test

# Run specific test class
./gradlew test --tests "com.example.TestClass"
```

---

## Special Case: Google Cloud Functions Testing

**Context** (from code-style.mdc):
> "Everything is built and tested in the context of deployment and executing in Google Cloud Run Functions. Every test has to add sys.path.append functionality to access production modules"

### Why `sys.path.append()` Is Required

Google Cloud Functions deploy ALL files to `/workspace/` directory WITHOUT package structure. This means:

- ❌ **No package imports** (`from package.module import func`)
- ❌ **No relative imports** (`from .module import func`)
- ✅ **Only direct imports** (`from module import func`)

**Problem**: Tests live in `tests/` directory, production code lives in `gc/function_name/`

**Solution**: Add function directory to `sys.path` in tests to simulate Cloud Functions environment.

### Test Structure for Cloud Functions

**From CLAUDE.md**:

```python
# test_module.py
import sys
from pathlib import Path

# Add the function directory to path (simulates Cloud Functions deployment)
function_dir = Path(__file__).parent.parent.parent / "gc" / "function_name"
sys.path.insert(0, str(function_dir))

# Now imports work as they would in Cloud Functions
from module_name import function_to_test
from subfolder.module import another_function
from types import Result, Success, Failure

def test_happy_path():
    """Test primary use case"""
    result = function_to_test(valid_input)
    assert isinstance(result, Success)
    assert result.value.field == expected_value

def test_error_handling():
    """Test failure scenario"""
    result = function_to_test(invalid_input)
    assert isinstance(result, Failure)
    assert "helpful error message" in result.error

def test_edge_case_empty_input():
    """Test boundary condition"""
    result = function_to_test([])
    # Define expected behavior for empty input
```

### Cloud Functions Test Organization

```
project/
├── gc/
│   └── function_name/            # Production code (deployed to Cloud Functions)
│       ├── main.py               # Entry point
│       ├── function_orchestrator.py
│       ├── types.py
│       └── subfolder/
│           └── module.py
└── tests/
    └── gc/
        └── function_name/        # Test code (mirrors structure)
            ├── test_main.py
            ├── test_function_orchestrator.py
            ├── test_types.py
            └── test_subfolder/
                └── test_module.py
```

**Each test file MUST**:
1. Add `sys.path.append()` to find production code
2. Use direct imports (matching Cloud Functions deployment)
3. Test `Result[T, E]` return types
4. Cover happy path + error cases

---

## Test Quality Standards

### 1. Tests Must Be Deterministic

**Bad**:
```python
def test_timestamp():
    result = get_current_timestamp()
    assert result == "2025-10-30"  # ❌ Fails tomorrow
```

**Good**:
```python
def test_timestamp():
    fixed_time = datetime(2025, 10, 30)
    result = get_timestamp(fixed_time)
    assert result == "2025-10-30"  # ✅ Always passes
```

### 2. Tests Must Be Independent

**Bad**:
```python
# test_calculator.py
state = 0

def test_add():
    global state
    state += 5
    assert state == 5

def test_multiply():
    global state  # ❌ Depends on test_add running first
    state *= 2
    assert state == 10
```

**Good**:
```python
def test_add():
    state = 0
    state += 5
    assert state == 5  # ✅ Isolated

def test_multiply():
    state = 5
    state *= 2
    assert state == 10  # ✅ Isolated
```

### 3. Tests Must Have Clear Names

**Bad**:
```python
def test_1():  # ❌ What does this test?
def test_function():  # ❌ Too vague
def test_edge_case():  # ❌ Which edge case?
```

**Good**:
```python
def test_calculate_total_with_valid_items():  # ✅ Clear
def test_calculate_total_with_empty_list():  # ✅ Specific
def test_calculate_total_returns_failure_for_negative_price():  # ✅ Descriptive
```

### 4. Tests Must Use FP-Style Assertions

**For `Result[T, E]` Types**:

```python
from returns.result import Success, Failure

def test_parse_request_success():
    """Test successful request parsing"""
    result = parse_request(valid_request)
    
    # ✅ Check Success/Failure type first
    assert isinstance(result, Success)
    
    # ✅ Then unwrap and check value
    parsed = result.unwrap()
    assert parsed.user_id == "12345"
    assert parsed.action == "CREATE"

def test_parse_request_failure():
    """Test failed request parsing"""
    result = parse_request(invalid_request)
    
    # ✅ Check Failure type
    assert isinstance(result, Failure)
    
    # ✅ Then check error message
    error = result.failure()
    assert "missing required field" in error
```

---

## When Tests Are Required

### Always Required

✅ **New modules** - All new code MUST have tests  
✅ **New functions** - All new functions MUST have tests  
✅ **Bug fixes** - Add test that reproduces bug, then fix  
✅ **Before git commits** - Tests MUST pass before committing  

### Optional (But Recommended)

⚠️ **Exploratory code** - Quick experiments, but add tests if keeping  
⚠️ **Type definitions only** - ADTs without logic (but test usage)  
⚠️ **Simple utilities** - Trivial helpers (e.g., `get_timestamp()`)  

### Explicitly Exempted

❌ **Configuration files** - JSON, YAML, etc.  
❌ **Documentation** - Markdown files  
❌ **Build scripts** - Shell scripts, Makefiles  
❌ **Templates** - Starter files for new projects  

---

## Integration with Git Workflow

**From CLAUDE.md** (git checkpoint checklist):

> **What to check before committing**:
> 1. ✅ Run relevant tests (if applicable)
> 2. ✅ Verify code follows style guidelines
> 3. ✅ Check that files are properly staged
> 4. ✅ Review git diff to ensure changes are intentional

**Mandatory Workflow**:

```bash
# 1. Run tests
uv run pytest tests/

# 2. Verify tests pass
# All tests must pass (no skipped, no failed)

# 3. Stage changes
git add <files>

# 4. Review diff
git diff --staged

# 5. Commit (only if tests pass)
git commit -m "Descriptive message

Added feature X with tests:
- test_feature_x_happy_path
- test_feature_x_error_handling
- test_feature_x_edge_cases

All tests passing (total: X tests)"
```

**Rule**: If tests fail, DO NOT commit. Fix tests first.

---

## Test-Driven Development (TDD) - Recommended

While not MANDATORY, TDD is STRONGLY RECOMMENDED for complex features:

### TDD Cycle (Red-Green-Refactor)

1. **Write Test First** (Red)
   ```python
   def test_calculate_discount():
       result = calculate_discount(price=100, percent=10)
       assert isinstance(result, Success)
       assert result.unwrap() == 90
   ```
   ❌ Test fails (function doesn't exist yet)

2. **Write Minimal Code** (Green)
   ```python
   def calculate_discount(price: float, percent: float) -> Result[float, str]:
       if price < 0 or percent < 0:
           return Failure("Negative values not allowed")
       return Success(price - (price * percent / 100))
   ```
   ✅ Test passes

3. **Refactor** (Clean)
   ```python
   def calculate_discount(price: float, percent: float) -> Result[float, str]:
       # Refactor: Extract validation
       validation = validate_inputs(price, percent)
       if isinstance(validation, Failure):
           return validation
       
       discount = price * percent / 100
       return Success(price - discount)
   ```
   ✅ Test still passes, code is cleaner

### Benefits of TDD

- 🎯 Tests define requirements before coding
- 🛡️ Prevents regressions during refactoring
- 📝 Tests serve as living documentation
- 🚀 Faster development (fewer debugging cycles)
- ✅ Higher confidence in code correctness

---

## Test Metrics (For Reporting)

When documenting completed work, include test metrics:

### Example (from CLAUDE.md):

> **Test Results**: 368 total tests passing (106 augmentation + 184 analysis + 43 business + 35 reporting)
> - **Calibration Tests**: 89 tests (13+14+15+20+17+10) covering all 6 modules
> - **Integration Tests**: 10 tests validating full calibration pipeline

### Recommended Format

```markdown
## Test Coverage Summary

**Total Tests**: X passing
- **Unit Tests**: Y (covering individual functions)
- **Integration Tests**: Z (covering module interactions)
- **End-to-End Tests**: W (covering full workflows)

**Coverage**: XX% (if measured)

**Test Files**:
- `test_module_a.py`: 15 tests
- `test_module_b.py`: 23 tests
- `test_integration.py`: 8 tests
```

---

## Testing Anti-Patterns (Avoid These)

### ❌ No Tests

**Problem**:
```python
# module.py
def complex_calculation(data):
    # 200 lines of complex logic
    return result

# NO test_module.py file
```

**Why Bad**: No confidence that code works, refactoring is dangerous

**Fix**: Write tests immediately after (or before) implementing

---

### ❌ Tests That Don't Assert Anything

**Problem**:
```python
def test_function():
    result = some_function()
    # ❌ No assertion, test always passes
```

**Why Bad**: False sense of security, doesn't validate behavior

**Fix**: Always assert expected outcome

---

### ❌ Tests That Test Implementation, Not Behavior

**Problem**:
```python
def test_function():
    # ❌ Testing internal variable names
    assert function._internal_cache == {}
    assert function._counter == 0
```

**Why Bad**: Tests break when refactoring, even if behavior is correct

**Fix**: Test public API and behavior only

---

### ❌ One Giant Test

**Problem**:
```python
def test_everything():
    # Test 1: ...
    # Test 2: ...
    # Test 3: ...
    # 50 lines testing 10 different things
```

**Why Bad**: Hard to debug failures, unclear what broke

**Fix**: One test per scenario

---

## Migrating Existing Code (Per User Decision 4)

**User Decision**:
> "Decision 4 is mandatory but monitor older code to review for updating the old code to mandatory FP, small incremental changes always with testing"

### Migration Strategy for Tests

**Old Code WITHOUT Tests**:

1. **Add Tests Incrementally**
   - Start with most critical functions
   - Add tests before refactoring
   - Aim for 80%+ coverage over time

2. **When Touching Old Code**
   - ✅ ALWAYS add tests when modifying old code
   - ✅ Test new behavior + existing behavior
   - ✅ If bug found, write failing test first, then fix

3. **Small Changes Only**
   - Don't rewrite entire module at once
   - Add tests to one function at a time
   - Commit frequently with passing tests

**Example Migration**:

```python
# old_module.py (no tests)
def legacy_function(x):
    # 50 lines of untested legacy code
    return result

# Step 1: Add tests for current behavior (even if not ideal)
# test_old_module.py
def test_legacy_function_current_behavior():
    """Document current behavior (even if buggy)"""
    result = legacy_function(10)
    assert result == 42  # Current output (may be wrong)

# Step 2: Fix bugs with new tests
def test_legacy_function_correct_behavior():
    """Test CORRECT behavior"""
    result = legacy_function(10)
    assert result == 50  # ✅ Correct output

# Step 3: Refactor incrementally (with tests as safety net)
# All tests pass after refactoring → safe to commit
```

---

## Summary: The Testing Rule

### Mandatory Requirements

1. ✅ **Comprehensive tests** for all new production code
2. ✅ **Tests must pass** before committing
3. ✅ **Use pytest** (Python) or appropriate framework for language
4. ✅ **Use `uv run`** for Python test execution
5. ✅ **Cloud Functions**: Use `sys.path.append()` in tests
6. ✅ **Cover happy path + error cases + edge cases**
7. ✅ **Test `Result[T, E]` types** (Success/Failure cases)

### Best Practices (Strongly Recommended)

1. 🎯 **TDD for complex features** (write tests first)
2. 📊 **Report test metrics** in documentation
3. 🔄 **Add tests when touching old code** (incremental migration)
4. 🎨 **Clear test names** (describe what is being tested)
5. 🔬 **Isolated tests** (no shared state)
6. 📈 **Aim for 80%+ coverage** (measured or estimated)

### Context-Specific Rules

| Context | Minimum Tests | Special Requirements |
|---------|---------------|---------------------|
| **Pure Functions** | 3+ | Happy + 2 edge/error |
| **ADT Types** | 1+ per variant | Test pattern matching |
| **Cloud Functions** | 4+ | Use `sys.path.append()` |
| **Integration** | 2+ | Success + failure |
| **Business Logic** | 80%+ coverage | Critical paths fully tested |
| **Legacy Code** | Add incrementally | Test before refactoring |

### When to Run Tests

- ✅ **During development** (every few minutes)
- ✅ **Before git commits** (mandatory)
- ✅ **After bug fixes** (to prevent regression)
- ✅ **Before deployment** (all tests must pass)
- ✅ **In CI/CD pipelines** (automated validation)

---

## Examples of "Comprehensive" Tests

### Example 1: Simple Pure Function

```python
# module.py
def add_numbers(a: int, b: int) -> Result[int, str]:
    if not isinstance(a, int) or not isinstance(b, int):
        return Failure("Both arguments must be integers")
    return Success(a + b)

# test_module.py (COMPREHENSIVE = 4 tests)
def test_add_numbers_positive():
    result = add_numbers(5, 3)
    assert isinstance(result, Success)
    assert result.unwrap() == 8

def test_add_numbers_negative():
    result = add_numbers(-5, 3)
    assert isinstance(result, Success)
    assert result.unwrap() == -2

def test_add_numbers_zero():
    result = add_numbers(0, 0)
    assert isinstance(result, Success)
    assert result.unwrap() == 0

def test_add_numbers_invalid_types():
    result = add_numbers("5", 3)
    assert isinstance(result, Failure)
    assert "must be integers" in result.failure()
```

### Example 2: ADT with Pattern Matching

```python
# types.py
from dataclasses import dataclass
from typing import Literal

@dataclass(frozen=True)
class Success:
    value: int

@dataclass(frozen=True)
class Failure:
    error: str

Result = Success | Failure

def process(result: Result) -> str:
    match result:
        case Success(value):
            return f"Got {value}"
        case Failure(error):
            return f"Error: {error}"

# test_types.py (COMPREHENSIVE = 2 tests, one per variant)
def test_process_success():
    result = Success(42)
    output = process(result)
    assert output == "Got 42"

def test_process_failure():
    result = Failure("Something went wrong")
    output = process(result)
    assert output == "Error: Something went wrong"
```

### Example 3: Complex Integration Function

```python
# orchestrator.py
def orchestrate_workflow(request: dict) -> Result[dict, str]:
    # Step 1: Parse request
    parsed = parse_request(request)
    if isinstance(parsed, Failure):
        return parsed
    
    # Step 2: Fetch data
    data = fetch_data(parsed.unwrap().user_id)
    if isinstance(data, Failure):
        return data
    
    # Step 3: Process
    processed = process_data(data.unwrap())
    return processed

# test_orchestrator.py (COMPREHENSIVE = 6 tests)
def test_orchestrate_workflow_success():
    """Happy path: everything works"""
    request = {"user_id": "123", "action": "process"}
    result = orchestrate_workflow(request)
    assert isinstance(result, Success)

def test_orchestrate_workflow_invalid_request():
    """Error path: bad request"""
    request = {}  # Missing fields
    result = orchestrate_workflow(request)
    assert isinstance(result, Failure)
    assert "missing" in result.failure().lower()

def test_orchestrate_workflow_user_not_found():
    """Error path: user doesn't exist"""
    request = {"user_id": "999", "action": "process"}
    result = orchestrate_workflow(request)
    assert isinstance(result, Failure)
    assert "not found" in result.failure().lower()

def test_orchestrate_workflow_processing_error():
    """Error path: processing fails"""
    request = {"user_id": "123", "action": "bad_action"}
    result = orchestrate_workflow(request)
    assert isinstance(result, Failure)

def test_orchestrate_workflow_empty_data():
    """Edge case: empty data"""
    request = {"user_id": "empty_user", "action": "process"}
    result = orchestrate_workflow(request)
    # Define expected behavior

def test_orchestrate_workflow_large_dataset():
    """Edge case: large dataset"""
    request = {"user_id": "big_user", "action": "process"}
    result = orchestrate_workflow(request)
    # Verify performance or chunking
```

---

## Cursor Integration

When working with Cursor:

1. **Before committing**, Cursor will (should):
   - ✅ Run tests automatically
   - ✅ Show test results
   - ✅ Prevent commit if tests fail

2. **During development**, Cursor can:
   - ✅ Generate test scaffolding
   - ✅ Suggest test cases
   - ✅ Run tests in background

3. **User responsibility**:
   - ✅ Review generated tests
   - ✅ Add additional test cases
   - ✅ Ensure tests are meaningful

---

## Final Answer to "What is the rule here?"

**The Rule**:

> **ALL production code MUST have comprehensive tests that pass before committing.**
>
> "Comprehensive" means:
> - ✅ Happy path (primary use case)
> - ✅ Error cases (failure scenarios)
> - ✅ Edge cases (boundaries, empty inputs, etc.)
> - ✅ Minimum 3 tests for simple functions, more for complex ones
> - ✅ 80%+ coverage for business logic modules
>
> **Mandatory**:
> - Use `pytest` (Python), appropriate framework for other languages
> - Use `uv run pytest` for Python
> - Tests MUST pass before committing
> - Cloud Functions: Use `sys.path.append()` in tests
> - Test `Result[T, E]` types (Success/Failure cases)
>
> **Incremental Migration** (existing code):
> - Add tests when touching old code
> - Small changes, always with tests
> - Monitor and review for gradual improvement

---

**Status**: Testing rule fully clarified, ready to incorporate into global rule set

**Next**: Add testing rule section to CURSOR.md (Phase 1 implementation)

