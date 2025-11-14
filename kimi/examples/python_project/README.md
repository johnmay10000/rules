# Python FP Example: ML Pipeline with Returns Library

**Project Type**: Machine Learning Data Processing Pipeline
**Language**: Python 3.9+
**FP Library**: `returns` (https://returns.readthedocs.io/)
** demonstrates**: Railway-oriented programming, error handling, monadic composition

---

## 📋 Project Overview

This example demonstrates functional programming patterns in Python using the `returns` library for a machine learning data processing pipeline. The pipeline follows railway-oriented programming principles where data flows through a series of transformations, with explicit error handling at each stage.

### Key FP Concepts Demonstrated

- **Result types**: `Success` and `Failure` instead of exceptions
- **Maybe/Option**: Handling optional/missing values
- **Railway-oriented programming**: Composing functions with `pipe` and `flow`
- **Immutability**: Data transformations return new objects
- **Type safety**: Using Python type hints throughout

---

## 🏗️ Project Structure

```
python_project/
├── README.md                           # This file
├── requirements.txt                    # Dependencies
├── ARCHITECTURE_PLAN.md               # Tier 1: Strategic overview
├── docs/
│   ├── plans/
│   │   ├── PIPELINE_DESIGN.md         # Tier 2: Technical design
│   │   └── IMPLEMENTATION_TODO.md     # Tier 2: Task tracking
│   └── 2025_11_14/                    # Tier 3: Execution logs
│       ├── 20251114_0001_DATA_LOADING.md
│       ├── 20251114_0002_TRANSFORMATION.md
│       └── 20251114_0003_MODEL_TRAINING.md
├── src/
│   ├── __init__.py
│   ├── pipeline.py                     # Main pipeline orchestration
│   ├── data_loader.py                  # Data loading with validation
│   ├── transformer.py                  # Data transformation functions
│   └── trainer.py                      # Model training with Result types
└── tests/
    ├── test_pipeline.py
    ├── test_data_loader.py
    └── test_transformer.py
```

---

## 🚀 Quick Start

### Installation

```bash
# Create virtual environment
python -m venv .venv
source .venv/bin/activate  # On Windows: .venv\Scripts\activate

# Install dependencies
pip install -r requirements.txt
```

### Running the Pipeline

```bash
# Run complete pipeline
python -m src.pipeline --config configs/default.yaml

# Run with custom data
python -m src.pipeline --input data/custom.csv --output results/
```

### Running Tests

```bash
# Run all tests
pytest tests/ -v

# Run with coverage
pytest tests/ --cov=src --cov-report=html
```

---

## 💡 FP Patterns Demonstrated

### 1. Result Type for Error Handling

Instead of exceptions, functions return `Result` types:

```python
from returns.result import Result, Success, Failure
from returns.pipeline import flow
from returns.pointfree import bind

def load_data(filepath: str) -> Result[pd.DataFrame, str]:
    """Load data with explicit error handling."""
    try:
        df = pd.read_csv(filepath)
        return Success(df) if not df.empty else Failure("Empty dataset")
    except Exception as e:
        return Failure(str(e))

def validate_data(df: pd.DataFrame) -> Result[pd.DataFrame, str]:
    """Validate data quality."""
    if df.isnull().sum().sum() > 0:
        return Failure("Dataset contains null values")
    return Success(df)

# Compose with railway pattern
result = flow(
    load_data("data/input.csv"),
    bind(validate_data),
    bind(clean_data),
    bind(train_model),
)
```

### 2. Railway-Oriented Programming

Pipeline follows success/failure tracks:

```python
from returns.pipeline import pipe

def create_pipeline() -> Callable[[str], Result[Model, str]]:
    """Create a composed pipeline function."""
    return pipe(
        load_data,
        validate_data,
        handle_missing_values,
        engineer_features,
        split_dataset,
        train_model,
        evaluate_model,
    )

# Execute pipeline
pipeline = create_pipeline()
result = pipeline("data/train.csv")

# Handle result
match result:
    case Success(model):
        print(f"Model trained successfully: {model.metrics}")
    case Failure(error):
        print(f"Pipeline failed: {error}")
```

### 3. Maybe Type for Optional Values

Handling potentially missing data:

```python
from returns.maybe import Maybe, Some, Nothing

def get_config_value(config: Dict, key: str) -> Maybe[Any]:
    """Safely retrieve configuration value."""
    return Some(config[key]) if key in config else Nothing

# Use in pipeline
batch_size = get_config_value(config, "batch_size").value_or(32)
```

### 4. Immutable Data Transformations

All transformations return new objects:

```python
from typing import Final

@dataclass(frozen=True)  # Immutable
class TransformConfig:
    scaling_method: Final[str] = "standard"
    handle_outliers: Final[bool] = True

def scale_features(
    df: pd.DataFrame, 
    config: TransformConfig
) -> Result[pd.DataFrame, str]:
    """Scale features immutably - returns new DataFrame."""
    try:
        scaler = StandardScaler() if config.scaling_method == "standard" else MinMaxScaler()
        scaled_data = scaler.fit_transform(df)
        return Success(pd.DataFrame(scaled_data, columns=df.columns))
    except Exception as e:
        return Failure(f"Scaling failed: {str(e)}")
```

### 5. Container Types for Context

Using `Reader` for dependency injection:

```python
from returns.context import RequiresContext

Context = Dict[str, Any]

def log_step(step_name: str) -> RequiresContext[Context, None]:
    """Log pipeline step with context."""
    def factory(ctx: Context) -> None:
        logger = ctx["logger"]
        logger.info(f"Executing step: {step_name}")
    return RequiresContext(factory)
def process_with_logging(
    df: pd.DataFrame
) -> RequiresContext[Context, Result[pd.DataFrame, str]]:
    """Processing step with logging context."""
    return RequiresContext(
        lambda ctx: flow(
            validate_data(df),
            bind(lambda data: tap(log_step("validation")) and Success(data)),
            bind(transform_data),
            bind(lambda data: tap(log_step("transformation")) and Success(data)),
        )
    )

# Execute with context
context = {"logger": logging.getLogger(__name__)}
result = process_with_logging(df)(context)
```

---

## 📊 Pipeline Architecture

```
Input CSV
    ↓
[load_data] → Result[DataFrame, Error]
    ↓
[validate_data] → Result[DataFrame, Error]
    ↓
[handle_missing] → Result[DataFrame, Error]
    ↓
[engineer_features] → Result[DataFrame, Error]
    ↓
[split_data] → Result[TrainTestSplit, Error]
    ↓
[train_model] → Result[Model, Error]
    ↓
[evaluate_model] → Result[Metrics, Error]
    ↓
Output: Model + Metrics or Error Trace
```

**Key Characteristics:**
- Each step returns `Result[SuccessType, ErrorType]`
- Short-circuit on failure (railway pattern)
- Explicit error messages at each stage
- Type-safe throughout pipeline
- Immutable data transformations

---

## 🔧 Kimi-Specific Integration

### Using SetTodoList for Pipeline Development

```python
# Kimi workflow example

def create_todo_for_pipeline_implementation():
    """SetTodoList structure for ML pipeline."""
    return {
        "title": "ML Pipeline with Returns Library",
        "tasks": [
            {"title": "TASK-1.1: Setup project structure", "status": "Done"},
            {"title": "TASK-1.2: Create requirements.txt", "status": "Done"},
            {"title": "TASK-1.3: Implement data_loader.py with Result types", "status": "In Progress"},
            {"title": "TASK-1.4: Implement transformer.py with railway pattern", "status": "Pending"},
            {"title": "TASK-1.5: Implement trainer.py with error handling", "status": "Pending"},
            {"title": "TASK-1.6: Create pipeline.py orchestration", "status": "Pending"},
            {"title": "TASK-1.7: Write comprehensive tests", "status": "Pending"},
        ]
    }
```

### Parallel Validation

Kimi can validate multiple pipeline components simultaneously:

```bash
# Read multiple files at once for review
ReadFile: src/data_loader.py
ReadFile: src/transformer.py
ReadFile: src/trainer.py
ReadFile: src/pipeline.py

# Run tests in parallel
cd src && python -m pytest tests/test_data_loader.py -v &
cd src && python -m pytest tests/test_transformer.py -v &
cd src && python -m pytest tests/test_trainer.py -v &
wait
```

---

## 🧪 Testing Strategy

### Unit Tests with Result Types

```python
import pytest
from returns.result import Success, Failure

def test_load_data_success(tmp_path):
    """Test successful data loading."""
    # Setup
    test_file = tmp_path / "test.csv"
    test_file.write_text("a,b,c\n1,2,3\n4,5,6")
    
    # Execute
    result = load_data(str(test_file))
    
    # Verify
    assert isinstance(result, Success)
    assert len(result.unwrap()) == 2

def test_load_data_file_not_found():
    """Test error handling for missing file."""
    result = load_data("nonexistent.csv")
    assert isinstance(result, Failure)
    assert "not found" in result.failure()

def test_validate_data_with_nulls():
    """Test validation catches null values."""
    df = pd.DataFrame({"a": [1, 2, None], "b": [4, 5, 6]})
    result = validate_data(df)
    assert isinstance(result, Failure)
    assert "null values" in result.failure()
```

---

## 📈 Benefits of FP Approach

### Compared to Traditional Python ML Code:

| Aspect | Traditional | FP with Returns |
|--------|-------------|-----------------|
| Error handling | Exceptions thrown | Errors as values |
| Error trace | Stack trace | Explicit error path |
| Pipeline composition | Imperative steps | Railway composition |
| Type safety | Runtime checks | Static hints + runtime |
| Testability | Mock exceptions | Test Result branches |
| Debugging | Try/catch | Pattern matching |

---

## 🎯 Real-World Scenarios

### Scenario 1: Missing Configuration
```python
# Graceful degradation
result = flow(
    load_config(env),
    bind(lambda cfg: Success(cfg) if "model_type" in cfg else Failure("Missing model_type")),
    bind(lambda cfg: load_data(cfg.get("data_path"))),
    # ... rest of pipeline
)
```

### Scenario 2: Partial Data Processing
```python
# Continue with warnings instead of failing
result = pipeline.map(lambda r: r.alt(lambda e: log_warning(e) or r))
```

### Scenario 3: Retry Logic
```python
from functools import partial

def retry(operation, max_attempts=3):
    """Retry failed operations."""
    def wrapper(*args, **kwargs):
        for attempt in range(max_attempts):
            result = operation(*args, **kwargs)
            if isinstance(result, Success):
                return result
            if attempt == max_attempts - 1:
                return result
        return result
    return wrapper

# Use with pipeline
safe_load = retry(load_data, max_attempts=3)
result = safe_load("data.csv")
```

---

## 🔗 Cross-References

**Project Documentation:**
- [ARCHITECTURE_PLAN.md](./ARCHITECTURE_PLAN.md) - Strategic overview
- [docs/plans/PIPELINE_DESIGN.md](./docs/plans/PIPELINE_DESIGN.md) - Technical design
- [docs/plans/IMPLEMENTATION_TODO.md](./docs/plans/IMPLEMENTATION_TODO.md) - Task tracking

**Kimi Guides:**
- [kimi/KIMI_WORKFLOW_GUIDE.md](../../KIMI_WORKFLOW_GUIDE.md) - Workflow patterns
- [kimi/python-fp-style-guide.md](../../kimi/python-fp-style-guide.md) - Python FP patterns
- [kimi/DATA_STRUCTURE_PATTERNS.md](../../kimi/DATA_STRUCTURE_PATTERNS.md) - FP data structures

---

## ✅ Completion Checklist

This example is complete when:
- [ ] All source files implemented with Result types
- [ ] Comprehensive tests written and passing
- [ ] Documentation hierarchy established (3 tiers)
- [ ] SetTodoList usage demonstrated
- [ ] Parallel validation examples created
- [ ] Cross-references functional
- [ ] Follows KIMI.md conventions
- [ ] Git checkpoint with completion summary

---

**Status**: 🔄 IN PROGRESS (baseline established)
**Last Updated**: 2025-11-14 19:45
**Maintained By**: Kimi CLI Global Rules System

🤖 Generated with [Kimi](https://kimi.ai)

Co-Authored-By: Kimi <noreply@kimi.ai>

---

*This is a functional Python ML pipeline example using the returns library for functional programming patterns. See [kimi/KIMI_WORKFLOW_GUIDE.md](../../kimi/KIMI_WORKFLOW_GUIDE.md) for Kimi-specific usage patterns.*
