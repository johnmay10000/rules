---
title: Python Functional Programming Libraries Guide
language: python
category: code_guidelines
type: libraries
applies_to: [cursor, kimi, claude, gemini]
version: 1.0.0
last_updated: 2025-11-19
---

# Python Functional Programming Libraries Guide

This guide covers the essential libraries for functional programming in Python, their use cases, and best practices.

---

## Core FP Libraries

### 1. `returns` - Monads and Railway-Oriented Programming

**Purpose**: Provides monads (Result, Maybe, IO), railway-oriented programming, and functional composition tools.

**Installation**:
```bash
pip install returns
```

**Key Types**:
- `Result[T, E]` - Success or Failure
- `Maybe[T]` - Some value or Nothing
- `IO[T]` - Explicit side effects
- `IOResult[T, E]` - IO operations that can fail

**When to Use**:
- Error handling without exceptions
- Chaining operations that can fail
- Explicit side effect management

**Example**:
```python
from returns.result import Result, Success, Failure
from returns.pipeline import flow, bind

def load_user(user_id: int) -> Result[User, LoadError]:
    # Might fail
    ...

def validate_user(user: User) -> Result[User, ValidationError]:
    # Might fail
    ...

def process_user(user_id: int) -> Result[User, Exception]:
    return flow(
        user_id,
        load_user,
        bind(validate_user)
    )
```

**Best Practices**:
- Use `@safe` decorator to convert exceptions to Result
- Prefer `bind` for chaining Result-returning functions
- Use `map` for transforming success values
- Use `fix` for error recovery

---

### 2. `toolz` / `cytoolz` - Functional Utilities

**Purpose**: Provides functional programming utilities like composition, currying, and higher-order functions.

**Installation**:
```bash
pip install toolz
# Or for better performance:
pip install cytoolz
```

**Key Functions**:
- `compose` - Right-to-left function composition
- `pipe` - Left-to-right function composition
- `curry` - Automatic currying
- `memoize` - Function result caching
- `thread_first` / `thread_last` - Threading macros

**When to Use**:
- Building function pipelines
- Creating reusable function compositions
- Data transformation pipelines
- Performance-critical operations (use cytoolz)

**Example**:
```python
from toolz import compose, pipe, curry

# Composition
preprocess = compose(
    normalize,
    augment,
    load_data
)

# Pipe (left-to-right)
result = pipe(
    data,
    load,
    clean,
    transform
)

# Currying
@curry
def add(x, y):
    return x + y

add5 = add(5)  # Partial application
```

**Best Practices**:
- Use `compose` for building pipelines from small functions
- Prefer `pipe` when the order of operations is more readable left-to-right
- Use `curry` for functions that will be partially applied
- Use `memoize` for expensive pure functions

---

### 3. `attrs` - Immutable Data Structures

**Purpose**: Simplifies creation of immutable data classes with validation and type safety.

**Installation**:
```bash
pip install attrs
```

**Key Features**:
- Immutable by default (`frozen=True`)
- Type validation
- Automatic `__init__`, `__repr__`, `__eq__`
- Conversion and validation hooks

**When to Use**:
- Domain models
- Configuration objects
- State management
- Any immutable data structure

**Example**:
```python
from attrs import frozen

@frozen
class ModelConfig:
    """Immutable configuration"""
    learning_rate: float
    batch_size: int
    epochs: int
    
    def with_learning_rate(self, lr: float) -> 'ModelConfig':
        """Return new instance with updated value"""
        return ModelConfig(
            learning_rate=lr,
            batch_size=self.batch_size,
            epochs=self.epochs
        )

# Usage
config = ModelConfig(0.001, 32, 100)
new_config = config.with_learning_rate(0.002)
# config unchanged (immutable)
```

**Best Practices**:
- Always use `frozen=True` for immutability
- Use `field()` for complex validation
- Implement `with_*` methods for "updates" (returns new instance)
- Use `validators` for runtime type checking

---

### 4. `mypy` - Static Type Checking

**Purpose**: Static type checker for Python that enforces type safety.

**Installation**:
```bash
pip install mypy
```

**Key Features**:
- Static type checking
- Type inference
- Strict mode for maximum safety
- Plugin system for libraries

**When to Use**:
- All production code
- Any project using type hints
- FP projects (essential)

**Configuration** (pyproject.toml):
```toml
[tool.mypy]
strict = true
warn_return_any = true
disallow_untyped_defs = true
disallow_incomplete_defs = true
check_untyped_defs = true
disallow_untyped_decorators = true
no_implicit_optional = true
warn_redundant_casts = true
warn_unused_ignores = true
warn_no_return = true
warn_unreachable = true
```

**Best Practices**:
- Always use `strict = true`
- Fix all mypy errors before committing
- Use `typing.TYPE_CHECKING` for import cycles
- Leverage `Protocol` for structural subtyping
- Use `Generic` for type-safe containers

---

### 5. `pyserde` - Serialization for Immutable Objects

**Purpose**: Type-safe serialization/deserialization for dataclasses and attrs classes.

**Installation**:
```bash
pip install pyserde
```

**Key Features**:
- JSON, YAML, TOML, MessagePack support
- Type-safe serialization
- Works with attrs and dataclasses
- Custom serializers

**When to Use**:
- Saving/loading configuration
- API request/response handling
- Data persistence
- Inter-service communication

**Example**:
```python
from attrs import frozen
from serde import serialize, deserialize

@serialize
@deserialize
@frozen
class AppConfig:
    api_key: str
    max_retries: int
    timeout: float

# Serialize
config = AppConfig("key123", 3, 30.0)
json_str = config.to_json()

# Deserialize
loaded = AppConfig.from_json(json_str)
```

**Best Practices**:
- Use with immutable data structures
- Validate after deserialization
- Use custom serializers for complex types
- Handle serialization errors with Result types

---

## Machine Learning & Data Science Libraries

### 6. `numpy` - Numerical Computing

**FP Considerations**:
- Arrays are mutable (be careful!)
- Use immutable patterns where possible
- Prefer pure functions that return new arrays

**FP Patterns**:
```python
import numpy as np

# Immutable operations (return new arrays)
def normalize(data: np.ndarray) -> np.ndarray:
    """Pure function: returns normalized copy"""
    return (data - data.mean()) / data.std()

# Bad: Mutating operations
def normalize_in_place(data: np.ndarray) -> None:
    """Impure: mutates input"""
    data[:] = (data - data.mean()) / data.std()  # Don't do this!
```

---

### 7. `pandas` - Data Manipulation

**FP Considerations**:
- DataFrames are mutable
- Chain operations to create new DataFrames
- Avoid in-place modifications

**FP Patterns**:
```python
import pandas as pd

# Good: Method chaining (functional style)
result = (df
    .filter(['col1', 'col2'])
    .assign(new_col=lambda x: x.col1 * 2)
    .groupby('col2')
    .agg({'new_col': 'mean'})
    .reset_index()
)

# Bad: In-place modifications
df['new_col'] = df.col1 * 2  # Mutates original
```

---

### 8. `polars` - Modern DataFrame Library (FP-Friendly)

**Purpose**: High-performance DataFrame library with immutable data structures.

**Installation**:
```bash
pip install polars
```

**FP Benefits**:
- Immutable DataFrames by default
- Expression API (functional)
- Lazy evaluation
- Type-safe operations

**Example**:
```python
import polars as pl

# Lazy, immutable pipeline
result = (
    pl.scan_csv("data.csv")
    .filter(pl.col("value") > 0)
    .with_columns(
        (pl.col("value") * 2).alias("doubled")
    )
    .groupby("category")
    .agg([
        pl.col("doubled").mean().alias("avg_doubled"),
        pl.col("value").count().alias("count")
    ])
    .collect()
)
```

**Best Practices**:
- Use lazy API for large datasets
- Chain operations for readability
- Leverage expression API
- Use `select` instead of column assignment

---

## Testing Libraries

### 9. `pytest` - Testing Framework

**FP Considerations**:
- Test pure functions easily
- Property-based testing with `hypothesis`
- Fixtures for test setup

**FP Patterns**:
```python
import pytest
from returns.result import Success, Failure

def test_pure_function():
+    """Pure functions: same input → same output"""
+    result = add(2, 3)
+    assert result == 5
+    
+    # Idempotent
+    assert add(2, 3) == add(2, 3)

+def test_result_type():
+    """Test error handling with Result"""
+    result = divide(10, 2)
+    assert result == Success(5)
+    
+    result = divide(10, 0)
+    assert isinstance(result, Failure)

+def test_immutability():
+    """Test that functions don't mutate inputs"""
+    original = [1, 2, 3]
+    result = add_to_all(original, 1)
+    
+    assert original == [1, 2, 3]  # Unchanged
+    assert result == [2, 3, 4]    # New list
```

---

### 10. `hypothesis` - Property-Based Testing

**Purpose**: Test properties that should hold for all inputs.

**Installation**:
```bash
pip install hypothesis
```

**Example**:
```python
from hypothesis import given, strategies as st
from returns.result import Result

+@given(st.integers(), st.integers().filter(lambda x: x != 0))
+def test_divide_properties(a: int, b: int):
+    """Property: division should be inverse of multiplication"""
+    result = divide(a, b)
+    assert isinstance(result, Result)
+    if isinstance(result, Success):
+        assert abs(result.unwrap() * b - a) < 0.0001
```

---

## Library Integration Patterns

### Combining `returns` and `toolz`

```python
from returns.result import Result
+from toolz import compose, curry
+
+@curry
+def safe_divide(a: float, b: float) -> Result[float, str]:
+    if b == 0:
+        return Failure("Division by zero")
+    return Success(a / b)
+
+# Partial application + composition
+divide_by_2 = safe_divide(b=2)  # Returns function: float -> Result
+pipeline = compose(
+    divide_by_2,
+    lambda r: r.map(lambda x: x * 3)
+)
+```

### Using `attrs` with `returns`

```python
from attrs import frozen
+from returns.result import Result, Success
+
+@frozen
+class Config:
+    value: int
+    
+    def validate(self) -> Result['Config', str]:
+        if self.value <= 0:
+            return Failure("Value must be positive")
+        return Success(self)
+
+# Immutable validation
+config = Config(10)
+validated = config.validate()  # Success(Config(10))
+```

---

## Library Selection Guide

| Use Case | Primary Library | Supporting Libraries |
+|----------|----------------|---------------------|
+| Error handling | `returns` | - |
+| Function composition | `toolz` | `returns.pipeline` |
+| Immutable data | `attrs` | `dataclasses` |
+| Type safety | `mypy` | `returns` |
+| Data pipelines | `polars` | `toolz` |
+| ML models | `polars` + `returns` | `attrs` |
+| Serialization | `pyserde` | `attrs` |
+| Testing | `pytest` | `hypothesis` |

---

## Version Compatibility

All libraries support Python 3.8+:

```toml
# pyproject.toml dependencies
+[tool.poetry.dependencies]
+python = "^3.8"
+returns = "^0.22.0"
+toolz = "^0.12.0"
+attrs = "^23.0.0"
+mypy = "^1.5.0"
+polars = "^0.19.0"
+pyserde = "^0.12.0"
+
+[tool.poetry.group.dev.dependencies]
+pytest = "^7.4.0"
+hypothesis = "^6.84.0"
+```

---

## Further Reading

- **Core FP Concepts**: `code_guidelines/principles/functional_programming.md`
- **Python Patterns**: `code_guidelines/languages/python/patterns.md`
- **Code Examples**: `code_guidelines/languages/python/examples.md`

---

**Last Updated**: 2025-11-19  
**Maintained By**: Global AI Rules System  
**Status**: Active  
**Applies To**: All AI assistants (Cursor, Kimi, Claude, Gemini)