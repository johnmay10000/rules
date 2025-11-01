# Python Functional Programming Style Guide

**Version**: 2.0.0  
**Last Updated**: 2025-10-31  
**Part of**: [CURSOR.md](CURSOR.md) Global Rule Set  
**Target**: Python projects (ML, backend, data processing)

> **📖 Global Rules**: This document extends [CURSOR.md](CURSOR.md) with Python-specific guidance. For mandatory universal rules (Git, documentation, testing, file size), see [CURSOR.md](CURSOR.md).

---

## Quick Links

- **Mandatory Rules**: See [CURSOR.md](CURSOR.md) sections 1-4
- **FP Principles Deep Dive**: See [CURSOR_FP_PRINCIPLES.md](CURSOR_FP_PRINCIPLES.md)
- **Workflow Guide**: See [CURSOR_WORKFLOW_GUIDE.md](CURSOR_WORKFLOW_GUIDE.md)
- **Integration**: See [.cursorrules Integration](#cursorrules-integration) below

---

## For MLX and Machine Learning Projects

### Core Principles

1. **Immutability by Default**: Use `dataclasses(frozen=True)` or `attrs(frozen=True)`
2. **Pure Functions**: Functions should have no side effects unless explicitly marked
3. **Composability**: Build complex operations from small, composable functions
4. **Explicit Error Handling**: Use `Result`/`Either` types instead of exceptions
5. **Type Safety**: Leverage Python's type system with strict mypy checking

---

## Required Libraries

```python
# Core FP library - provides monads, Either, Result, etc.
from returns.result import Result, Success, Failure
from returns.maybe import Maybe, Some, Nothing
from returns.io import IO, IOResult
from returns.pipeline import flow, pipe
from returns.pointfree import bind, map_
from returns.curry import curry, partial

# For functional utilities
from toolz import compose, pipe as toolz_pipe
from toolz.curried import map, filter, reduce

# Type hints
from typing import Callable, TypeVar, ParamSpec, Generic
```

---

## 1. Error Handling with Result/Either

### ❌ Avoid: Try/Catch
```python
def divide(a: float, b: float) -> float:
    try:
        return a / b
    except ZeroDivisionError:
        raise ValueError("Cannot divide by zero")
```

### ✅ Prefer: Result Type
```python
from returns.result import Result, Success, Failure, safe

@safe
def divide(a: float, b: float) -> float:
    if b == 0:
        raise ValueError("Cannot divide by zero")
    return a / b

# Or explicitly:
def divide_explicit(a: float, b: float) -> Result[float, str]:
    if b == 0:
        return Failure("Cannot divide by zero")
    return Success(a / b)
```

---

## 2. Monadic Composition (Do Notation Style)

### ✅ Chain Operations with bind/flat_map
```python
from returns.result import Result, Success, Failure
from returns.pipeline import flow

def validate_positive(x: float) -> Result[float, str]:
    return Success(x) if x > 0 else Failure("Must be positive")

def sqrt_safe(x: float) -> Result[float, str]:
    return Success(x ** 0.5)

def reciprocal(x: float) -> Result[float, str]:
    if x == 0:
        return Failure("Cannot take reciprocal of zero")
    return Success(1 / x)

# Monadic composition - like Haskell's do notation
result = (
    validate_positive(16.0)
    .bind(sqrt_safe)        # 4.0
    .bind(reciprocal)       # 0.25
    .map(lambda x: x * 100) # 25.0
)

# Alternative using flow for point-free style
def process_value(x: float) -> Result[float, str]:
    return flow(
        validate_positive,
        lambda r: r.bind(sqrt_safe),
        lambda r: r.bind(reciprocal),
        lambda r: r.map(lambda v: v * 100),
    )(x)
```

---

## 3. Currying and Partial Application

### ✅ Curry Functions for Composition
```python
from returns.curry import curry

@curry
def scale_tensor(factor: float, offset: float, tensor: Array) -> Array:
    return (tensor * factor) + offset

# Partially apply to create specialized functions
normalize = scale_tensor(2.0)(0.0)
center = scale_tensor(1.0)(-0.5)

# Compose them
preprocess = compose(center, normalize)
```

---

## 4. Function Composition

### ✅ Build Complex Pipelines
```python
from returns.pipeline import flow, pipe
from toolz import compose

# Example: Data preprocessing pipeline
def load_data(path: str) -> Result[Array, str]:
    ...

def normalize(arr: Array) -> Array:
    return (arr - arr.mean()) / arr.std()

def augment(arr: Array) -> Array:
    ...

def to_tensor(arr: Array) -> Tensor:
    ...

# Using flow (left-to-right)
pipeline = lambda path: flow(
    load_data(path),
    lambda r: r.map(normalize),
    lambda r: r.map(augment),
    lambda r: r.map(to_tensor),
)

# Using compose (right-to-left, more Haskell-like)
pure_pipeline = compose(to_tensor, augment, normalize)
```

---

## 5. Immutable Data Structures

### ✅ Use Frozen Dataclasses
```python
from dataclasses import dataclass
from typing import List
import mlx.core as mx

@dataclass(frozen=True)
class ModelConfig:
    hidden_size: int
    num_layers: int
    dropout: float
    
    def with_dropout(self, new_dropout: float) -> 'ModelConfig':
        """Return new instance with modified dropout"""
        return ModelConfig(
            hidden_size=self.hidden_size,
            num_layers=self.num_layers,
            dropout=new_dropout,
        )

@dataclass(frozen=True)
class TrainingState:
    model_weights: mx.array
    optimizer_state: dict
    step: int
    
    def next_step(self) -> 'TrainingState':
        return TrainingState(
            model_weights=self.model_weights,
            optimizer_state=self.optimizer_state,
            step=self.step + 1,
        )
```

---

## 6. Railway-Oriented Programming

### ✅ Handle Multiple Failure Modes
```python
from returns.result import Result, Success, Failure
from returns.pipeline import flow

def load_model(path: str) -> Result[Model, str]:
    ...

def validate_model(model: Model) -> Result[Model, str]:
    if not model.is_valid():
        return Failure("Model validation failed")
    return Success(model)

def compile_model(model: Model) -> Result[Model, str]:
    ...

def initialize_weights(model: Model) -> Result[Model, str]:
    ...

# All steps happen only if previous succeeded
def setup_model(path: str) -> Result[Model, str]:
    return (
        load_model(path)
        .bind(validate_model)
        .bind(compile_model)
        .bind(initialize_weights)
    )

# Pattern match on result
match setup_model("model.safetensors"):
    case Success(model):
        print(f"Model ready: {model}")
    case Failure(error):
        print(f"Setup failed: {error}")
```

---

## 7. IO and Side Effects

### ✅ Mark Side Effects Explicitly
```python
from returns.io import IO, IOResult
from returns.result import Result, Success

def pure_computation(x: int) -> int:
    """Pure function - no IO"""
    return x * 2

def read_file(path: str) -> IO[str]:
    """IO operation - explicitly marked"""
    return IO(lambda: open(path).read())

def save_model_io(model: Model, path: str) -> IO[None]:
    """Side effect wrapped in IO"""
    def _save():
        model.save(path)
        return None
    return IO(_save)

# Combine IO with Result for fallible IO
def load_config(path: str) -> IOResult[Config, str]:
    """IO operation that can fail"""
    def _load():
        try:
            with open(path) as f:
                return Success(parse_config(f.read()))
        except FileNotFoundError:
            return Failure(f"Config not found: {path}")
    return IOResult(IO(_load))
```

---

## 8. Higher-Order Functions and Abstractions

### ✅ Generic Functional Patterns
```python
from typing import TypeVar, Callable, List
from returns.result import Result, Success

A = TypeVar('A')
B = TypeVar('B')
E = TypeVar('E')

def traverse_result(
    items: List[A],
    f: Callable[[A], Result[B, E]]
) -> Result[List[B], E]:
    """
    Apply f to each item, collect results.
    Fails fast on first error.
    Like Haskell's traverse for Result.
    """
    results: List[B] = []
    for item in items:
        match f(item):
            case Success(value):
                results.append(value)
            case Failure(_) as err:
                return err
    return Success(results)

# Usage
def validate_value(x: int) -> Result[int, str]:
    return Success(x) if x > 0 else Failure(f"{x} is not positive")

values = [1, 2, 3, 4]
result = traverse_result(values, validate_value)  # Success([1, 2, 3, 4])

bad_values = [1, -2, 3]
result = traverse_result(bad_values, validate_value)  # Failure("-2 is not positive")
```

---

## 9. Type-Level Programming

### ✅ Use Protocols for Higher-Kinded Types
```python
from typing import Protocol, TypeVar, Generic, Callable

A = TypeVar('A', contravariant=True)
B = TypeVar('B', covariant=True)
C = TypeVar('C')

class Functor(Protocol[A, B]):
    """Simulates HKT Functor"""
    def map(self, f: Callable[[A], B]) -> 'Functor[A, C]':
        ...

class Monad(Functor[A, B], Protocol):
    """Simulates HKT Monad"""
    def bind(self, f: Callable[[A], 'Monad[B, C]']) -> 'Monad[B, C]':
        ...
    
    @classmethod
    def pure(cls, value: A) -> 'Monad[A, B]':
        ...
```

---

## 10. Pattern Matching for ADTs

### ✅ Use Structural Pattern Matching
```python
from dataclasses import dataclass
from typing import Union
from returns.result import Result, Success, Failure

@dataclass(frozen=True)
class Loading:
    progress: float

@dataclass(frozen=True)
class Loaded:
    data: Array

@dataclass(frozen=True)
class Error:
    message: str

State = Union[Loading, Loaded, Error]

def handle_state(state: State) -> str:
    match state:
        case Loading(progress):
            return f"Loading... {progress:.0%}"
        case Loaded(data):
            return f"Ready: {data.shape}"
        case Error(message):
            return f"Error: {message}"
```

---

## Complete Example: Training Loop

```python
from dataclasses import dataclass
from returns.result import Result, Success, Failure
from returns.pipeline import flow
from returns.curry import curry
import mlx.core as mx

@dataclass(frozen=True)
class TrainConfig:
    learning_rate: float
    batch_size: int
    num_epochs: int

@dataclass(frozen=True)
class TrainState:
    weights: mx.array
    step: int
    loss: float

@curry
def compute_gradients(
    batch: tuple[mx.array, mx.array],
    weights: mx.array
) -> Result[mx.array, str]:
    """Pure gradient computation"""
    x, y = batch
    # ... gradient computation
    return Success(gradients)

@curry
def update_weights(
    learning_rate: float,
    weights: mx.array,
    gradients: mx.array
) -> mx.array:
    """Pure weight update"""
    return weights - learning_rate * gradients

@curry
def train_step(
    config: TrainConfig,
    state: TrainState,
    batch: tuple[mx.array, mx.array]
) -> Result[TrainState, str]:
    """Single training step - pure and composable"""
    return (
        compute_gradients(batch, state.weights)
        .map(update_weights(config.learning_rate, state.weights))
        .map(lambda new_weights: TrainState(
            weights=new_weights,
            step=state.step + 1,
            loss=compute_loss(batch, new_weights),
        ))
    )

def train_epoch(
    config: TrainConfig,
    initial_state: TrainState,
    batches: list[tuple[mx.array, mx.array]]
) -> Result[TrainState, str]:
    """Train for one epoch by folding over batches"""
    def fold_step(
        state_result: Result[TrainState, str],
        batch: tuple[mx.array, mx.array]
    ) -> Result[TrainState, str]:
        return state_result.bind(
            lambda state: train_step(config, state, batch)
        )
    
    from functools import reduce
    return reduce(fold_step, batches, Success(initial_state))
```

---

## Style Rules Summary

1. **No naked try/except**: Wrap in `@safe` or return `Result`
2. **No mutations**: Use frozen dataclasses, return new instances
3. **Explicit effects**: Wrap side effects in `IO` or `IOResult`
4. **Compose, don't nest**: Use `flow`, `pipe`, `compose`, `bind`
5. **Curry when useful**: Enable partial application for reusable components
6. **Type everything**: Use mypy strict mode, leverage type inference
7. **Pattern match**: Use structural pattern matching for ADTs
8. **Small functions**: Each function does one thing, compose for complexity
9. **Railway-oriented**: Chain Results, fail fast, handle at boundaries
10. **Document purity**: Mark pure vs impure functions clearly

---

## File Organization

```
project/
├── types/          # ADTs, type aliases
│   ├── result.py   # Custom Result extensions
│   └── state.py    # State types
├── pure/           # Pure business logic
│   ├── compute.py
│   └── transform.py
├── io/             # IO operations
│   ├── load.py
│   └── save.py
└── pipeline/       # Composed pipelines
    └── training.py
```

---

## Tooling

```toml
# pyproject.toml
[tool.mypy]
strict = true
warn_return_any = true
warn_unused_configs = true

[tool.ruff]
select = ["E", "F", "I", "N", "UP", "RUF"]
```

---

## .cursorrules Integration

### Setup in Your Python Project

**Step 1**: Set up global rules (one-time machine setup)

See [SETUP_GUIDE.md](SETUP_GUIDE.md) for detailed instructions.

Quick setup:
```bash
# Option 1: Environment variable
export CURSOR_RULES_PATH="$HOME/path/to/rules"

# Option 2: Git submodule
git submodule add <rules-repo-url> .cursor-rules
```

**Step 2**: Create `.cursorrules` in your project root:

```markdown
# .cursorrules for Python Project

## Global Rules
@${CURSOR_RULES_PATH}/CURSOR.md
# Or if using submodule: @.cursor-rules/CURSOR.md

## Language-Specific Rules
@${CURSOR_RULES_PATH}/python-fp-style-guide.md

## Project-Specific Overrides

### Tech Stack
- **Language**: Python 3.11+
- **FP Library**: returns
- **Type Checking**: mypy (strict mode)
- **Package Manager**: uv
- **Linting**: ruff, black, isort

### Project Structure
```
src/
├── types/          # ADTs and type definitions
├── pure/           # Pure business logic
├── io/             # IO operations
├── api/            # API endpoints (if applicable)
└── pipeline/       # Composed pipelines
```

### Mandatory for This Project
- All functions must have type hints
- All public functions must return Result types
- No exceptions except at IO boundaries
- 100% test coverage for pure functions
- File size limit: 250 lines
```

---

### Example: Python + GCP Project

```markdown
# .cursorrules for Python + GCP Project

## Global Rules
@${CURSOR_RULES_PATH}/CURSOR.md

## Language Rules
@${CURSOR_RULES_PATH}/python-fp-style-guide.md

## Platform Rules
@${CURSOR_RULES_PATH}/GCP_GUIDELINES.md

## Project Context
- **Platform**: Google Cloud Run Functions
- **Data**: Cloud Storage (GCS)
- **Libraries**: returns, polars, google-cloud-storage

## Testing
- Use `sys.path.append()` for Cloud Functions imports
- Test structure mirrors function structure
- All tests must pass before commit
```

---

### Auto-Detection Example

If using the smart template (see [SETUP_GUIDE.md](SETUP_GUIDE.md)):

```markdown
# .cursorrules (auto-detects Python)

@${CURSOR_RULES_PATH}/templates/.cursorrules_smart_template_envvar

# The template will automatically detect:
# - Language: Python (from .py files)
# - FP library: returns (from pyproject.toml)
# - Testing: pytest (from pyproject.toml)
# - Platform: GCP (from dependencies)
```

---

## Quick Reference Card

**Before Every Commit** (from [CURSOR.md](CURSOR.md)):
- [ ] All tests passing (mandatory)
- [ ] Type checks passing (mypy strict)
- [ ] Linters passing (ruff, black, isort)
- [ ] All files < 250 lines (mandatory)
- [ ] Commit message follows template (mandatory)
- [ ] TODO list updated (if applicable)

**Python-Specific Checks**:
- [ ] All functions have type hints
- [ ] Public functions return Result types
- [ ] Dataclasses are frozen
- [ ] No naked exceptions
- [ ] Pattern matching used for ADTs

---

## Universal FP Pattern (Python)

From [CURSOR.md](CURSOR.md) section 5.2:

```python
# Railway-oriented programming
result = (
    Success(data)
    .bind(validate)      # Returns Result
    .bind(transform)     # Returns Result
    .map(format)         # Pure function
)

# Mental model: Factory assembly line
# - Each function = one station
# - Errors stop the line
# - Success continues to next station
```

---

## Data Structure Patterns (Python)

**For Foldable and Traversable patterns in Python**, see:

- **Quick Reference**: [DATA_STRUCTURE_PATTERNS.md](DATA_STRUCTURE_PATTERNS.md#python) - Fast lookup for common patterns
- **Full Guide**: [guides/traversable-foldable-guide.md](guides/traversable-foldable-guide.md#python-implementation) - Comprehensive guide with examples
- **CURSOR.md Section 8**: [Data Structure Guidelines](CURSOR.md#8-data-structure-guidelines-recommended)

### When to Use

✅ **Use Foldable** (reduce/fold) when:
- Aggregating collections: sum, product, concat
- Converting between collection types
- Building accumulations

✅ **Use Traversable** (traverse) when:
- Validating collections with early exit
- Performing effects on collections (IO, async)
- Need "all-or-nothing" semantics
- Parallel operations on collections

### Python Implementation

**Foldable**:
```python
from functools import reduce

# Sum numbers
total = reduce(lambda acc, n: acc + n, numbers, 0)

# Or with toolz
from toolz import reduce
total = reduce(add, numbers, 0)
```

**Traversable**:
```python
from returns.result import Result, Success, Failure

def traverse(items, f):
    """Apply f to each item, short-circuit on first failure"""
    results = []
    for item in items:
        result = f(item)
        if isinstance(result, Failure):
            return result  # Early exit!
        results.append(result.unwrap())
    return Success(results)

# Usage
validated = traverse(numbers, validate_positive)
```

**Parallel Traverse**:
```python
import asyncio

async def parallel_traverse(items, async_f):
    """Execute async operations in parallel"""
    return await asyncio.gather(*[async_f(item) for item in items])

# Usage
users = await parallel_traverse(user_ids, fetch_user)
```

### Common Patterns

**Form Validation** (all fields must pass):
```python
def validate_form(name: str, email: str, age: int) -> Result[User, ValidationError]:
    name_result = validate_name(name)
    if isinstance(name_result, Failure):
        return name_result
    
    email_result = validate_email(email)
    if isinstance(email_result, Failure):
        return email_result
    
    age_result = validate_age(age)
    if isinstance(age_result, Failure):
        return age_result
    
    return Success(User(name, email, age))
```

**ETL Pipeline** (parse → validate → enrich):
```python
from returns.pipeline import flow

def etl_pipeline(raw_data: list[RawRecord]) -> Result[list[EnrichedRecord], Error]:
    return flow(
        raw_data,
        lambda data: traverse(data, parse_record),
        lambda parsed: parsed.bind(lambda p: traverse(p, validate_record)),
        lambda validated: validated.bind(lambda v: traverse(v, enrich_record))
    )
```

See the [full guide](guides/traversable-foldable-guide.md#python-implementation) for comprehensive examples and patterns.

---

## Mandatory Rules Reference

From [CURSOR.md](CURSOR.md):

1. **Git Checkpoints** (Section 1) - Commit every 30-60 min
2. **Documentation** (Section 2) - 3-tier hierarchy
3. **Testing** (Section 3) - Comprehensive coverage, all passing
4. **File Size** (Section 4) - 250-300 lines maximum

See [CURSOR.md](CURSOR.md) for complete details.

---

**Version**: 2.0.0  
**Last Updated**: 2025-10-31  
**Maintained By**: Global Rules Repository

---

This guide transforms Python into a strongly typed, pure functional language similar to Haskell while working within Python's constraints.
