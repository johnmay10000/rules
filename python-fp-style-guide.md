# Python Functional Programming Style Guide
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

This guide transforms Python into a strongly typed, pure functional language similar to Haskell while working within Python's constraints.
