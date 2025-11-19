---
title: Python Functional Programming Style Guide
language: python
category: code_guidelines
type: language
applies_to: [cursor, kimi, claude, gemini]
version: 2.0.0
last_updated: 2025-11-19
---

# Python Functional Programming Style Guide

**Target**: Python projects (ML, backend, data processing, web applications)

> **📖 Universal Rules**: This document provides Python-specific functional programming guidance. For mandatory universal rules (Git, testing, documentation, project structure), see the `universal_rules/` directory.

---

## Quick Links

- **FP Principles Deep Dive**: See `code_guidelines/principles/functional_programming.md`
- **Pattern Reference**: See `code_guidelines/languages/python/patterns.md`
- **Code Examples**: See `code_guidelines/languages/python/examples.md`
- **Library Guide**: See `code_guidelines/languages/python/libraries.md`

---

## Core Principles

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
from typing import Callable, TypeVar, ParamSpec, Generic, List, Dict, Any
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

**Key Benefits**:
- Errors become part of the type signature
- Forces callers to handle errors explicitly
- Composable with other Result-returning functions
- No hidden exception flows

---

## 2. Monadic Composition (Do Notation Style)

### ✅ Chain Operations with bind/flat_map

```python
from returns.result import Result, Success, Failure

def validate_positive(x: float) -> Result[float, str]:
    if x <= 0:
        return Failure("Must be positive")
    return Success(x)

def sqrt_safe(x: float) -> Result[float, str]:
    return Success(x ** 0.5)

def reciprocal(x: float) -> Result[float, str]:
    if x == 0:
        return Failure("Cannot take reciprocal of zero")
    return Success(1 / x)

def process_value(x: float) -> Result[float, str]:
    """Chain operations that can each fail"""
    return flow(
        x,
        validate_positive,
        bind(sqrt_safe),      # Use bind for functions that return Result
        bind(reciprocal)
    )

# Example usage
result = process_value(4.0)  # Success(0.5)
result = process_value(-1.0) # Failure("Must be positive")
result = process_value(0.0)  # Failure("Must be positive")
```

**Pattern**: Each step returns a `Result`. The `bind` function:
- If previous step was `Success`, extracts the value and calls next function
- If previous step was `Failure`, short-circuits and returns the error

---

## 3. Currying and Partial Application

### ✅ Curry Functions for Composition

```python
from returns.curry import curry

@curry
def scale_tensor(factor: float, tensor: Tensor) -> Tensor:
    return tensor * factor

# Partial application
scale_by_2 = scale_tensor(2.0)  # Returns function: Tensor -> Tensor
scale_by_0.5 = scale_tensor(0.5)

# Use in pipelines
result = scale_by_2(tensor)  # Double the tensor
```

**Benefits**:
- Functions become more reusable
- Easy to create specialized versions
- Natural fit for function composition
- Type-safe partial application

---

## 4. Function Composition

### ✅ Build Complex Pipelines

```python
from toolz import compose
from typing import List

# Small, focused functions
def load_data(path: str) -> List[DataPoint]:
    """Pure: Load data from file"""
    ...

def normalize(data: List[DataPoint]) -> List[DataPoint]:
    """Pure: Normalize features"""
    ...

def augment(data: List[DataPoint]) -> List[DataPoint]:
    """Pure: Data augmentation"""
    ...

def to_tensor(data: List[DataPoint]) -> Tensor:
    """Pure: Convert to tensor format"""
    ...

# Compose into pipeline
preprocess_pipeline = compose(
    to_tensor,
    augment,
    normalize,
    load_data
)

# Or using pipe (left-to-right)
def preprocess_with_pipe(path: str) -> Tensor:
    return pipe(
        path,
        load_data,
        normalize,
        augment,
        to_tensor
    )

# Usage
tensor = preprocess_pipeline("./data/train.csv")
```

**Principle**: Complex operations are just compositions of simple functions.

---

## 5. Immutable Data Structures

### ✅ Use Frozen Dataclasses

```python
from dataclasses import dataclass
from typing import Optional

@dataclass(frozen=True)
class ModelConfig:
    """Immutable configuration - cannot be changed after creation"""
    learning_rate: float
    batch_size: int
    dropout_rate: float
    num_epochs: int
    
    def with_dropout(self, rate: float) -> 'ModelConfig':
        """Return new config with updated dropout (immutable update)"""
        return ModelConfig(
            learning_rate=self.learning_rate,
            batch_size=self.batch_size,
            dropout_rate=rate,
            num_epochs=self.num_epochs
        )

# Usage
config = ModelConfig(
    learning_rate=0.001,
    batch_size=32,
    dropout_rate=0.1,
    num_epochs=100
)

# Update creates new instance
new_config = config.with_dropout(0.2)  # config unchanged!
```

### ✅ Immutable State Management

```python
@dataclass(frozen=True)
class TrainingState:
    """Immutable training state"""
    epoch: int
    loss: float
    accuracy: float
    model_weights: Dict[str, Tensor]
    
    def next_step(self, new_loss: float, new_acc: float) -> 'TrainingState':
        """Return new state with updated metrics"""
        return TrainingState(
            epoch=self.epoch + 1,
            loss=new_loss,
            accuracy=new_acc,
            model_weights=self.model_weights  # In practice, these would be updated too
        )

# Training loop with immutable state
def train_epoch(state: TrainingState) -> TrainingState:
    """Pure function: takes state, returns new state"""
    loss, acc = run_training_step(state)
    return state.next_step(loss, acc)

# Fold over epochs (functional style)
final_state = reduce(
    lambda state, _: train_epoch(state),
    range(10),
    initial_state
)
```

---

## 6. Railway-Oriented Programming

### ✅ Handle Multiple Failure Modes

```python
from returns.result import Result, Success, Failure
from typing import Callable

def load_model(path: str) -> Result[Model, LoadError]:
    """Load model from disk"""
    try:
        model = deserialize(path)
        return Success(model)
    except Exception as e:
        return Failure(LoadError(f"Failed to load: {e}"))

def validate_model(model: Model) -> Result[Model, ValidationError]:
    """Validate model architecture"""
    if not model.layers:
        return Failure(ValidationError("Model has no layers"))
    return Success(model)

def compile_model(model: Model) -> Result[CompiledModel, CompileError]:
    """Compile model for inference"""
    try:
        compiled = model.compile()
        return Success(compiled)
    except Exception as e:
        return Failure(CompileError(f"Compilation failed: {e}"))

def initialize_weights(model: CompiledModel) -> Result[InitializedModel, InitError]:
    """Initialize model weights"""
    try:
        initialized = model.initialize()
        return Success(initialized)
    except Exception as e:
        return Failure(InitError(f"Initialization failed: {e}"))

# Railway-oriented pipeline
def setup_model(path: str) -> Result[InitializedModel, Exception]:
    """
    Railway: Either get InitializedModel or the first error encountered
    Each step returns Result, bind handles the railway switching
    """
    return flow(
        path,
        load_model,
        bind(validate_model),
        bind(compile_model),
        bind(initialize_weights)
    )

# Usage
result = setup_model("./model.pkl")
# Success(InitializedModel(...)) or Failure(SomeError)
```

**Visualizing the Railway**:
```
Input → load_model → validate_model → compile_model → initialize_weights → Result
          ↓              ↓                ↓                  ↓
        Success        Success          Success           Success
          ↓              ↓                ↓                  ↓
        Failure ←──────┴───────┴─────────┴─────────────────┘
          ↑
     Short-circuit on first error
```

---

## 7. IO and Side Effects

### ✅ Mark Side Effects Explicitly

```python
from returns.io import IO, IOResult
from returns.result import Result

def pure_computation(x: int, y: int) -> int:
    """Pure function: no side effects, deterministic"""
    return x + y

def read_file(path: str) -> IOResult[str, IOError]:
    """Impure operation: reading from disk"""
    def _read() -> Result[str, IOError]:
        try:
            with open(path, 'r') as f:
                return Success(f.read())
        except IOError as e:
            return Failure(e)
    return IO(_read)

def save_model_io(model: Model, path: str) -> IOResult[None, IOError]:
    """Impure operation: writing to disk"""
    def _save() -> Result[None, IOError]:
        try:
            with open(path, 'wb') as f:
                f.write(serialize(model))
            return Success(None)
        except IOError as e:
            return Failure(e)
    return IO(_save)

def load_config(path: str) -> IOResult[Config, Exception]:
    """IO operation that returns Result"""
    return read_file(path).map(lambda content: parse_json(content))

# Run IO operations at the edge of your program
def main():
    # All pure logic here
    config_result = load_config("./config.json")
    
    # Execute IO at the end
    config_io = config_result.unwrap()
    config = config_io()  # Actually performs the IO
```

**Principle**: Pure functions are easy to test and compose. Push IO to the boundaries.

---

## 8. Higher-Order Functions and Abstractions

### ✅ Generic Functional Patterns

```python
from typing import TypeVar, Callable, List

A = TypeVar('A')
B = TypeVar('B')
C = TypeVar('C')
E = TypeVar('E')

def traverse_result(
    items: List[A],
    func: Callable[[A], Result[B, E]]
) -> Result[List[B], E]:
    """
    Apply a function to each item, collecting Results.
    If any function returns Failure, short-circuit and return that error.
    """
    result: List[B] = []
    for item in items:
        item_result = func(item)
        if isinstance(item_result, Failure):
            return item_result
        result.append(item_result.unwrap())
    return Success(result)

# Usage
def validate_value(x: int) -> Result[int, str]:
    if x < 0:
        return Failure("Value must be non-negative")
    return Success(x * 2)

# Validate all values
numbers = [1, 2, 3, -1, 5]
result = traverse_result(numbers, validate_value)
# Failure("Value must be non-negative") - fails on 4th element

numbers = [1, 2, 3, 4, 5]
result = traverse_result(numbers, validate_value)
# Success([2, 4, 6, 8, 10])
```

---

## 9. Type-Level Programming

### ✅ Use Protocols for Higher-Kinded Types

```python
from typing import Protocol, TypeVar, Generic

A = TypeVar('A')
B = TypeVar('B')

class Functor(Protocol):
    """Type class for mappable containers"""
    def map(self, func: Callable[[A], B]) -> 'Functor[B]':
        ...

class Monad(Functor, Protocol):
    """Type class for monadic containers"""
    def bind(self, func: Callable[[A], 'Monad[B]']) -> 'Monad[B]':
        ...
    
    @staticmethod
    def pure(value: A) -> 'Monad[A]':
        ...

# Example implementation
class Maybe(Generic[A]):
    def __init__(self, value: Optional[A] = None):
        self._value = value
    
    def map(self, func: Callable[[A], B]) -> 'Maybe[B]':
        if self._value is None:
            return Maybe[B]()
        return Maybe(func(self._value))
    
    def bind(self, func: Callable[[A], 'Maybe[B]']) -> 'Maybe[B]':
        if self._value is None:
            return Maybe[B]()
        return func(self._value)
    
    @staticmethod
    def pure(value: A) -> 'Maybe[A]':
        return Maybe(value)
```

---

## 10. Pattern Matching for ADTs

### ✅ Use Structural Pattern Matching (Python 3.10+)

```python
from dataclasses import dataclass
from typing import Union

@dataclass(frozen=True)
class Loading:
    progress: float

@dataclass(frozen=True)
class Loaded:
    data: List[Item]

@dataclass(frozen=True)
class Error:
    message: str

# Algebraic Data Type
State = Union[Loading, Loaded, Error]

def handle_state(state: State) -> str:
    """Pattern match on ADT"""
    match state:
        case Loading(progress=p):
            return f"Loading... {p:.0%}"
        case Loaded(data=d):
            return f"Loaded {len(d)} items"
        case Error(message=m):
            return f"Error: {m}"
        case _:
            return "Unknown state"

# Usage
loading = Loading(0.5)
loaded = Loaded([1, 2, 3])
error = Error("Failed to load")

print(handle_state(loading))  # "Loading... 50%"
print(handle_state(loaded))   # "Loaded 3 items"
print(handle_state(error))    # "Error: Failed to load"
```

---

## Complete Example: Training Loop

```python
from dataclasses import dataclass
from returns.result import Result, Success, Failure
from returns.pipeline import flow, pipe
from returns.curry import curry
from typing import List, Tuple

@dataclass(frozen=True)
class TrainConfig:
    """Immutable training configuration"""
    learning_rate: float
    batch_size: int
    epochs: int

@dataclass(frozen=True)
class TrainState:
    """Immutable training state"""
    epoch: int
    loss: float
    accuracy: float
    weights: Dict[str, Tensor]
    
    def next_step(self, loss: float, acc: float) -> 'TrainState':
        """Return new state (immutable update)"""
        return TrainState(
            epoch=self.epoch + 1,
            loss=loss,
            accuracy=acc,
            weights=self.weights  # In real implementation, update weights
        )

def compute_gradients(
    weights: Dict[str, Tensor],
    batch: Batch
) -> Result[Dict[str, Tensor], ComputeError]:
    """Pure: Compute gradients for one batch"""
    try:
        gradients = ...
        return Success(gradients)
    except Exception as e:
        return Failure(ComputeError(f"Gradient computation failed: {e}"))

def update_weights(
    weights: Dict[str, Tensor],
    gradients: Dict[str, Tensor],
    lr: float
) -> Result[Dict[str, Tensor], UpdateError]:
    """Pure: Update weights using gradients"""
    try:
        new_weights = {
++            k: weights[k] - lr * gradients[k]
++            for k in weights
++        }
++        return Success(new_weights)
++    except Exception as e:
++        return Failure(UpdateError(f"Weight update failed: {e}"))

@curry
def train_step(
++    config: TrainConfig,
++    state: TrainState,
++    batch: Batch
++) -> Result[TrainState, Exception]:
++    """Pure: One training step"""
++    return flow(
++        compute_gradients(state.weights, batch),
++        bind(lambda grads: update_weights(state.weights, grads, config.learning_rate)),
++        map_(lambda new_weights: state.next_step(
++            loss=compute_loss(new_weights, batch),
++            acc=compute_accuracy(new_weights, batch)
++        ))
++    )

def train_epoch(
++    config: TrainConfig,
++    state: TrainState,
++    data_loader: DataLoader
++) -> Result[TrainState, Exception]:
++    """Pure: Train for one epoch"""
++    def fold_step(
++        current_state: Result[TrainState, Exception],
++        batch: Batch
++    ) -> Result[TrainState, Exception]:
++        return current_state.bind(lambda s: train_step(config, s, batch))
++    
++    return flow(
++        data_loader.batches(),
++        lambda batches: reduce(fold_step, batches, Success(state))
++    )

# Railway-oriented training pipeline
def run_training(
++    config: TrainConfig,
++    initial_state: TrainState,
++    data_loader: DataLoader
++) -> Result[TrainState, Exception]:
++    """Pure: Complete training pipeline"""
++    return flow(
++        range(config.epochs),
++        lambda epochs: reduce(
++            lambda state, _: state.bind(lambda s: train_epoch(config, s, data_loader)),
++            epochs,
++            Success(initial_state)
++        )
++    )

# Usage
config = TrainConfig(learning_rate=0.001, batch_size=32, epochs=10)
initial_state = TrainState(epoch=0, loss=0.0, accuracy=0.0, weights=initial_weights)

final_state = run_training(config, initial_state, data_loader)
# Success(TrainState) or Failure(Exception)
```

---

## Style Rules Summary

### ✅ DO:
+- Use `Result`/`Maybe` for error handling
+- Prefer immutable data structures
+- Curry functions for composition
+- Use railway-oriented programming
+- Mark side effects with `IO`/`IOResult`
+- Leverage type hints and strict mypy
+- Compose small functions into pipelines
+- Use pattern matching for ADTs

### ❌ DON'T:
+- Use try/except for control flow
+- Mutate global state
+- Mix IO and logic
+- Use `any` type (disable with mypy)
+- Raise exceptions for expected failures
+- Use mutable default arguments
+- Ignore return values from functions

---

## File Organization

```
project/
├── src/
│   ├── domain/           # Pure domain logic
│   │   ├── entities.py   # Immutable dataclasses
│   │   └── operations.py # Pure functions
│   ├── infrastructure/   # IO and side effects
│   │   ├── database.py   # IOResult operations
│   │   └── api.py        # HTTP I/O
│   └── use_cases/        # Business logic (orchestration)
│       └── train_model.py # Pipelines and composition
├── tests/
│   ├── test_pure.py      # Unit tests for pure functions
│   └── test_io.py        # Integration tests for IO
└── .cursorrules          # AI assistant rules
```

---

## Tooling

### mypy Configuration

```ini
# pyproject.toml or mypy.ini
[mypy]
strict = true
warn_return_any = true
warn_unused_configs = true
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

### Ruff Configuration

```toml
# pyproject.toml
[tool.ruff]
select = ["E", "F", "B", "I", "N", "UP", "ANN", "S", "BLE", "FBT", "A", "COM", "C4", "DTZ", "T10", "DJ", "EM", "EXE", "FA", "ISC", "ICN", "G", "INP", "PIE", "T20", "PYI", "PT", "Q", "RSE", "RET", "SLF", "SLOT", "SIM", "TID", "TCH", "INT", "ARG", "PTH", "TD", "FIX", "ERA", "PD", "PGH", "PL", "TRY", "FLY", "NPY", "AIR", "PERF", "FURB", "LOG", "RUF"]
ignore = ["E501", "COM812", "ISC001"]
```

---

## Tool-Specific Notes

### For Kimi Users

Kimi provides enhanced support for FP validation:
- **Parallel Verification**: Kimi can verify multiple pure functions simultaneously
- **Task Tool**: Use Kimi's Task tool for complex FP pipeline validation
- **SetTodoList Integration**: Track FP refactoring tasks automatically
- **Subagents**: Spawn subagents to verify type signatures and composition

Example Kimi-specific pattern:
```python
# Kimi can verify these properties in parallel:
# 1. Function purity (no side effects)
# 2. Type signature correctness  
# 3. Monad law compliance
# 4. Composition associativity
```

### For Cursor Users

Cursor provides IDE-integrated FP support:
- **Inline Suggestions**: Real-time FP pattern suggestions
- **Refactoring**: Automated conversion to FP patterns
- **Type Checking**: Integrated mypy feedback
- **VS Code Integration**: Native IDE support

### For Claude Users

Claude excels at FP code generation:
- **Pattern Generation**: Create complex FP pipelines from descriptions
- **Error Handling**: Automatic Result type wrapping
- **Composition**: Suggest optimal function compositions

### For Gemini Users

Gemini provides comprehensive FP guidance:
- **Best Practices**: Detailed explanations of FP concepts
- **Code Examples**: Extensive example libraries
- **Pattern Recognition**: Identify opportunities for FP refactoring

---

## Quick Reference Card

### Result Type Patterns

```python
# Creating Results
Success(value)          # Successful result
Failure(error)          # Failed result
safe(func)(args)        # Convert exception to Result

# Transforming Results
result.map(func)        # Apply func to success value
result.bind(func)       # Apply func that returns Result
result.fix(func)        # Recover from failure

# Chaining
flow(value, f1, bind(f2), bind(f3))
```

### Maybe Type Patterns

```python
# Creating Maybe
Some(value)             # Present value
Nothing                 # Absent value

# Working with Maybe
maybe.map(func)         # Apply if present
maybe.bind(func)        # Apply and flatten
maybe.or_else_call(func) # Provide default
```

### Pipeline Patterns

```python
# Using flow (right-to-left composition)
flow(value, f1, bind(f2), map(f3))

# Using pipe (left-to-right)
pipe(value, f1, bind(f2), map(f3))

# Using compose
composed = compose(f3, bind(f2), f1)
result = composed(value)
```

---

## Further Reading

- **Core Principles**: `code_guidelines/principles/functional_programming.md`
- **Python Patterns**: `code_guidelines/languages/python/patterns.md`
- **Code Examples**: `code_guidelines/languages/python/examples.md`
- **Library Guide**: `code_guidelines/languages/python/libraries.md`

---

**Last Updated**: 2025-11-19  
**Maintained By**: Global AI Rules System  
**Status**: Active  
**Applies To**: All AI assistants (Cursor, Kimi, Claude, Gemini)