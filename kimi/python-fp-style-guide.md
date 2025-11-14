# Python Functional Programming Style Guide for Kimi CLI

**Version**: 2.0.0  
**Last Updated**: 2025-11-14  
**Part of**: [KIMI.md](KIMI.md) Global Rule Set  
**Target**: Python projects (ML, backend, data processing)

> **📖 Global Rules**: This document extends [KIMI.md](KIMI.md) with Python-specific guidance. For mandatory universal rules (Git, documentation, testing, file size), see [KIMI.md](KIMI.md).

---

## Quick Links

- **Mandatory Rules**: See [KIMI.md](KIMI.md) sections 1-4
- **FP Principles Deep Dive**: See [KIMI_FP_PRINCIPLES.md](KIMI_FP_PRINCIPLES.md)
- **Workflow Guide**: See [KIMI_WORKFLOW_GUIDE.md](KIMI_WORKFLOW_GUIDE.md)
- **Integration**: See [KIMI.md Integration](#cursorrules-integration) below

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

## Kimi-Specific Patterns

### Using Kimi's Task Tool for FP Operations

When working with complex FP pipelines, consider spawning subagents:

```python
# Good: Use Kimi to validate complex FP pipelines
# Can spawn a subagent to verify type signatures and composition
def process_data_pipeline(data: List[Data]) -> Result[List[Result], ProcessingError]:
    # Complex pipeline that benefits from verification
    return flow(
        data,
        validate_input,           # Result[List[Data], ValidationError]
        map_(process_item),       # Result[List[Result], ProcessingError]
        bind(aggregate_results),  # Result[Aggregated, ProcessingError]
    )
```

### Parallel Processing with Kimi

Kimi can run multiple verification tasks in parallel:

```python
# Good: Structure code for parallel verification
# Kimi can verify multiple pure functions simultaneously
@curry
def transform_a(x: int) -> Result[int, ErrorA]: ...

@curry
def transform_b(x: int) -> Result[int, ErrorB]: ...

# These can be verified in parallel by Kimi's subagents
composed = transform_a >> transform_b
```

---

## Python FP Best Practices

### 1. Immutable Data Structures

```python
from dataclasses import dataclass
from typing import Tuple

# Good: Immutable configuration
dataclass(frozen=True)
class AppConfig:
    api_key: str
    max_retries: int
    timeout: float

# Good: Immutable domain models
@dataclass(frozen=True)
class User:
    id: UserId
    name: str
    email: Email
    
    def update_email(self, new_email: Email) -> 'User':
        """Returns new User instance (immutable)"""
        return User(self.id, self.name, new_email)
```

### 2. Function Composition

```python
from toolz import compose
from returns.curry import curry

# Good: Small, composable functions
@curry
def add(x: int, y: int) -> int:
    return x + y

@curry
def multiply(x: int, y: int) -> int:
    return x * y

# Compose with type safety
add_then_multiply = compose(multiply(2), add(5))
result = add_then_multiply(3)  # (3 + 5) * 2 = 16
```

### 3. Railway-Oriented Programming

```python
from returns.result import Result, Success, Failure

def parse_int(s: str) -> Result[int, ValueError]:
    try:
        return Success(int(s))
    except ValueError as e:
        return Failure(e)

def divide(a: int, b: int) -> Result[float, ZeroDivisionError]:
    if b == 0:
        return Failure(ZeroDivisionError("Cannot divide by zero"))
    return Success(a / b)

# Railway-oriented composition
def parse_and_divide(s1: str, s2: str) -> Result[float, Exception]:
    return flow(
        (s1, s2),
        lambda nums: Success(nums).map(
            lambda pair: (parse_int(pair[0]), parse_int(pair[1]))
        ),
        bind(lambda ints: divide(ints[0], ints[1]))
    )
```

### 4. Dependency Injection with Reader

```python
from returns.context import Reader

# Configuration context
@dataclass(frozen=True)
class Config:
    db_url: str
    api_key: str

# Reader pattern for DI
def get_user(user_id: UserId) -> Reader[Config, Result[User, DbError]]:
    def _inner(config: Config) -> Result[User, DbError]:
        # Use config.db_url here
        ...
    return Reader(_inner)

# Usage
config = Config(db_url="...", api_key="...")
user_result = get_user(UserId(123))(config)
```

### 5. Async FP with Returns

```python
import asyncio
from returns.future import FutureResult
from returns.io import IO

async def fetch_user_async(user_id: UserId) -> FutureResult[User, NetworkError]:
    """Async operation returning FutureResult"""
    try:
        user = await api_call(user_id)
        return Success(user)
    except NetworkError as e:
        return Failure(e)

# Compose async operations
async def get_user_with_posts(user_id: UserId) -> FutureResult[UserWithPosts, ApiError]:
    return flow(
        await fetch_user_async(user_id),
        bind(lambda user: fetch_posts_async(user.id)),
        map_(lambda posts: UserWithPosts(user, posts))
    )
```

---

## Kimi CLI Integration

### Using with Kimi Projects

1. **Project Setup**: Set KIMI_RULES_PATH environment variable
   ```bash
   export KIMI_RULES_PATH="$HOME/projects/rules"
   ```

2. **Project-Specific Rules**: Create `.kimirules` file at project root:
   ```markdown
   # .kimirules for ML Project

   ## Core FP Rules
   - All functions must be pure or explicitly marked as impure
   - Use Result types for error handling (no exceptions)
   - Prefer immutable data structures
   
   ## Kimi-Specific
   - Use Task tool for complex FP validation
   - Spawn subagents for type checking verification
   - Verify function composition with parallel tool calls
   ```

3. **Daily Work**: Kimi will automatically use corresponding language guide
   - Python projects → Use this guide
   - TypeScript projects → Use TypeScript guide
   - Rust projects → Use Rust guide

4. **Documentation**: Follow Kimi's 3-tier structure:
   - Tier 1: ARCHITECTURE_PLAN.md (strategic)
   - Tier 2: docs/plans/ (tactical)
   - Tier 3: docs/YYYY_MM_DD/ (execution)

---

## Testing FP Code with Kimi

Kimi excels at verifying FP properties:

```python
# Kimi can verify these properties:
# 1. Function purity (no side effects)
# 2. Type signature correctness
# 3. Monad law compliance
# 4. Composition associativity

# Example test structure that Kimi can validate
def test_function_purity():
    """Verify function has no side effects"""
    # Same input → same output
    # No external state modification
    pass

def test_monad_laws():
    """Verify monad laws (Left identity, Right identity, Associativity)"""
    pass

def test_type_safety():
    """Verify mypy passes with strict mode"""
    pass
```

---

## Common Patterns for MLX Projects

### ML Pipeline Example

```python
from dataclasses import dataclass
from returns.result import Result, Success, Failure
from returns.pipeline import flow
from typing import List, Any

@dataclass(frozen=True)
class TrainingConfig:
    learning_rate: float
    batch_size: int
    epochs: int

@dataclass(frozen=True)  
class TrainingData:
    features: List[List[float]]
    labels: List[int]

def load_data(path: str) -> Result[TrainingData, DataError]:
    """Pure function: Load data without side effects"""
    try:
        # Load logic here
        return Success(TrainingData(...))
    except Exception as e:
        return Failure(DataError(f"Failed to load: {e}"))

def validate_data(data: TrainingData) -> Result[TrainingData, ValidationError]:
    """Pure function: Validate data structure"""
    if not data.features:
        return Failure(ValidationError("No features found"))
    return Success(data)

def train_model(
    config: TrainingConfig, 
    data: TrainingData
) -> Result[Model, TrainingError]:
    """Pure function: Train returns Result"""
    try:
        # Training logic that doesn't mutate global state
        model = ...
        return Success(model)
    except Exception as e:
        return Failure(TrainingError(f"Training failed: {e}"))

# Railway-oriented pipeline
def run_training_pipeline(
    config: TrainingConfig,
    data_path: str
) -> Result[Model, Exception]:
    return flow(
        data_path,
        load_data,
        bind(validate_data),
        bind(lambda data: train_model(config, data))
    )

# Kimi can verify:
# - All functions are pure
# - No mutation of inputs
# - Proper error handling with Result
# - Type safety throughout pipeline
```

---

## Anti-Patterns to Avoid

### ❌ Avoid: Mutating Global State

```python
# BAD: Global state mutation
_global_cache = {}

def process_item(item: Item) -> Result[Item, Error]:
    global _global_cache
    _global_cache[item.id] = item  # Side effect!
    return Success(item)

# GOOD: Return new state explicitly
@dataclass(frozen=True)
class ProcessingState:
    cache: Dict[ItemId, Item]

def process_item(
    item: Item, 
    state: ProcessingState
) -> Result[Tuple[Item, ProcessingState], Error]:
    new_state = ProcessingState(cache={**state.cache, item.id: item})
    return Success((item, new_state))
```

### ❌ Avoid: Mixed IO and Logic

```python
# BAD: Database access mixed with computation
def calculate_and_save(value: int) -> int:
    result = value * 2  # Logic
    db.save(result)     # IO - side effect!
    return result

# GOOD: Separate IO and logic
def calculate(value: int) -> int:
    return value * 2  # Pure logic only

# IO handled at boundaries
def save_result(result: int) -> IO[None]:
    return IO(lambda: db.save(result))  # Explicit IO
```

---

## Kimi CLI Commands for FP Projects

### Verification Commands

```bash
# Verify mypy passes
$ mypy --strict src/

# Verify no mutations
$ python -m pyimmutable src/

# Run FP-specific tests
$ pytest tests/fp/ -v

# Kimi can run verification in parallel:
# - Run mypy, pyflakes, black in parallel
# - Spawn subagent for complex type checking
# - Parallel verification of multiple pure functions
```

---

## Summary

This guide provides Python-specific FP patterns optimized for Kimi CLI usage patterns. Follow the mandatory rules in KIMI.md, use the libraries listed above, and leverage Kimi's parallel verification capabilities.

**Key Takeaways**:
- Use `returns` library for monads and error handling
- Immutable data structures with `dataclasses(frozen=True)`
- Pure functions with explicit types
- Railway-oriented programming with `Result` types
- Separate IO from logic using `Reader` and `IO` monads
- Leverage Kimi's Task tool for complex FP validation

**Next**: See [KIMI_FP_PRINCIPLES.md](KIMI_FP_PRINCIPLES.md) for deeper FP concepts.

---

**Last Updated**: 2025-11-14  
**Maintained By**: Kimi CLI Global Rules System  
**Status**: Active
