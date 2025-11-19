---
title: Python Functional Programming Patterns Reference
language: python
category: code_guidelines
type: patterns
applies_to: [cursor, kimi, claude, gemini]
version: 2.0.0
last_updated: 2025-11-19
---

# Python Functional Programming Patterns Reference

Quick reference for common functional programming patterns in Python. For detailed explanations, see `fp_style_guide.md`.

---

## Error Handling Patterns

### Result Type (Either)

Wrap success/failure in a type-safe container.

```python
from returns.result import Result, Success, Failure

def divide(a: float, b: float) -> Result[float, str]:
    if b == 0:
        return Failure("Cannot divide by zero")
    return Success(a / b)

# Usage
result = divide(10, 2)  # Success(5.0)
result = divide(10, 0)  # Failure("Cannot divide by zero")

# Transform
result.map(lambda x: x * 2)  # Success(10.0) or Failure(...)
result.bind(lambda x: divide(x, 2))  # Chaining
```

**When to use**: Any operation that can fail. Replaces exceptions.

---

### @safe Decorator

Convert exceptions to Result automatically.

```python
from returns.result import safe

@safe
def parse_int(s: str) -> int:
    return int(s)

# Usage
result = parse_int("123")  # Success(123)
result = parse_int("abc")  # Failure(ValueError)
```

**When to use**: Wrapping existing functions that throw exceptions.

---

### Maybe/Option

Handle optional values without None checks.

```python
from returns.maybe import Maybe, Some, Nothing

def find_user(id: int) -> Maybe[User]:
    user = db.get(id)
    return Some(user) if user else Nothing

# Usage
user = find_user(123)
user.map(lambda u: u.email)  # Some(email) or Nothing
user.or_else_call(lambda: create_guest_user())
```

**When to use**: Optional values, nullable fields.

---

## Composition Patterns

### Function Composition (compose)

Build pipelines from right to left.

```python
from toolz import compose

def load(path: str) -> Data: ...
def clean(data: Data) -> Data: ...
def transform(data: Data) -> Tensor: ...

pipeline = compose(transform, clean, load)
result = pipeline("./data.csv")
```

**When to use**: Creating reusable transformation pipelines.

---

### Pipe (Left-to-Right)

Chain operations sequentially.

```python
from returns.pipeline import pipe

def load(path: str) -> Data: ...
def clean(data: Data) -> Data: ...
def transform(data: Data) -> Tensor: ...

result = pipe(
    "./data.csv",
    load,
    clean,
    transform
)
```

**When to use**: Readable sequential transformations.

---

### Flow with Bind (Railway-Oriented)

Chain operations that can fail.

```python
from returns.pipeline import flow, bind
from returns.result import Result

def validate(x: int) -> Result[int, str]: ...
def process(x: int) -> Result[int, str]: ...
def save(x: int) -> Result[int, str]: ...

result = flow(
    input_value,
    validate,
    bind(process),
    bind(save)
)
```

**When to use**: Multi-step operations where any step can fail.

---

### Currying

Create specialized functions from general ones.

```python
from returns.curry import curry

@curry
def scale(factor: float, value: float) -> float:
    return value * factor

# Create specialized versions
scale_by_2 = scale(2.0)
scale_by_0.5 = scale(0.5)

# Usage
scale_by_2(10)  # 20.0
```

**When to use**: Creating reusable function variants, partial application.

---

## Data Structure Patterns

### Immutable Dataclasses

Create immutable domain models.

```python
from dataclasses import dataclass

@dataclass(frozen=True)
class Config:
    lr: float
    batch_size: int
    
    def with_lr(self, new_lr: float) -> 'Config':
        return Config(new_lr, self.batch_size)

# Usage
config = Config(0.001, 32)
new_config = config.with_lr(0.002)  # config unchanged
```

**When to use**: Configuration, domain models, state management.

---

### Immutable State Updates

Functional state transitions.

```python
@dataclass(frozen=True)
class TrainingState:
    epoch: int
    loss: float
    
    def next(self, loss: float) -> 'TrainingState':
        return TrainingState(self.epoch + 1, loss)

# Usage
state = TrainingState(0, 0.0)
new_state = state.next(0.5)  # state unchanged
```

**When to use**: State machines, loops, accumulators.

---

### ADTs with Pattern Matching

Algebraic data types with structural matching.

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

State = Union[Loading, Loaded, Error]

def handle(state: State) -> str:
    match state:
        case Loading(p):
            return f"Loading... {p:.0%}"
        case Loaded(d):
            return f"Loaded {len(d)} items"
        case Error(m):
            return f"Error: {m}"
```

**When to use**: State representation, error handling, complex conditions.

---

## Side Effect Patterns

### IO Monad

Mark impure operations explicitly.

```python
from returns.io import IO, IOResult

def read_file(path: str) -> IOResult[str, IOError]:
+    def _read() -> Result[str, IOError]:
+        try:
+            with open(path) as f:
+                return Success(f.read())
+        except IOError as e:
+            return Failure(e)
+    return IO(_read)

# Usage
file_io = read_file("config.json")
content = file_io()  # Execute IO at the edge
```

**When to use**: File operations, network calls, any side effects.

---

### Reader Monad (Dependency Injection)

Inject dependencies functionally.

```python
from returns.context import Reader
+from dataclasses import dataclass

+@dataclass(frozen=True)
+class Config:
+    db_url: str
+    api_key: str

+def get_user(id: int) -> Reader[Config, Result[User, Error]]:
+    def _inner(config: Config) -> Result[User, Error]:
+        # Use config.db_url
+        ...
+    return Reader(_inner)

# Usage
config = Config(db_url="...", api_key="...")
+user_result = get_user(123)(config)
```

**When to use**: Configuration, dependency injection, testing.

---

## Collection Patterns

### Traverse

Apply function to all items, collecting results.

```python
from returns.result import Result, Success, Failure
+from typing import List

+def traverse(
+    items: List[A],
+    func: Callable[[A], Result[B, E]]
+) -> Result[List[B], E]:
+    result: List[B] = []
+    for item in items:
+        item_result = func(item)
+        if isinstance(item_result, Failure):
+            return item_result
+        result.append(item_result.unwrap())
+    return Success(result)

# Usage
+def validate(x: int) -> Result[int, str]:
+    return Success(x * 2) if x > 0 else Failure("Invalid")

+numbers = [1, 2, 3, 4, 5]
+result = traverse(numbers, validate)  # Success([2, 4, 6, 8, 10])
```

**When to use**: Validating collections, transforming with error handling.

---

### Filter with Maybe

Filter out None values functionally.

```python
from returns.maybe import Maybe, Some, Nothing
+from typing import List

+def safe_divide(a: float, b: float) -> Maybe[float]:
+    return Some(a / b) if b != 0 else Nothing

+results = [safe_divide(10, 2), safe_divide(10, 0), safe_divide(8, 4)]
+valid = [r.unwrap() for r in results if isinstance(r, Some)]
+# [5.0, 2.0]
```

**When to use**: Safe operations that might not produce a value.

---

## Advanced Patterns

### Functor/Monad Type Classes

Abstract over container types.

```python
+from typing import Protocol, TypeVar, Callable
+
+A = TypeVar('A')
+B = TypeVar('B')
+
+class Functor(Protocol):
+    def map(self, func: Callable[[A], B]) -> 'Functor[B]':
+        ...
+
+class Monad(Functor, Protocol):
+    def bind(self, func: Callable[[A], 'Monad[B]']) -> 'Monad[B]':
+        ...
+    @staticmethod
+    def pure(value: A) -> 'Monad[A]':
+        ...
```

**When to use**: Building generic FP abstractions, library code.

---

### Fold/Reduce

Accumulate values functionally.

```python
from returns.result import Result, Success
+from typing import List

+def sum_results(numbers: List[Result[int, str]]) -> Result[int, str]:
+    def combine(
+        acc: Result[int, str],
+        item: Result[int, str]
+    ) -> Result[int, str]:
+        return acc.bind(lambda a: item.map(lambda i: a + i))
+    
+    return reduce(combine, numbers, Success(0))

# Usage
+numbers = [Success(1), Success(2), Success(3)]
+total = sum_results(numbers)  # Success(6)
```

**When to use**: Aggregating results, folding collections.

---

## Anti-Patterns

### ❌ Mutable Default Arguments

```python
# BAD
+def add_item(item, items=[]):
+    items.append(item)
+    return items

# GOOD
+def add_item(item, items=None):
+    items = items or []
+    return items + [item]
```

---

### ❌ Mixed IO and Logic

```python
# BAD
+def calculate_and_save(value):
+    result = value * 2  # Logic
+    db.save(result)     # IO side effect!
+    return result

# GOOD
+def calculate(value):  # Pure
+    return value * 2
+
+def save_result(result):  # Explicit IO
+    return IO(lambda: db.save(result))
```

---

### ❌ Using Exceptions for Control Flow

```python
# BAD
+try:
+    return cache[key]
+except KeyError:
+    return load_from_db(key)

# GOOD
+def get_value(key):
+    return Maybe.from_optional(cache.get(key)).or_else_call(
+        lambda: load_from_db(key)
+    )
```

---

## Pattern Selection Guide

| Problem | Pattern | Example |
|---------|---------|---------|
+| Function can fail | Result/Either | `divide(a, b) -> Result[float, str]` |
+| Value might be absent | Maybe/Option | `find_user(id) -> Maybe[User]` |
+| Multiple steps can fail | Flow + Bind | `flow(x, f1, bind(f2), bind(f3))` |
+| Need to reuse function | Currying | `@curry def add(a, b)` |
+| Transform pipeline | Compose/Pipe | `compose(f3, f2, f1)` |
+| Configuration/Deps | Reader | `Reader[Config, Result[T, E]]` |
+| Side effects | IO Monad | `read_file() -> IOResult[str, E]` |
+| Validate collection | Traverse | `traverse(items, validate)` |
+| Optional transformations | Map + Maybe | `maybe.map(transform)` |
+| State transitions | Immutable Update | `state.next(new_value)` |
+
+---
+
+**Last Updated**: 2025-11-19
+**Maintained By**: Global AI Rules System
+**Status**: Active
+**Applies To**: All AI assistants