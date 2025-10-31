# CURSOR_FP_PRINCIPLES.md - Functional Programming Deep Dive

**Version**: 1.0.0  
**Last Updated**: 2025-10-31  
**Companion To**: [CURSOR.md](CURSOR.md)  
**Purpose**: Deep dive into functional programming principles

---

## Overview

This document provides detailed explanations and examples of functional programming (FP) principles referenced in [CURSOR.md](CURSOR.md). 

**Who This Is For**:
- Developers learning FP
- Teams adopting FP patterns
- Anyone needing deeper understanding of FP concepts

**Mental Model**: Think "factory assembly line"
- No category theory required
- Practical, real-world examples
- Same patterns across languages

---

## Table of Contents

1. [Algebraic Data Types (ADTs)](#1-algebraic-data-types-adts)
2. [Result/Either Types](#2-resulteither-types)
3. [Monadic Composition](#3-monadic-composition)
4. [Railway-Oriented Programming](#4-railway-oriented-programming)
5. [Function Currying](#5-function-currying)
6. [Immutable Data Structures](#6-immutable-data-structures)
7. [Pattern Matching](#7-pattern-matching)
8. [Real-World Examples](#8-real-world-examples)

---

## 1. Algebraic Data Types (ADTs)

### What Are ADTs?

**Simple Definition**: Type-safe way to model data with multiple possible shapes.

**Two Main Types**:

**Product Types** (AND relationship):
```python
@dataclass(frozen=True)
class User:
    id: str          # Has id AND
    name: str        # Has name AND
    email: str       # Has email
```

**Sum Types** (OR relationship):
```python
@dataclass(frozen=True)
class Success:
    value: int

@dataclass(frozen=True)
class Failure:
    error: str

Result = Success | Failure  # Success OR Failure
```

### Why ADTs?

**Problem Without ADTs**:
```python
# What does None mean?
def find_user(id: str) -> User | None:
    ...

user = find_user("123")
if user is None:
    # Was it not found? Network error? Permission denied?
    # We don't know!
```

**Solution With ADTs**:
```python
@dataclass(frozen=True)
class UserFound:
    user: User

@dataclass(frozen=True)
class UserNotFound:
    user_id: str

@dataclass(frozen=True)
class NetworkError:
    message: str

FindUserResult = UserFound | UserNotFound | NetworkError

def find_user(id: str) -> FindUserResult:
    ...

# Now we know exactly what happened!
match result:
    case UserFound(user):
        print(f"Found: {user.name}")
    case UserNotFound(user_id):
        print(f"User {user_id} not found")
    case NetworkError(message):
        print(f"Network error: {message}")
```

### Cross-Language ADTs

**TypeScript**:
```typescript
type FindUserResult =
  | { _tag: 'UserFound'; user: User }
  | { _tag: 'UserNotFound'; userId: string }
  | { _tag: 'NetworkError'; message: string }
```

**Swift**:
```swift
enum FindUserResult {
    case userFound(User)
    case userNotFound(String)
    case networkError(String)
}
```

**Kotlin**:
```kotlin
sealed class FindUserResult {
    data class UserFound(val user: User) : FindUserResult()
    data class UserNotFound(val userId: String) : FindUserResult()
    data class NetworkError(val message: String) : FindUserResult()
}
```

---

## 2. Result/Either Types

### The Pattern

**Result Type** = Generic Success OR Failure

```python
Result[SuccessType, ErrorType] = Success[SuccessType] | Failure[ErrorType]
```

### Why Not Exceptions?

**Problems with Exceptions**:
```python
def divide(a: float, b: float) -> float:
    # What errors can this throw? 
    # Function signature doesn't tell us!
    if b == 0:
        raise ValueError("Division by zero")
    if not isinstance(a, (int, float)):
        raise TypeError("Invalid type")
    return a / b

# Calling code must know implementation details
try:
    result = divide(10, 0)  # Runtime error!
except ValueError:
    ...
except TypeError:
    ...
except Exception:  # What else can go wrong?
    ...
```

**Solutions with Result**:
```python
def divide(a: float, b: float) -> Result[float, str]:
    # Errors are in the type signature!
    if b == 0:
        return Failure("Division by zero")
    return Success(a / b)

# Calling code knows exactly what to expect
match divide(10, 0):
    case Success(value):
        print(f"Result: {value}")
    case Failure(error):
        print(f"Error: {error}")
```

### Real-World Example: Database Query

**Before** (exceptions):
```python
def get_user(id: str) -> User:
    user = db.query("SELECT * FROM users WHERE id = ?", id)
    if not user:
        raise UserNotFoundError(id)
    if not db.connection:
        raise DatabaseError("Connection lost")
    return user

# Must remember all possible exceptions
try:
    user = get_user("123")
except UserNotFoundError:
    ...
except DatabaseError:
    ...
except Exception:  # Catch-all for unknown errors
    ...
```

**After** (Result):
```python
@dataclass(frozen=True)
class UserNotFound:
    user_id: str

@dataclass(frozen=True)
class DatabaseError:
    message: str

UserError = UserNotFound | DatabaseError

def get_user(id: str) -> Result[User, UserError]:
    if not db.connection:
        return Failure(DatabaseError("Connection lost"))
    
    user = db.query("SELECT * FROM users WHERE id = ?", id)
    if not user:
        return Failure(UserNotFound(id))
    
    return Success(user)

# Type system forces us to handle all cases
match get_user("123"):
    case Success(user):
        print(f"Found: {user.name}")
    case Failure(UserNotFound(user_id)):
        print(f"User {user_id} not found")
    case Failure(DatabaseError(message)):
        print(f"Database error: {message}")
```

---

## 3. Monadic Composition

### The Problem: Error Handling in Chains

**Without Composition** (messy):
```python
result1 = fetch_user(id)
if isinstance(result1, Failure):
    return result1  # Early return

result2 = validate_user(result1.value)
if isinstance(result2, Failure):
    return result2  # Early return

result3 = update_user(result2.value)
if isinstance(result3, Failure):
    return result3  # Early return

return format_response(result3.value)
```

**With Composition** (clean):
```python
return (
    fetch_user(id)
    .bind(validate_user)
    .bind(update_user)
    .map(format_response)
)
```

### How It Works

**Two Operations**:

**`bind` (flatMap)** - Chain operations that return Result:
```python
# If Success, apply function and return its Result
# If Failure, skip function and pass Failure through

Success(5).bind(lambda x: Success(x * 2))
# → Success(10)

Failure("error").bind(lambda x: Success(x * 2))
# → Failure("error")  # Function not called!
```

**`map`** - Transform success value:
```python
# If Success, apply function and wrap result
# If Failure, skip function and pass Failure through

Success(5).map(lambda x: x * 2)
# → Success(10)

Failure("error").map(lambda x: x * 2)
# → Failure("error")  # Function not called!
```

### Factory Assembly Line Analogy

```
Station 1 (fetch)  →  Station 2 (validate)  →  Station 3 (transform)  →  Output
   .bind()                .bind()                  .map()

If ANY station fails → STOP LINE → Pass error to end
If ALL succeed → Continue → Get final product
```

### Real Example: User Registration

```python
def register_user(data: dict) -> Result[User, RegistrationError]:
    return (
        validate_email(data)         # Station 1: Check email format
        .bind(check_email_unique)    # Station 2: Check not already used
        .bind(validate_password)     # Station 3: Check password strength
        .bind(hash_password)         # Station 4: Hash the password
        .bind(save_to_database)      # Station 5: Save user
        .map(send_welcome_email)     # Station 6: Send email (side effect)
        .map(format_user_response)   # Station 7: Format response
    )

# If validate_email fails → stops at Station 1, returns Failure
# If check_email_unique fails → stops at Station 2, returns Failure
# If all succeed → returns Success with formatted user
```

---

## 4. Railway-Oriented Programming

### The Concept

**Two Parallel Tracks**:
- **Success Track** (top rail) - Operations continue
- **Failure Track** (bottom rail) - Errors propagate

```
Success Track:  fetch → validate → transform → save → SUCCESS
                  ↓         ↓          ↓         ↓
Failure Track:  ──────────────────────────────→ FAILURE
```

**Switches** (operations):
- Start on success track
- If operation fails → switch to failure track
- Once on failure track → stay there (skip remaining operations)
- At end → check which track we're on

### Visual Example

```python
# Each .bind() is a potential switch point

result = (
    fetch_user(id)           # ✓ Success → continue
    .bind(validate_age)      # ✓ Success → continue
    .bind(validate_email)    # ✗ FAIL → switch to failure track
    .bind(update_profile)    # ⊗ Skipped (on failure track)
    .bind(send_notification) # ⊗ Skipped (on failure track)
    .map(format_response)    # ⊗ Skipped (on failure track)
)

# result = Failure(EmailInvalid(...))
```

### Real-World API Endpoint

```typescript
// Next.js API route with Railway-Oriented Programming

export async function POST(req: Request) {
  const result = await pipe(
    parseRequestBody(req),        // Parse JSON
    TE.flatMap(validateInput),    // Validate schema
    TE.flatMap(checkPermissions),  // Check user can do this
    TE.flatMap(fetchFromDatabase), // Get data
    TE.flatMap(transformData),     // Process it
    TE.flatMap(saveToDatabase),    // Save result
    TE.map(formatResponse)         // Format output
  )()

  // Handle the final result
  return match(result)
    .with({ _tag: 'Right' }, ({ right }) => 
      Response.json(right, { status: 200 }))
    .with({ _tag: 'Left' }, ({ left }) =>
      Response.json({ error: left }, { status: 400 }))
    .exhaustive()
}
```

---

## 5. Function Currying

### What Is Currying?

**Definition**: Transform function with multiple arguments into sequence of functions with single argument.

**Without Currying**:
```python
def add(a: int, b: int) -> int:
    return a + b

result = add(5, 3)  # 8
```

**With Currying**:
```python
def add(a: int) -> Callable[[int], int]:
    def add_b(b: int) -> int:
        return a + b
    return add_b

add_five = add(5)     # Returns function
result = add_five(3)  # 8
```

### Why Curry?

**Partial Application** - Create specialized functions:

```python
from toolz import curry

@curry
def fetch_from_api(base_url: str, endpoint: str, params: dict) -> Result:
    return requests.get(f"{base_url}/{endpoint}", params=params)

# Create specialized fetchers
fetch_users = fetch_from_api("https://api.example.com", "users")
fetch_posts = fetch_from_api("https://api.example.com", "posts")

# Use them
users = fetch_users({"page": 1})
posts = fetch_posts({"limit": 10})
```

### Real Example: Logging

```python
@curry
def log(level: str, module: str, message: str) -> None:
    print(f"[{level}] {module}: {message}")

# Create specialized loggers
info = log("INFO")
info_auth = info("AUTH")
info_api = info("API")

# Use them
info_auth("User logged in")        # [INFO] AUTH: User logged in
info_api("Request processed")      # [INFO] API: Request processed
```

---

## 6. Immutable Data Structures

### Why Immutability?

**Problems with Mutation**:
```python
# Mutable data (dangerous)
user = {"name": "Alice", "age": 30}

def update_age(user: dict) -> dict:
    user["age"] += 1  # Mutates original!
    return user

original = {"name": "Alice", "age": 30}
updated = update_age(original)

print(original)  # {"name": "Alice", "age": 31}  😱
# Original changed! We didn't want that!
```

**Solution with Immutability**:
```python
from dataclasses import dataclass

@dataclass(frozen=True)  # Immutable!
class User:
    name: str
    age: int

def update_age(user: User) -> User:
    # Create new instance, don't modify original
    return User(name=user.name, age=user.age + 1)

original = User("Alice", 30)
updated = update_age(original)

print(original.age)  # 30  ✓ Original unchanged!
print(updated.age)   # 31  ✓ New instance created!
```

### Benefits

1. **Thread Safety** - No race conditions
2. **Easy Reasoning** - Value never changes
3. **Time Travel** - Keep history of states
4. **Undo/Redo** - Store previous versions
5. **Caching** - Safe to cache (value won't change)

### Updating Nested Structures

**Problem**:
```python
@dataclass(frozen=True)
class Address:
    street: str
    city: str

@dataclass(frozen=True)
class User:
    name: str
    address: Address

# How to update city?
user = User("Alice", Address("123 Main", "Boston"))
# Can't do: user.address.city = "NYC"  # frozen!
```

**Solution - Create New Instances**:
```python
def update_city(user: User, new_city: str) -> User:
    new_address = Address(
        street=user.address.street,
        city=new_city
    )
    return User(
        name=user.name,
        address=new_address
    )

updated = update_city(user, "NYC")
```

**Better Solution - Use Lenses** (see language guides):
```python
# With lenses (monocle-ts in TypeScript)
updated = city_lens.set("NYC")(user)
```

---

## 7. Pattern Matching

### Exhaustive Case Handling

**Why Pattern Matching > if/isinstance**:

**Bad** (isinstance):
```python
def handle_result(result):
    if isinstance(result, Success):
        return result.value
    elif isinstance(result, Failure):
        return None
    # What if we add a new type? No compile error!
```

**Good** (pattern matching):
```python
def handle_result(result: Result[int, str]) -> int | None:
    match result:
        case Success(value):
            return value
        case Failure(error):
            return None
    # Compiler ensures all cases handled!
```

### Real Example: Payment Processing

```python
@dataclass(frozen=True)
class PaymentSuccessful:
    transaction_id: str
    amount: float

@dataclass(frozen=True)
class InsufficientFunds:
    available: float
    required: float

@dataclass(frozen=True)
class CardDeclined:
    reason: str

@dataclass(frozen=True)
class NetworkError:
    retry_after: int

PaymentResult = PaymentSuccessful | InsufficientFunds | CardDeclined | NetworkError

def handle_payment(result: PaymentResult) -> str:
    match result:
        case PaymentSuccessful(transaction_id, amount):
            return f"Success! Transaction {transaction_id} for ${amount}"
        
        case InsufficientFunds(available, required):
            return f"Insufficient funds: ${available} available, ${required} required"
        
        case CardDeclined(reason):
            return f"Card declined: {reason}"
        
        case NetworkError(retry_after):
            return f"Network error. Retry after {retry_after}s"
    
    # If we add a new payment result type, compiler will error!
```

---

## 8. Real-World Examples

### Example 1: User Registration Pipeline

```python
from dataclasses import dataclass
from typing import Result

@dataclass(frozen=True)
class User:
    id: str
    email: str
    password_hash: str

@dataclass(frozen=True)
class RegistrationData:
    email: str
    password: str

# Error types
@dataclass(frozen=True)
class InvalidEmail:
    email: str

@dataclass(frozen=True)
class WeakPassword:
    reason: str

@dataclass(frozen=True)
class EmailTaken:
    email: str

RegistrationError = InvalidEmail | WeakPassword | EmailTaken

# Pipeline functions
def validate_email(data: RegistrationData) -> Result[RegistrationData, InvalidEmail]:
    if "@" not in data.email:
        return Failure(InvalidEmail(data.email))
    return Success(data)

def check_password_strength(data: RegistrationData) -> Result[RegistrationData, WeakPassword]:
    if len(data.password) < 8:
        return Failure(WeakPassword("Password must be at least 8 characters"))
    return Success(data)

def check_email_available(data: RegistrationData) -> Result[RegistrationData, EmailTaken]:
    if db.email_exists(data.email):
        return Failure(EmailTaken(data.email))
    return Success(data)

def create_user(data: RegistrationData) -> Result[User, Never]:
    password_hash = hash_password(data.password)
    user = User(id=generate_id(), email=data.email, password_hash=password_hash)
    db.save(user)
    return Success(user)

# Main registration function
def register_user(email: str, password: str) -> Result[User, RegistrationError]:
    data = RegistrationData(email, password)
    
    return (
        Success(data)
        .bind(validate_email)
        .bind(check_password_strength)
        .bind(check_email_available)
        .bind(create_user)
    )

# Usage
match register_user("alice@example.com", "secure123"):
    case Success(user):
        print(f"User {user.email} registered!")
    case Failure(InvalidEmail(email)):
        print(f"Invalid email: {email}")
    case Failure(WeakPassword(reason)):
        print(f"Weak password: {reason}")
    case Failure(EmailTaken(email)):
        print(f"Email {email} already taken")
```

### Example 2: API Data Fetching

```typescript
import { pipe } from 'fp-ts/function'
import * as TE from 'fp-ts/TaskEither'

// Types
interface User {
  id: string
  name: string
  email: string
}

interface Post {
  id: string
  userId: string
  title: string
  content: string
}

interface UserWithPosts {
  user: User
  posts: Post[]
}

type ApiError =
  | { _tag: 'NetworkError'; message: string }
  | { _tag: 'NotFound'; id: string }
  | { _tag: 'Unauthorized' }

// API functions
const fetchUser = (id: string): TE.TaskEither<ApiError, User> =>
  TE.tryCatch(
    async () => {
      const response = await fetch(`/api/users/${id}`)
      if (!response.ok) {
        throw response.status === 404 
          ? { _tag: 'NotFound' as const, id }
          : { _tag: 'Unauthorized' as const }
      }
      return response.json()
    },
    (error): ApiError => ({ 
      _tag: 'NetworkError', 
      message: String(error) 
    })
  )

const fetchUserPosts = (userId: string): TE.TaskEither<ApiError, Post[]> =>
  TE.tryCatch(
    async () => {
      const response = await fetch(`/api/users/${userId}/posts`)
      if (!response.ok) throw new Error('Failed to fetch posts')
      return response.json()
    },
    (error): ApiError => ({ 
      _tag: 'NetworkError', 
      message: String(error) 
    })
  )

// Composed pipeline
const fetchUserWithPosts = (
  userId: string
): TE.TaskEither<ApiError, UserWithPosts> =>
  pipe(
    fetchUser(userId),                              // Fetch user
    TE.flatMap(user =>                              // If success, fetch posts
      pipe(
        fetchUserPosts(user.id),
        TE.map(posts => ({ user, posts }))          // Combine user + posts
      )
    )
  )

// Usage
const program = pipe(
  fetchUserWithPosts('123'),
  TE.match(
    (error) => {
      // Handle errors
      switch (error._tag) {
        case 'NetworkError':
          return `Network error: ${error.message}`
        case 'NotFound':
          return `User ${error.id} not found`
        case 'Unauthorized':
          return 'Unauthorized'
      }
    },
    (data) => {
      // Handle success
      return `User ${data.user.name} has ${data.posts.length} posts`
    }
  )
)

// Run the program
const result = await program()
console.log(result)
```

---

## Summary

### Key Takeaways

1. **ADTs** - Model data with type safety
2. **Result Types** - Explicit error handling in type signatures
3. **Composition** - Chain operations with `.bind()` and `.map()`
4. **Railway-Oriented** - Two tracks, switch on error
5. **Currying** - Create specialized functions through partial application
6. **Immutability** - Data never changes, create new instances
7. **Pattern Matching** - Exhaustive case handling with compiler support

### Mental Model

**Factory Assembly Line**:
- Each function = one station
- Each does one thing
- Errors stop the line
- Success continues to next station
- No category theory needed!

### Next Steps

**Language-Specific Implementation**:
- Python: See [`python-fp-style-guide.md`](python-fp-style-guide.md)
- TypeScript: See [`typescript-fp-style-guide.md`](typescript-fp-style-guide.md)
- Swift: See [`swift-fp-style-guide.md`](swift-fp-style-guide.md)
- Kotlin: See [`kotlin-fp-style-guide.md`](kotlin-fp-style-guide.md)

**Further Reading**:
- Railway-Oriented Programming (Scott Wlaschin)
- Domain Modeling Made Functional (Scott Wlaschin)
- fp-ts documentation
- returns documentation

---

**Version**: 1.0.0  
**Last Updated**: 2025-10-31  
**Maintained By**: Global Rules Repository

---

**Master FP patterns for type-safe, maintainable code!**

