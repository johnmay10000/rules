---
title: Swift Functional Programming Patterns Reference
language: swift
category: code_guidelines
type: patterns
applies_to: [cursor, kimi, claude, gemini]
version: 1.0.0
last_updated: 2025-11-19
---

# Swift Functional Programming Patterns Reference

Quick reference for common functional programming patterns in Swift.

## Error Handling Patterns

### Result Type

Replace throwing functions with `Result<Success, Failure>`.

```swift
enum DivisionError: Error {
    case divideByZero
}

func divide(_ a: Double, by b: Double) -> Result<Double, DivisionError> {
    guard b != 0 else {
        return .failure(.divideByZero)
    }
    return .success(a / b)
}
```

### Async/Await with Result

Combine modern concurrency with explicit error handling.

```swift
func fetchUser(id: String) async -> Result<User, NetworkError> {
    // Wrap throwing async code
    await Result {
        let (data, _) = try await URLSession.shared.data(from: url)
        try JSONDecoder().decode(User.self, from: data)
    } // Returns Result<User, Error>
}
```

## Monadic Composition

### FlatMap Chaining

Chain operations that return `Result`.

```swift
func processValue(_ x: Double) -> Result<Double, ValidationError> {
    validatePositive(x)
        .flatMap(safeSqrt)
        .flatMap(safeReciprocal)
        .map { $0 * 100 }
}
```

### Custom Operators (Optional)

For Haskell-like syntax (use with caution in teams).

```swift
infix operator >>-: MultiplicationPrecedence

func >>- <T, U, E>(lhs: Result<T, E>, rhs: (T) -> Result<U, E>) -> Result<U, E> {
    lhs.flatMap(rhs)
}

// Usage
let result = validate(x) >>- process >>- save
```

## Immutable Data

### Structs and Let

Use `struct` for value semantics and `let` for immutability.

```swift
struct User {
    let id: UUID
    let name: String
    
    func withName(_ newName: String) -> User {
        User(id: id, name: newName)
    }
}
```

### Lenses

For deep immutable updates.

```swift
struct Lens<Whole, Part> {
    let get: (Whole) -> Part
    let set: (Part, Whole) -> Whole
}

let nameLens = Lens<User, String>(
    get: { $0.name },
    set: { name, user in User(id: user.id, name: name) }
)
```

## Function Composition

### Composition Operator

```swift
infix operator >>>: AdditionPrecedence

func >>> <A, B, C>(f: @escaping (A) -> B, g: @escaping (B) -> C) -> (A) -> C {
    { g(f($0)) }
}

let pipeline = normalize >>> sigmoid >>> scale
let result = pipeline(200.0)
```

### Pipe Operator

```swift
infix operator |>: AdditionPrecedence

func |> <A, B>(value: A, f: (A) -> B) -> B {
    f(value)
}

let result = 200.0 |> normalize |> sigmoid |> scale
```

## Railway-Oriented Programming

Chain validation and operations.

```swift
func fetchUser(urlString: String) -> Result<User, NetworkError> {
    validateURL(urlString)
        .flatMap(fetchData)
        .flatMap(decodeUser)
}
```

## Pattern Matching (ADTs)

Use `enum` with associated values.

```swift
enum RemoteData<Value, Error> {
    case notAsked
    case loading
    case failure(Error)
    case success(Value)
}

// Exhaustive matching
switch state {
case .notAsked: showLoadButton()
case .loading: showSpinner()
case .failure(let error): showError(error)
case .success(let data): showContent(data)
}
```

## Higher-Order Functions

### Traverse

Map a function returning a Result over a collection, flipping the structure.

```swift
extension Sequence {
    func traverse<T, E>(_ transform: (Element) -> Result<T, E>) -> Result<[T], E> {
        // Implementation...
    }
}

let numbers = [1, 2, 3]
let result = numbers.traverse(validatePositive) 
// Returns Result<[Int], Error> instead of [Result<Int, Error>]
```
