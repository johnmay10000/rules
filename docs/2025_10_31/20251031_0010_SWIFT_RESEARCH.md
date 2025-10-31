# Swift Research - Foldable & Traversable

**Date**: 2025-10-31  
**Task**: Research Swift native + Bow library for Foldable/Traversable  
**Status**: ✅ COMPLETE  
**Time**: 40 minutes  

---

## Overview

Swift has strong native support for Foldable-like operations through its `Sequence` and `Collection` protocols. For full Traversable support, the Bow library provides functional programming typeclasses.

---

## Part 1: Native Swift (Foldable)

### Sequence Protocol

Swift's `Sequence` protocol provides the foundation for folding:

```swift
public protocol Sequence {
    associatedtype Element
    associatedtype Iterator: IteratorProtocol where Iterator.Element == Element
    
    func makeIterator() -> Iterator
}
```

### Built-in Fold Operations

**reduce (foldl)**:
```swift
let numbers = [1, 2, 3, 4, 5]

// Sum (left fold)
let sum = numbers.reduce(0, +)
// Result: 15

// Product
let product = numbers.reduce(1, *)
// Result: 120

// String concatenation
let words = ["Hello", "World", "Swift"]
let sentence = words.reduce("") { $0 + " " + $1 }
// Result: " Hello World Swift"

// Custom accumulation
let doubled = numbers.reduce([]) { acc, n in
    acc + [n * 2]
}
// Result: [2, 4, 6, 8, 10]
```

**reduce(into:) - Efficient Mutation**:
```swift
// More efficient for collection building
let doubled = numbers.reduce(into: []) { acc, n in
    acc.append(n * 2)
}

// Dictionary accumulation
let pairs = [(1, "a"), (2, "b"), (3, "c")]
let dict = pairs.reduce(into: [:]) { acc, pair in
    acc[pair.0] = pair.1
}
// Result: [1: "a", 2: "b", 3: "c"]
```

### Custom Foldable Protocol

We can create our own Foldable protocol matching Haskell's:

```swift
protocol Foldable {
    associatedtype Element
    
    /// Right-associative fold
    func foldr<B>(_ f: @escaping (Element, B) -> B, _ initial: B) -> B
    
    /// Left-associative fold (native reduce)
    func foldl<B>(_ f: @escaping (B, Element) -> B, _ initial: B) -> B
    
    /// Fold with monoid
    func fold<M: Monoid>(_ monoid: M) -> M.Value where M.Value == Element
    
    /// Map then fold
    func foldMap<M: Monoid>(_ monoid: M, _ f: @escaping (Element) -> M.Value) -> M.Value
}

// Monoid protocol
protocol Monoid {
    associatedtype Value
    static var empty: Value { get }
    static func combine(_ a: Value, _ b: Value) -> Value
}

// Array conformance
extension Array: Foldable {
    func foldr<B>(_ f: @escaping (Element, B) -> B, _ initial: B) -> B {
        reversed().reduce(initial) { f($1, $0) }
    }
    
    func foldl<B>(_ f: @escaping (B, Element) -> B, _ initial: B) -> B {
        reduce(initial, f)
    }
    
    func fold<M: Monoid>(_ monoid: M) -> M.Value where M.Value == Element {
        foldl({ M.combine($0, $1) }, M.empty)
    }
    
    func foldMap<M: Monoid>(_ monoid: M, _ f: @escaping (Element) -> M.Value) -> M.Value {
        foldl({ M.combine($0, f($1)) }, M.empty)
    }
}

// Example Monoids
struct SumMonoid: Monoid {
    typealias Value = Int
    static var empty: Int { 0 }
    static func combine(_ a: Int, _ b: Int) -> Int { a + b }
}

struct ProductMonoid: Monoid {
    typealias Value = Int
    static var empty: Int { 1 }
    static func combine(_ a: Int, _ b: Int) -> Int { a * b }
}

// Usage
let numbers = [1, 2, 3, 4, 5]
let sum = numbers.fold(SumMonoid())  // 15
let product = numbers.fold(ProductMonoid())  // 120
```

### Custom Tree with Foldable

```swift
indirect enum Tree<A>: Foldable {
    case leaf(A)
    case branch(Tree<A>, Tree<A>)
    
    typealias Element = A
    
    func foldr<B>(_ f: @escaping (A, B) -> B, _ initial: B) -> B {
        switch self {
        case .leaf(let value):
            return f(value, initial)
        case .branch(let left, let right):
            let rightResult = right.foldr(f, initial)
            return left.foldr(f, rightResult)
        }
    }
    
    func foldl<B>(_ f: @escaping (B, A) -> B, _ initial: B) -> B {
        switch self {
        case .leaf(let value):
            return f(initial, value)
        case .branch(let left, let right):
            let leftResult = left.foldl(f, initial)
            return right.foldl(f, leftResult)
        }
    }
    
    func fold<M: Monoid>(_ monoid: M) -> M.Value where M.Value == A {
        foldl({ M.combine($0, $1) }, M.empty)
    }
    
    func foldMap<M: Monoid>(_ monoid: M, _ f: @escaping (A) -> M.Value) -> M.Value {
        foldl({ M.combine($0, f($1)) }, M.empty)
    }
}

// Usage
let tree: Tree<Int> = .branch(
    .leaf(1),
    .branch(.leaf(2), .leaf(3))
)

let sum = tree.fold(SumMonoid())  // 6
let product = tree.fold(ProductMonoid())  // 6
```

---

## Part 2: Native Swift (Traversable)

### Traverse with Result

```swift
extension Array {
    /// Traverse with Result
    func traverse<Success, Failure: Error>(
        _ transform: (Element) -> Result<Success, Failure>
    ) -> Result<[Success], Failure> {
        var results: [Success] = []
        results.reserveCapacity(count)
        
        for element in self {
            switch transform(element) {
            case .success(let value):
                results.append(value)
            case .failure(let error):
                return .failure(error)
            }
        }
        
        return .success(results)
    }
    
    /// Sequence: [Result<A, E>] -> Result<[A], E>
    func sequence<Success, Failure: Error>() -> Result<[Success], Failure>
        where Element == Result<Success, Failure> {
        traverse { $0 }
    }
}

// Usage
enum ValidationError: Error {
    case negative(Int)
    case zero
}

func validatePositive(_ n: Int) -> Result<Int, ValidationError> {
    if n > 0 {
        return .success(n)
    } else if n == 0 {
        return .failure(.zero)
    } else {
        return .failure(.negative(n))
    }
}

let numbers = [1, 2, 3, 4, 5]
let validated = numbers.traverse(validatePositive)
// Result: .success([1, 2, 3, 4, 5])

let badNumbers = [1, -2, 3]
let badValidated = badNumbers.traverse(validatePositive)
// Result: .failure(.negative(-2))
```

### Traverse with Optional

```swift
extension Array {
    /// Traverse with Optional
    func traverse<T>(_ transform: (Element) -> T?) -> [T]? {
        var results: [T] = []
        results.reserveCapacity(count)
        
        for element in self {
            guard let value = transform(element) else {
                return nil
            }
            results.append(value)
        }
        
        return results
    }
    
    /// Sequence: [T?] -> [T]?
    func sequence<T>() -> [T]? where Element == T? {
        traverse { $0 }
    }
}

// Usage
func safeDivide(_ n: Int) -> Int? {
    n != 0 ? 100 / n : nil
}

let numbers = [1, 2, 5, 10]
let divisions = numbers.traverse(safeDivide)
// Result: [100, 50, 20, 10]

let withZero = [1, 0, 5]
let badDivisions = withZero.traverse(safeDivide)
// Result: nil
```

### Traverse with Async/Await

```swift
extension Array {
    /// Traverse with async operations (sequential)
    func traverse<T>(_ transform: (Element) async throws -> T) async rethrows -> [T] {
        var results: [T] = []
        results.reserveCapacity(count)
        
        for element in self {
            let value = try await transform(element)
            results.append(value)
        }
        
        return results
    }
    
    /// Traverse with async operations (parallel)
    func traverseParallel<T>(_ transform: @escaping (Element) async throws -> T) async rethrows -> [T] {
        try await withThrowingTaskGroup(of: (Int, T).self) { group in
            for (index, element) in enumerated() {
                group.addTask {
                    (index, try await transform(element))
                }
            }
            
            var results: [(Int, T)] = []
            for try await result in group {
                results.append(result)
            }
            
            return results.sorted(by: { $0.0 < $1.0 }).map(\.1)
        }
    }
}

// Usage
func fetchUser(id: Int) async throws -> User {
    // Simulate API call
    try await Task.sleep(nanoseconds: 100_000_000)
    return User(id: id, name: "User \(id)")
}

let userIds = [1, 2, 3, 4, 5]

// Sequential
let users = await userIds.traverse(fetchUser)

// Parallel (faster)
let usersParallel = await userIds.traverseParallel(fetchUser)
```

---

## Part 3: Bow Library

### Overview

**Bow** is a comprehensive functional programming library for Swift, inspired by Haskell and Arrow (Kotlin).

**GitHub**: https://github.com/bow-swift/bow  
**Documentation**: https://bow-swift.io/  
**Status**: Active, mature (3.0.0+)  

### Installation

```swift
// Package.swift
dependencies: [
    .package(url: "https://github.com/bow-swift/bow.git", from: "3.0.0")
]
```

### Bow's Kind<F, A> (HKT Encoding)

Like Arrow, Bow uses `Kind<F, A>` for higher-kinded types:

```swift
public protocol Kind {}

// F is a type constructor witness
// A is the type parameter

// Example: Array
public final class ForArray: Kind {}
public typealias ArrayOf<A> = Kind<ForArray, A>

// Conversion
extension Array {
    func toKind() -> ArrayOf<Element> {
        self as! ArrayOf<Element>
    }
}

func fromKind<A>(_ kind: ArrayOf<A>) -> [A] {
    kind as! [A]
}
```

### Bow's Foldable

```swift
import Bow

public protocol Foldable {
    /// Left fold
    static func foldLeft<A, B>(
        _ fa: Kind<Self, A>,
        _ b: B,
        _ f: @escaping (B, A) -> B
    ) -> B
    
    /// Right fold
    static func foldRight<A, B>(
        _ fa: Kind<Self, A>,
        _ b: Eval<B>,
        _ f: @escaping (A, Eval<B>) -> Eval<B>
    ) -> Eval<B>
    
    /// Fold with monoid
    static func fold<A>(
        _ fa: Kind<Self, A>,
        _ monoid: Monoid<A>
    ) -> A
    
    /// Map then fold
    static func foldMap<A, B>(
        _ fa: Kind<Self, A>,
        _ monoid: Monoid<B>,
        _ f: @escaping (A) -> B
    ) -> B
}

// Array conforms to Foldable
extension Array: Foldable {
    public static func foldLeft<A, B>(
        _ fa: ArrayOf<A>,
        _ b: B,
        _ f: @escaping (B, A) -> B
    ) -> B {
        fromKind(fa).reduce(b, f)
    }
    
    public static func foldRight<A, B>(
        _ fa: ArrayOf<A>,
        _ b: Eval<B>,
        _ f: @escaping (A, Eval<B>) -> Eval<B>
    ) -> Eval<B> {
        fromKind(fa).reversed().reduce(b) { f($1, $0) }
    }
}

// Usage with Bow
import Bow

let numbers: ArrayOf<Int> = [1, 2, 3, 4, 5].toKind()
let sum = Array.foldLeft(numbers, 0, +)  // 15
```

### Bow's Traverse

```swift
import Bow

public protocol Traverse: Functor, Foldable {
    /// Traverse with applicative
    static func traverse<G: Applicative, A, B>(
        _ fa: Kind<Self, A>,
        _ f: @escaping (A) -> Kind<G, B>
    ) -> Kind<G, Kind<Self, B>>
    
    /// Sequence
    static func sequence<G: Applicative, A>(
        _ fga: Kind<Self, Kind<G, A>>
    ) -> Kind<G, Kind<Self, A>>
}

// Usage with Either
import Bow

let numbers = [1, 2, 3, 4, 5]

func validate(_ n: Int) -> Either<String, Int> {
    n > 0 ? .right(n) : .left("Negative: \(n)")
}

let validated: Either<String, [Int]> = numbers.traverse(validate)
// Result: .right([1, 2, 3, 4, 5])
```

### Bow's Data Types

Bow provides many FP data types:

```swift
// Either
let right: Either<String, Int> = .right(42)
let left: Either<String, Int> = .left("error")

// Option (like Swift's Optional)
let some: Option<Int> = .some(42)
let none: Option<Int> = .none

// Try (like Result but with Error)
let success: Try<Int> = .success(42)
let failure: Try<Int> = .failure(MyError())

// IO (for side effects)
let io: IO<String> = IO.pure("Hello")

// Validated (accumulates errors)
typealias ValidationResult<A> = Validated<[String], A>
let valid: ValidationResult<Int> = .valid(42)
let invalid: ValidationResult<Int> = .invalid(["error1", "error2"])
```

---

## Comparison: Native vs Bow

| Feature | Native Swift | Bow Library |
|---------|--------------|-------------|
| **Foldable** | ⭐⭐⭐⭐⭐ (reduce) | ⭐⭐⭐⭐⭐ (full typeclass) |
| **Traversable** | ⭐⭐⭐ (manual impl) | ⭐⭐⭐⭐⭐ (full typeclass) |
| **HKT** | ❌ No | ✅ Kind<F, A> |
| **Learning Curve** | Easy | Steep |
| **Performance** | Excellent | Good |
| **Async Support** | ⭐⭐⭐⭐⭐ (async/await) | ⭐⭐⭐⭐ (IO monad) |
| **Type Inference** | Excellent | Moderate |
| **Community** | Huge | Small but active |

---

## Real-World Patterns

### Pattern 1: Form Validation (Native)

```swift
struct User {
    let name: String
    let email: String
    let age: Int
}

enum ValidationError: Error {
    case invalidName
    case invalidEmail
    case invalidAge
}

func validateName(_ name: String) -> Result<String, ValidationError> {
    name.isEmpty ? .failure(.invalidName) : .success(name)
}

func validateEmail(_ email: String) -> Result<String, ValidationError> {
    email.contains("@") ? .success(email) : .failure(.invalidEmail)
}

func validateAge(_ age: Int) -> Result<Int, ValidationError> {
    (0...150).contains(age) ? .success(age) : .failure(.invalidAge)
}

// Validate multiple users
let userData = [
    ("Alice", "alice@example.com", 30),
    ("Bob", "bob@example.com", 25),
    ("Charlie", "charlie@example.com", 35)
]

let validatedUsers = userData.traverse { (name, email, age) in
    validateName(name).flatMap { validName in
        validateEmail(email).flatMap { validEmail in
            validateAge(age).map { validAge in
                User(name: validName, email: validEmail, age: validAge)
            }
        }
    }
}
// Result: .success([User...])
```

### Pattern 2: Parallel API Calls (Native async/await)

```swift
struct Post {
    let id: Int
    let title: String
}

func fetchPost(id: Int) async throws -> Post {
    // Simulate API call
    try await Task.sleep(nanoseconds: 100_000_000)
    return Post(id: id, title: "Post \(id)")
}

let postIds = [1, 2, 3, 4, 5]

// Parallel fetch
let posts = await postIds.traverseParallel(fetchPost)
// All fetched in parallel, ~100ms total
```

### Pattern 3: SwiftUI Validation

```swift
import SwiftUI

class FormViewModel: ObservableObject {
    @Published var fields: [String] = ["", "", ""]
    @Published var validationResult: Result<[String], ValidationError>?
    
    func validate() {
        validationResult = fields.traverse { field in
            field.isEmpty ? .failure(.invalidField) : .success(field)
        }
    }
}

struct FormView: View {
    @StateObject var viewModel = FormViewModel()
    
    var body: some View {
        VStack {
            ForEach(0..<viewModel.fields.count, id: \.self) { index in
                TextField("Field \(index + 1)", text: $viewModel.fields[index])
            }
            
            Button("Validate") {
                viewModel.validate()
            }
            
            if case .success(let values) = viewModel.validationResult {
                Text("Valid: \(values.joined(separator: ", "))")
            } else if case .failure(let error) = viewModel.validationResult {
                Text("Error: \(error.localizedDescription)")
            }
        }
    }
}
```

---

## Recommendations

### Use Native Swift When:
✅ Simple reduce/fold operations  
✅ Standard collections (Array, Set, Dictionary)  
✅ Async/await patterns  
✅ SwiftUI integration  
✅ Performance is critical  
✅ Team unfamiliar with FP  

### Use Bow Library When:
✅ Need full typeclass abstraction  
✅ Working across multiple container types  
✅ Complex monad/applicative compositions  
✅ Team experienced with FP (Haskell, Arrow)  
✅ Need HKT-based polymorphism  

### Hybrid Approach (Recommended):
✅ Native Swift for Foldable (reduce works great)  
✅ Native Swift extensions for Traversable (Result, Optional)  
✅ Bow for advanced cases (if team is ready)  

---

## Summary

| Aspect | Native Swift | Bow Library |
|--------|--------------|-------------|
| **Foldable** | ⭐⭐⭐⭐⭐ Excellent | ⭐⭐⭐⭐⭐ Excellent |
| **Traversable** | ⭐⭐⭐⭐ Good (DIY) | ⭐⭐⭐⭐⭐ Excellent |
| **Learning Curve** | ⭐⭐⭐⭐⭐ Easy | ⭐⭐ Steep |
| **Maturity** | ⭐⭐⭐⭐⭐ Very mature | ⭐⭐⭐⭐ Mature |
| **Documentation** | ⭐⭐⭐⭐⭐ Excellent | ⭐⭐⭐⭐ Good |
| **Community** | ⭐⭐⭐⭐⭐ Huge | ⭐⭐⭐ Small |

**Final Recommendation**:
- **Start with native Swift** - reduce, custom traverse extensions
- **Consider Bow** for advanced FP teams or complex requirements
- **Hybrid approach** works best for most projects

---

**Status**: ✅ Research Complete  
**Next**: Create library comparison table

