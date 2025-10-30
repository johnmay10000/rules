# Swift Functional Programming Style Guide
## For iOS, macOS, and SwiftUI Projects

### Core Principles

1. **Immutability by Default**: Use `let` over `var`, value types over reference types
2. **Pure Functions**: Functions should have no side effects unless explicitly marked
3. **Type Safety**: Leverage Swift's powerful type system
4. **Composability**: Build complex operations from small, composable functions
5. **Explicit Error Handling**: Use `Result` instead of throwing

---

## Required Libraries

```swift
// Core Swift provides good FP primitives
// Optional but recommended for advanced patterns:

// Swift Composable Architecture (TCA) - for app architecture
import ComposableArchitecture

// PointFree's libraries
import Overture         // Function composition
import Tagged           // Type-safe identifiers

// Optional: More functional utilities
import Bow              // FP library (like fp-ts for Swift)
```

**Note:** Swift has excellent built-in FP support. External libraries are optional for most use cases.

---

## 1. Error Handling with Result

### ❌ Avoid: Throwing Errors
```swift
func divide(_ a: Double, by b: Double) throws -> Double {
    guard b != 0 else {
        throw DivisionError.divideByZero
    }
    return a / b
}
```

### ✅ Prefer: Result Type
```swift
enum DivisionError: Error {
    case divideByZero
    case invalidInput
}

func divide(_ a: Double, by b: Double) -> Result<Double, DivisionError> {
    guard b != 0 else {
        return .failure(.divideByZero)
    }
    return .success(a / b)
}

// Or with computed property
extension Double {
    func divided(by divisor: Double) -> Result<Double, DivisionError> {
        guard divisor != 0 else {
            return .failure(.divideByZero)
        }
        return .success(self / divisor)
    }
}
```

---

## 2. Monadic Composition (Do Notation Style)

### ✅ Chain Operations with flatMap
```swift
enum ValidationError: Error {
    case notPositive
    case divisionByZero
}

func validatePositive(_ x: Double) -> Result<Double, ValidationError> {
    x > 0 ? .success(x) : .failure(.notPositive)
}

func safeSqrt(_ x: Double) -> Result<Double, ValidationError> {
    .success(x.squareRoot())
}

func safeReciprocal(_ x: Double) -> Result<Double, ValidationError> {
    x == 0 ? .failure(.divisionByZero) : .success(1 / x)
}

// Monadic composition - like Haskell's do notation
func processValue(_ x: Double) -> Result<Double, ValidationError> {
    validatePositive(x)
        .flatMap(safeSqrt)
        .flatMap(safeReciprocal)
        .map { $0 * 100 }
}

// Usage
let result = processValue(16.0)
// .success(25.0)
```

### Custom Result Extensions
```swift
extension Result {
    // Bind operator for more Haskell-like syntax
    static func >>- <T>(
        lhs: Result<Success, Failure>,
        rhs: (Success) -> Result<T, Failure>
    ) -> Result<T, Failure> {
        lhs.flatMap(rhs)
    }
    
    // Functor map operator
    static func <^> <T>(
        f: (Success) -> T,
        result: Result<Success, Failure>
    ) -> Result<T, Failure> {
        result.map(f)
    }
    
    // Applicative apply
    func apply<T>(_ transform: Result<(Success) -> T, Failure>) -> Result<T, Failure> {
        transform.flatMap { f in
            self.map(f)
        }
    }
}

// Now you can write
let result = validatePositive(16.0)
    >>- safeSqrt
    >>- safeReciprocal
    >>- { .success($0 * 100) }
```

---

## 3. Immutable Data Structures

### ✅ Use Structs and Let
```swift
struct User {
    let id: UUID
    let name: String
    let email: String
    let age: Int
    
    // Return new instance for updates
    func withName(_ newName: String) -> User {
        User(id: id, name: newName, email: email, age: age)
    }
    
    func withEmail(_ newEmail: String) -> User {
        User(id: id, name: name, email: newEmail, age: age)
    }
}

// Or use functional setters
extension User {
    func updating<T>(_ keyPath: WritableKeyPath<User, T>, to value: T) -> User {
        var copy = self
        copy[keyPath: keyPath] = value
        return copy
    }
}

// Usage
let user = User(id: UUID(), name: "Alice", email: "alice@example.com", age: 30)
let updated = user.updating(\.name, to: "Alice Smith")
```

### Lenses (Advanced)
```swift
// Lens for deep immutable updates
struct Lens<Whole, Part> {
    let get: (Whole) -> Part
    let set: (Part, Whole) -> Whole
    
    func modify(_ transform: @escaping (Part) -> Part) -> (Whole) -> Whole {
        { whole in
            self.set(transform(self.get(whole)), whole)
        }
    }
}

// Example with nested structure
struct Address {
    let street: String
    let city: String
}

struct UserProfile {
    let user: User
    let address: Address
}

// Define lenses
let userLens = Lens<UserProfile, User>(
    get: { $0.user },
    set: { User(id: $1.user.id, name: $0.name, email: $0.email, age: $0.age) }
)

let addressLens = Lens<UserProfile, Address>(
    get: { $0.address },
    set: { address, profile in UserProfile(user: profile.user, address: address) }
)

// Compose lenses
let streetLens = addressLens.compose(
    Lens<Address, String>(
        get: { $0.street },
        set: { street, address in Address(street: street, city: address.city) }
    )
)
```

---

## 4. Function Composition

### ✅ Build Complex Pipelines
```swift
// Composition operator
precedencegroup CompositionPrecedence {
    associativity: left
}

infix operator >>>: CompositionPrecedence

func >>> <A, B, C>(
    _ f: @escaping (A) -> B,
    _ g: @escaping (B) -> C
) -> (A) -> C {
    { g(f($0)) }
}

// Pipe operator (reverse composition)
infix operator |>: CompositionPrecedence

func |> <A, B>(_ value: A, _ f: (A) -> B) -> B {
    f(value)
}

// Example usage
let normalize: (Double) -> Double = { $0 / 255.0 }
let sigmoid: (Double) -> Double = { 1 / (1 + exp(-$0)) }
let scale: (Double) -> Double = { $0 * 100 }

let pipeline = normalize >>> sigmoid >>> scale

let result = pipeline(200.0)
// Or with pipe
let result2 = 200.0 |> normalize |> sigmoid |> scale
```

---

## 5. Currying and Partial Application

### ✅ Curry Functions for Composition
```swift
// Manual currying
func curry<A, B, C>(_ f: @escaping (A, B) -> C) -> (A) -> (B) -> C {
    { a in { b in f(a, b) } }
}

func curry<A, B, C, D>(_ f: @escaping (A, B, C) -> D) -> (A) -> (B) -> (C) -> D {
    { a in { b in { c in f(a, b, c) } } }
}

// Example: String formatting
func formatString(_ template: String, _ value: String) -> String {
    template.replacingOccurrences(of: "{}", with: value)
}

let curriedFormat = curry(formatString)
let greetingFormatter = curriedFormat("Hello, {}!")
let fareewellFormatter = curriedFormat("Goodbye, {}!")

print(greetingFormatter("Alice"))  // "Hello, Alice!"
print(fareewellFormatter("Bob"))   // "Goodbye, Bob!"

// Map operations
let names = ["Alice", "Bob", "Charlie"]
let greetings = names.map(greetingFormatter)
```

---

## 6. Railway-Oriented Programming

### ✅ Chain Operations with Result
```swift
enum NetworkError: Error {
    case invalidURL
    case requestFailed(String)
    case decodingError(String)
    case notFound
}

struct User: Codable {
    let id: String
    let name: String
    let email: String
}

// Individual steps
func validateURL(_ string: String) -> Result<URL, NetworkError> {
    guard let url = URL(string: string) else {
        return .failure(.invalidURL)
    }
    return .success(url)
}

func fetchData(from url: URL) -> Result<Data, NetworkError> {
    // In real code, this would be async
    // For now, simulating synchronous version
    guard let data = try? Data(contentsOf: url) else {
        return .failure(.requestFailed("Network request failed"))
    }
    return .success(data)
}

func decodeUser(from data: Data) -> Result<User, NetworkError> {
    do {
        let user = try JSONDecoder().decode(User.self, from: data)
        return .success(user)
    } catch {
        return .failure(.decodingError(error.localizedDescription))
    }
}

// Composed pipeline
func fetchUser(from urlString: String) -> Result<User, NetworkError> {
    validateURL(urlString)
        .flatMap(fetchData)
        .flatMap(decodeUser)
}

// Pattern matching on result
switch fetchUser(from: "https://api.example.com/user/123") {
case .success(let user):
    print("Fetched user: \(user.name)")
case .failure(let error):
    print("Error: \(error)")
}
```

---

## 7. Async/Await with Result

### ✅ Combine Result with Modern Concurrency
```swift
// Async Result type
typealias AsyncResult<Success, Failure: Error> = () async -> Result<Success, Failure>

// Or use async throws with Result wrapper
extension Result {
    init(catching body: () async throws -> Success) async where Failure == Error {
        do {
            self = .success(try await body())
        } catch {
            self = .failure(error)
        }
    }
}

// Example: Async network operations
func fetchUserAsync(id: String) async -> Result<User, NetworkError> {
    await Result {
        guard let url = URL(string: "https://api.example.com/user/\(id)") else {
            throw NetworkError.invalidURL
        }
        
        let (data, response) = try await URLSession.shared.data(from: url)
        
        guard let httpResponse = response as? HTTPURLResponse,
              httpResponse.statusCode == 200 else {
            throw NetworkError.notFound
        }
        
        return try JSONDecoder().decode(User.self, from: data)
    }
}

// Usage with async/await
Task {
    let result = await fetchUserAsync(id: "123")
    
    switch result {
    case .success(let user):
        print("User: \(user.name)")
    case .failure(let error):
        print("Error: \(error)")
    }
}

// Chain async operations
func fetchUserProfile(id: String) async -> Result<(User, [Post]), NetworkError> {
    let userResult = await fetchUserAsync(id: id)
    
    switch userResult {
    case .success(let user):
        let postsResult = await fetchUserPosts(userId: user.id)
        return postsResult.map { posts in (user, posts) }
    case .failure(let error):
        return .failure(error)
    }
}
```

---

## 8. Pattern Matching for ADTs

### ✅ Use Enums with Associated Values
```swift
// ADT for remote data
enum RemoteData<Value, Error> {
    case notAsked
    case loading
    case failure(Error)
    case success(Value)
    
    // Functor
    func map<T>(_ transform: (Value) -> T) -> RemoteData<T, Error> {
        switch self {
        case .notAsked: return .notAsked
        case .loading: return .loading
        case .failure(let error): return .failure(error)
        case .success(let value): return .success(transform(value))
        }
    }
    
    // Monad
    func flatMap<T>(_ transform: (Value) -> RemoteData<T, Error>) -> RemoteData<T, Error> {
        switch self {
        case .notAsked: return .notAsked
        case .loading: return .loading
        case .failure(let error): return .failure(error)
        case .success(let value): return transform(value)
        }
    }
}

// Usage with SwiftUI
struct UserListView: View {
    @State private var users: RemoteData<[User], NetworkError> = .notAsked
    
    var body: some View {
        Group {
            switch users {
            case .notAsked:
                Button("Load Users") {
                    loadUsers()
                }
            case .loading:
                ProgressView("Loading...")
            case .failure(let error):
                VStack {
                    Text("Error: \(error.localizedDescription)")
                    Button("Retry") {
                        loadUsers()
                    }
                }
            case .success(let userList):
                List(userList, id: \.id) { user in
                    Text(user.name)
                }
            }
        }
    }
    
    private func loadUsers() {
        users = .loading
        Task {
            let result = await fetchUsersAsync()
            users = result.fold(
                onSuccess: { .success($0) },
                onFailure: { .failure($0) }
            )
        }
    }
}

// Helper for Result to RemoteData
extension Result {
    func fold<T>(
        onSuccess: (Success) -> T,
        onFailure: (Failure) -> T
    ) -> T {
        switch self {
        case .success(let value):
            return onSuccess(value)
        case .failure(let error):
            return onFailure(error)
        }
    }
}
```

---

## 9. Higher-Order Functions and Abstractions

### ✅ Generic Functional Patterns
```swift
// Traverse for Result
extension Sequence {
    func traverse<T, E>(
        _ transform: (Element) -> Result<T, E>
    ) -> Result<[T], E> {
        var results: [T] = []
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
}

// Usage
func validatePositiveInt(_ x: Int) -> Result<Int, ValidationError> {
    x > 0 ? .success(x) : .failure(.notPositive)
}

let numbers = [1, 2, 3, 4, 5]
let validated = numbers.traverse(validatePositiveInt)
// .success([1, 2, 3, 4, 5])

let badNumbers = [1, -2, 3]
let invalidated = badNumbers.traverse(validatePositiveInt)
// .failure(.notPositive)

// Sequence for Result
extension Sequence where Element: ResultProtocol {
    func sequence() -> Result<[Element.Success], Element.Failure> {
        traverse { $0 }
    }
}

protocol ResultProtocol {
    associatedtype Success
    associatedtype Failure: Error
    func flatMap<T>(_ transform: (Success) -> Result<T, Failure>) -> Result<T, Failure>
}

extension Result: ResultProtocol {}
```

---

## 10. Optics (Lenses and Prisms)

### ✅ Type-Safe Deep Updates
```swift
// Simple lens implementation
struct Lens<Whole, Part> {
    let get: (Whole) -> Part
    let set: (Part, Whole) -> Whole
    
    func modify(_ transform: @escaping (Part) -> Part) -> (Whole) -> Whole {
        { whole in
            self.set(transform(self.get(whole)), whole)
        }
    }
    
    func compose<SubPart>(_ other: Lens<Part, SubPart>) -> Lens<Whole, SubPart> {
        Lens<Whole, SubPart>(
            get: { whole in other.get(self.get(whole)) },
            set: { subpart, whole in
                self.set(other.set(subpart, self.get(whole)), whole)
            }
        )
    }
}

// Key path based lens (Swift native)
extension Lens {
    init(_ keyPath: WritableKeyPath<Whole, Part>) {
        self.init(
            get: { $0[keyPath: keyPath] },
            set: { part, whole in
                var copy = whole
                copy[keyPath: keyPath] = part
                return copy
            }
        )
    }
}

// Prism for sum types
struct Prism<Whole, Part> {
    let preview: (Whole) -> Part?
    let review: (Part) -> Whole
    
    func modify(_ transform: @escaping (Part) -> Part) -> (Whole) -> Whole {
        { whole in
            guard let part = self.preview(whole) else {
                return whole
            }
            return self.review(transform(part))
        }
    }
}

// Example with RemoteData
let successPrism = Prism<RemoteData<Int, NetworkError>, Int>(
    preview: {
        if case .success(let value) = $0 {
            return value
        }
        return nil
    },
    review: { .success($0) }
)

let data: RemoteData<Int, NetworkError> = .success(42)
let modified = successPrism.modify { $0 * 2 }(data)
// .success(84)
```

---

## Complete Example: SwiftUI App with FP

```swift
import SwiftUI
import Combine

// MARK: - Domain Types

struct User: Codable, Identifiable {
    let id: String
    let name: String
    let email: String
    let age: Int
}

enum NetworkError: Error, LocalizedError {
    case invalidURL
    case requestFailed(String)
    case decodingError(String)
    case notFound
    
    var errorDescription: String? {
        switch self {
        case .invalidURL:
            return "Invalid URL"
        case .requestFailed(let message):
            return "Request failed: \(message)"
        case .decodingError(let message):
            return "Decoding error: \(message)"
        case .notFound:
            return "Resource not found"
        }
    }
}

// MARK: - Remote Data ADT

enum RemoteData<Value, Error> {
    case notAsked
    case loading
    case failure(Error)
    case success(Value)
    
    var value: Value? {
        if case .success(let v) = self { return v }
        return nil
    }
    
    var error: Error? {
        if case .failure(let e) = self { return e }
        return nil
    }
    
    var isLoading: Bool {
        if case .loading = self { return true }
        return false
    }
}

// MARK: - API Layer

struct UserAPI {
    // Pure function - returns Result
    static func fetchUser(id: String) async -> Result<User, NetworkError> {
        await Result {
            guard let url = URL(string: "https://api.example.com/users/\(id)") else {
                throw NetworkError.invalidURL
            }
            
            let (data, response) = try await URLSession.shared.data(from: url)
            
            guard let httpResponse = response as? HTTPURLResponse else {
                throw NetworkError.requestFailed("Invalid response")
            }
            
            guard httpResponse.statusCode == 200 else {
                throw NetworkError.notFound
            }
            
            return try JSONDecoder().decode(User.self, from: data)
        }
    }
    
    static func fetchUsers() async -> Result<[User], NetworkError> {
        await Result {
            guard let url = URL(string: "https://api.example.com/users") else {
                throw NetworkError.invalidURL
            }
            
            let (data, _) = try await URLSession.shared.data(from: url)
            return try JSONDecoder().decode([User].self, from: data)
        }
    }
}

// MARK: - View Model

@MainActor
class UserListViewModel: ObservableObject {
    @Published private(set) var users: RemoteData<[User], NetworkError> = .notAsked
    @Published var searchText: String = ""
    
    // Pure computed property
    var filteredUsers: [User] {
        guard let userList = users.value else { return [] }
        
        guard !searchText.isEmpty else { return userList }
        
        return userList.filter { user in
            user.name.localizedCaseInsensitiveContains(searchText) ||
            user.email.localizedCaseInsensitiveContains(searchText)
        }
    }
    
    // Effects are isolated to specific methods
    func loadUsers() {
        users = .loading
        
        Task {
            let result = await UserAPI.fetchUsers()
            
            // Map Result to RemoteData
            users = result.fold(
                onSuccess: { .success($0) },
                onFailure: { .failure($0) }
            )
        }
    }
    
    func selectUser(id: String) {
        // Handle user selection
    }
}

// MARK: - Views

struct UserListView: View {
    @StateObject private var viewModel = UserListViewModel()
    
    var body: some View {
        NavigationView {
            content
                .navigationTitle("Users")
                .searchable(text: $viewModel.searchText)
                .toolbar {
                    if !viewModel.users.isLoading {
                        Button("Refresh") {
                            viewModel.loadUsers()
                        }
                    }
                }
        }
        .task {
            if case .notAsked = viewModel.users {
                viewModel.loadUsers()
            }
        }
    }
    
    @ViewBuilder
    private var content: some View {
        switch viewModel.users {
        case .notAsked:
            ContentUnavailableView(
                "No Users",
                systemImage: "person.3",
                description: Text("Tap to load users")
            )
            
        case .loading:
            ProgressView("Loading users...")
            
        case .failure(let error):
            ContentUnavailableView(
                "Error",
                systemImage: "exclamationmark.triangle",
                description: Text(error.localizedDescription)
            )
            
        case .success:
            List(viewModel.filteredUsers) { user in
                UserRowView(user: user)
            }
        }
    }
}

struct UserRowView: View {
    let user: User
    
    var body: some View {
        VStack(alignment: .leading, spacing: 4) {
            Text(user.name)
                .font(.headline)
            Text(user.email)
                .font(.subheadline)
                .foregroundColor(.secondary)
        }
        .padding(.vertical, 4)
    }
}

// MARK: - Result Extensions

extension Result {
    func fold<T>(
        onSuccess: (Success) -> T,
        onFailure: (Failure) -> T
    ) -> T {
        switch self {
        case .success(let value):
            return onSuccess(value)
        case .failure(let error):
            return onFailure(error)
        }
    }
}
```

---

## Style Rules Summary

1. **No throwing**: Use `Result` type instead of `throw`
2. **Use `let` over `var`**: Immutability by default
3. **Value types first**: Prefer `struct` over `class`
4. **Explicit effects**: Mark side effects clearly with async/Task
5. **Compose functions**: Use `>>>` and `|>` operators
6. **Curry when useful**: Enable partial application
7. **Type everything**: Leverage Swift's type inference, but be explicit in signatures
8. **Pattern match**: Use `switch` exhaustively on enums
9. **Small functions**: Each function does one thing
10. **Railway-oriented**: Chain Result, fail fast, handle at boundaries

---

## File Organization

```
Project/
├── Domain/
│   ├── Models/
│   │   ├── User.swift
│   │   └── Post.swift
│   └── Types/
│       ├── RemoteData.swift
│       └── Result+Extensions.swift
├── Data/
│   ├── API/
│   │   ├── UserAPI.swift
│   │   └── PostAPI.swift
│   └── Repository/
│       └── UserRepository.swift
├── Presentation/
│   ├── ViewModels/
│   │   └── UserListViewModel.swift
│   └── Views/
│       └── UserListView.swift
└── Core/
    ├── FP/
    │   ├── Operators.swift
    │   ├── Lens.swift
    │   └── Composition.swift
    └── Extensions/
        └── Result+Utilities.swift
```

---

## Tooling

```swift
// Package.swift
// swift-tools-version:5.9
import PackageDescription

let package = Package(
    name: "MyApp",
    platforms: [.iOS(.v17), .macOS(.v14)],
    dependencies: [
        // Optional FP libraries
        .package(url: "https://github.com/pointfreeco/swift-composable-architecture", from: "1.0.0"),
        .package(url: "https://github.com/bow-swift/bow", from: "0.8.0"),
    ]
)
```

This guide transforms Swift into a strongly typed, pure functional language similar to Haskell while leveraging Swift's modern features like async/await and SwiftUI.
