# Swift Functional Programming Style Guide for Kimi CLI

**Version**: 2.0.0  
**Last Updated**: 2025-11-14  
**Part of**: [KIMI.md](KIMI.md) Global Rule Set  
**Target**: Swift projects (iOS, macOS, SwiftUI, server-side)

> **📖 Global Rules**: This document extends [KIMI.md](KIMI.md) with Swift-specific guidance. For mandatory universal rules (Git, documentation, testing, file size), see [KIMI.md](KIMI.md).

---

## Quick Links

- **Mandatory Rules**: See [KIMI.md](KIMI.md) sections 1-4  
- **FP Principles Deep Dive**: See [KIMI_FP_PRINCIPLES.md](KIMI_FP_PRINCIPLES.md)  
- **Workflow Guide**: See [KIMI_WORKFLOW_GUIDE.md](KIMI_WORKFLOW_GUIDE.md)  
- **Integration**: See [KIMI.md Integration](#kimi-integration) below  

---

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
// Core Swift provides excellent FP primitives
// Optional but recommended for advanced patterns:

// Swift Composable Architecture (TCA) - for app architecture
import ComposableArchitecture

// PointFree's libraries
import Overture         // Function composition
import Tagged           // Type-safe identifiers

// Optional: More functional utilities
import Bow              // FP library (like fp-ts for Swift)
```

**Note**: Swift has excellent built-in FP support. External libraries are optional for most use cases.

---

## Kimi-Specific Patterns

### Parallel Validation of Swift Code

Kimi can validate multiple Swift functions simultaneously using parallel tool calls:

```swift
// Kimi excels at validating Swift's value types and pure functions
struct User {
    let id: User.ID
    let name: String
    let email: String
}

// Extension with custom validation (Kimi validates all simultaneously)
extension User {
    func validate() -> ValidationResult {
        // Kimi can validate: ✓ id format
        //                  ✓ name non-empty
        //                  ✓ email format
        // All validations in parallel
    }
    
    func update(name: String) -> User {
        User(id: self.id, name: name, email: self.email)
        // Kimi validates: ✓ Immutability preserved
        //                 ✓ Returns new instance
    }
}
```

### Subagent Pattern for Complex SwiftUI Validations

Use Kimi's Task tool to spawn subagents for validating SwiftUI components:

```swift
// Complex SwiftUI view that benefits from Kimi's subagent validation
struct UserProfileView: View {
    let store: StoreOf<UserProfile>
    
    var body: some View {
        WithViewStore(self.store) { viewStore in
            VStack {
                Text(viewStore.name)
                    .font(.title)
                
                Text(viewStore.email)
                    .font(.subheadline)
                
                Button("Save") {
                    viewStore.send(.saveButtonTapped)
                }
            }
        }
    }
}

// Kimi can spawn subagents to verify:
// - State management correctness
// - Action handling
// - View updates on state changes
```

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
extension Result {
    var value: Success? {
        try? get()
    }
    
    var error: Failure? {
        guard case .failure(let error) = self else { return nil }
        return error
    }
}
```

### ✅ Chain Results (Railway Pattern)
```swift
// Kimi validates railway composition
func processUserData(_ raw: String) -> Result<User, ProcessingError> {
    parseJson(raw)
        .flatMap(validateSchema)
        .flatMap(enrichWithMetadata)
        .map(transformToUser)
    // Kimi can validate each step in parallel
}

// Helper functions with explicit types
func parseJson(_ raw: String) -> Result<JSON, ParseError>
func validateSchema(_ json: JSON) -> Result<ValidatedData, ValidationError>
func enrichWithMetadata(_ data: ValidatedData) -> Result<EnrichedData, EnrichmentError>
func transformToUser(_ enriched: EnrichedData) -> User
```

---

## 2. Immutable Value Types

```swift
// Immutable struct (value semantics)
struct User {
    let id: UUID
    let name: String
    let email: String
    let preferences: UserPreferences
}

struct UserPreferences {
    let notifications: Bool
    let theme: Theme
    let language: Language
}

enum Theme { case light, dark, system }
enum Language { case english, spanish, french }

// Updating immutable state (returns new instance)
extension User {
    func withEmail(_ newEmail: String) -> User {
        User(
            id: self.id,
            name: self.name,
            email: newEmail,
            preferences: self.preferences
        )
        // Kimi validates: ✓ All properties copied
        //                 ✓ Only email changed
        //                 ✓ Immutability preserved
    }
    
    func updatingPreferences(_ updates: (UserPreferences) -> UserPreferences) -> User {
        User(
            id: self.id,
            name: self.name,
            email: self.email,
            preferences: updates(self.preferences)
        )
    }
}

// Usage
let user = User(id: UUID(), name: "Alice", email: "alice@example.com", preferences: UserPreferences(...))
let updatedUser = user.withEmail("new@example.com")
// Original user unchanged ✓
```

---

## 3. Functional Composables (SwiftUI)

```swift
// Kimi can validate SwiftUI view composition in parallel
struct ContentView: View {
    let store: StoreOf<App>
    
    var body: some View {
        NavigationView {
            ListStore(
                store: self.store.scope(
                    state: \.users,
                    action: App.Action.users
                )
            ) { user in
                UserRow(user: user)  // Kimi validates this subview
            }
            .navigationTitle("Users")
            .toolbar {
                ToolbarItem(placement: .navigationBarTrailing) {
                    Button(action: { viewStore.send(.addUser) }) {
                        Image(systemName: "plus")
                    }
                }
            }
        }
        // Kimi can spawn subagents to validate each view component
    }
}

// Reusable components
struct UserRow: View {
    let user: User
    
    var body: some View {
        HStack {
            Text(user.name)
                .font(.headline)
            
            Spacer()
            
            Text(user.email)
                .font(.caption)
                .foregroundColor(.gray)
        }
    }
}
```

---

## 4. Swift Composable Architecture (TCA)

```swift
import ComposableArchitecture

// State (immutable)
struct UserProfile: Equatable {
    var user: User?
    var isLoading = false
    var errorMessage: String?
}

// Actions (sum type/enum)
enum UserProfileAction: Equatable {
    case onAppear
    case userResponse(Result<User, UserError>)
    case saveButtonTapped
    case saveResponse(Result<Void, UserError>)
}

// Environment (dependencies)
struct UserProfileEnvironment {
    var fetchUser: (User.ID) -> EffectPublisher<User, UserError>
    var saveUser: (User) -> EffectPublisher<Void, UserError>
    var mainQueue: AnySchedulerOf<DispatchQueue>
}

// Reducer (pure function)
let userProfileReducer = Reducer<
    UserProfile,
    UserProfileAction,
    UserProfileEnvironment
> { state, action, environment in
    switch action {
    case .onAppear:
        state.isLoading = true
        state.errorMessage = nil
        
        return environment.fetchUser(state.user?.id ?? UUID())
            .receive(on: environment.mainQueue)
            .catchToEffect(UserProfileAction.userResponse)
        // Kimi validates: ✓ EffectPublisher usage
        //                 ✓ Error handling
        //                 ✓ State updates
    
    case let .userResponse(.success(user)):
        state.user = user
        state.isLoading = false
        return .none
    
    case let .userResponse(.failure(error)):
        state.isLoading = false
        state.errorMessage = error.localizedDescription
        return .none
    
    case .saveButtonTapped:
        guard let user = state.user else { return .none }
        
        return environment.saveUser(user)
            .receive(on: environment.mainQueue)
            .catchToEffect(UserProfileAction.saveResponse)
    
    case .saveResponse:
        // Handle save response
        return .none
    }
    // Kimi validates: ✓ All actions handled
    //                 ✓ State mutations are proper
    //                 ✓ Effects are properly returned
}
```

---

## 5. Function Composition

```swift
// Using Overture library or custom operators
import Overture

// Small, composable functions (Kimi validates each)
func toUpper(_ s: String) -> String { s.uppercased() }
func exclaim(_ s: String) -> String { "\(s)!" }
func addPrefix(_ prefix: String) -> (String) -> String {
    { "\(prefix) \($0)" }
}

// Composition
let formatUserName = pipe(
    toUpper,
    exclaim,
    addPrefix("Welcome")
)

// Or with custom operators
precedencegroup ForwardComposition {
    associativity: left
}

infix operator >>>: ForwardComposition

func >>> <A, B, C>(
    f: @escaping (A) -> B,
    g: @escaping (B) -> C
) -> (A) -> C {
    { g(f($0)) }
}

let formatUserName = toUpper >>> exclaim >>> addPrefix("Welcome")

// Usage
let name = formatUserName("alice")  // "WELCOME ALICE!"
```

---

## 6. Tagged Types for Type Safety

```swift
import Tagged

// Type-safe identifiers (Kimi validates no mixing)
typealias UserID = Tagged<User, UUID>
typealias PostID = Tagged<Post, UUID>
typealias Email = Tagged<EmailValidation, String>

struct User {
    let id: UserID
    let name: String
    let email: Email
}

struct Post {
    let id: PostID
    let userID: UserID
    let content: String
}

// Cannot accidentally mix IDs!
let userID: UserID = UserID(uuidString: "...")!
let postID: PostID = PostID(uuidString: "...")!

// userID == postID  // Compile error! ✓
```

---

## 7. Optional Pattern Matching

```swift
// Explicit optional handling (no force unwrapping!)
func getUserAvatarURL(_ user: User?) -> URL? {
    guard let user = user,
          let urlString = user.profile?.avatarURL,
          let url = URL(string: urlString) else {
        return nil
    }
    return url
}

// Or with Result
func getUserAvatarURL(_ user: User?) -> Result<URL, UserError> {
    guard let user = user else {
        return .failure(.userNotFound)
    }
    
    guard let urlString = user.profile?.avatarURL else {
        return .failure(.noAvatar)
    }
    
    guard let url = URL(string: urlString) else {
        return .failure(.invalidURL)
    }
    
    return .success(url)
}

// Railway pattern with optionals
extension Optional {
    func flatMap<U>(_ transform: (Wrapped) -> U?) -> U? {
        switch self {
        case .none: return nil
        case .some(let value): return transform(value)
        }
    }
    
    func map<U>(_ transform: (Wrapped) -> U) -> U? {
        switch self {
        case .none: return nil
        case .some(let value): return transform(value)
        }
    }
}
```

---

## 8. Async FP with Combine

```swift
import Combine

// FP-style Combine pipeline
class UserService {
    private let apiClient: APIClient
    
    func fetchUser(_ id: User.ID) -> AnyPublisher<User, APIError> {
        apiClient.request(.getUser(id))
            .map { data -> User in
                // Decode and validate
                let decoder = JSONDecoder()
                let user = try decoder.decode(User.self, from: data)
                
                // Kimi validates: ✓ Type safety
                //                 ✓ Error handling
                //                 ✓ Validation logic
                
                return user
            }
            .mapError { error in
                // Transform to domain error
                APIError.network(error)
            }
            .eraseToAnyPublisher()
    }
    
    func fetchUserWithPosts(_ id: User.ID) -> AnyPublisher<(User, [Post]), APIError> {
        Publishers.Zip(
            fetchUser(id),
            fetchPosts(for: id)
        )
        .eraseToAnyPublisher()
        // Kimi validates: ✓ Parallel composition
        //                 ✓ Error handling
        //                 ✓ Type matching
    }
    
    private func fetchPosts(for userID: User.ID) -> AnyPublisher<[Post], APIError> {
        apiClient.request(.getPosts(userID: userID))
            .map { data -> [Post] in
                let decoder = JSONDecoder()
                return try decoder.decode([Post].self, from: data)
            }
            .mapError(APIError.network)
            .eraseToAnyPublisher()
    }
}

// Usage
let userService = UserService()

userService.fetchUserWithPosts(userID)
    .sink(
        receiveCompletion: { completion in
            switch completion {
            case .finished:
                print("✓ Completed")
            case .failure(let error):
                print("✗ Error: \(error)")
            }
        },
        receiveValue: { user, posts in
            // Kimi validates: ✓ Value handling
            //                 ✓ Type safety
            print("User: \(user.name), Posts: \(posts.count)")
        }
    )
    .store(in: &cancellables)
```

---

## 9. Testing FP Code with Kimi

```swift
import XCTest
import ComposableArchitecture

final class UserProfileTests: XCTestCase {
    func testUserLoading() {
        let store = TestStore(
            initialState: UserProfile(),
            reducer: userProfileReducer,
            environment: UserProfileEnvironment(
                fetchUser: { _ in Effect(value: mockUser) },
                saveUser: { _ in Effect(value: ()) },
                mainQueue: .immediate
            )
        )
        
        // Kimi can validate: ✓ Action flow
        //                     ✓ State mutations
        //                     ✓ Effect handling
        
        store.send(.onAppear) {
            $0.isLoading = true
            $0.errorMessage = nil
        }
        
        store.receive(.userResponse(.success(mockUser))) {
            $0.user = mockUser
            $0.isLoading = false
        }
    }
    
    func testUserLoadingFailure() {
        let store = TestStore(
            initialState: UserProfile(),
            reducer: userProfileReducer,
            environment: UserProfileEnvironment(
                fetchUser: { _ in Effect(error: .networkFailure) },
                saveUser: { _ in Effect(value: ()) },
                mainQueue: .immediate
            )
        )
        
        // Kimi can spawn a subagent to verify error handling:
        // - Error state updates
        // - Loading state reset
        // - Error message display
        
        store.send(.onAppear) {
            $0.isLoading = true
        }
        
        store.receive(.userResponse(.failure(.networkFailure))) {
            $0.isLoading = false
            $0.errorMessage = "Network connection failed"
        }
    }
}
```

---

## 10. Kimi CLI Integration

### Project Setup

1. **Environment Variable**:
   ```bash
   export KIMI_RULES_PATH="$HOME/projects/rules"
   export KIMI_SWIFT_RULES="$KIMI_RULES_PATH/kimi/swift-fp-style-guide.md"
   ```

2. **Project-Specific Rules** (`.kimirules`):
   ```markdown
   # .kimirules for Swift iOS Project
   
   ## Swift FP Rules
   - Use `let` over `var` (immutability by default)
   - Use `Result` type for error handling (no throwing)
   - Prefer structs over classes (value types)
   - Use Swift Composable Architecture (TCA) for state management
   - No force unwrapping (!) or force casting (as!)
   - Use Tagged for type-safe identifiers
   
   ## Kimi-Specific
   - Validate TCA reducer purity
   - Check Result type usage throughout
   - Verify SwiftUI view composition
   - Spawn subagents for complex TCA validation
   - Parallel verification of multiple views
   ```

3. **Xcode Integration**:
   - Add Build Phase script to verify rules
   - Run SwiftFormat/SwiftLint via Kimi
   - Validate TCA pattern adherence

4. **Documentation**: Follow Kimi's 3-tier structure:
   - Tier 1: ARCHITECTURE_PLAN.md (strategic)
   - Tier 2: docs/plans/ (tactical)
   - Tier 3: docs/YYYY_MM_DD/ (execution)

---

## Summary

Swift's strong type system and value semantics make it excellent for functional programming. Use immutable value types, explicit Result types for error handling, and Swift Composable Architecture for state management. Kimi excels at validating TCA reducers and SwiftUI view composition in parallel.

**Key Takeaways**:
- `let` over `var` for immutability
- `Result` type instead of throwing
- Structs over classes (value semantics)
- Swift Composable Architecture for state
- Tagged types for type safety
- Kimi's parallel validation for TCA

**Next**: See [KIMI_FP_PRINCIPLES.md](KIMI_FP_PRINCIPLES.md) for deeper FP concepts.

---

**Last Updated**: 2025-11-14  
**Maintained By**: Kimi CLI Global Rules System  
**Status**: Active
