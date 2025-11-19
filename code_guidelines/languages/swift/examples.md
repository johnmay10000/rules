---
title: Swift Functional Programming Examples
language: swift
category: code_guidelines
type: examples
applies_to: [cursor, kimi, claude, gemini]
version: 1.0.0
last_updated: 2025-11-19
---

# Swift Functional Programming Examples

Practical examples of functional programming in Swift, focusing on SwiftUI and Architecture.

## SwiftUI App with FP Architecture

A complete example demonstrating `RemoteData` ADT, `Result` for errors, and ViewModel integration.

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
        case .invalidURL: return "Invalid URL"
        case .requestFailed(let m): return "Request failed: \(m)"
        case .decodingError(let m): return "Decoding error: \(m)"
        case .notFound: return "Resource not found"
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
    
    var isLoading: Bool {
        if case .loading = self { return true }
        return false
    }
}

// MARK: - API Layer

struct UserAPI {
    // Pure function - returns Result
    static func fetchUsers() async -> Result<[User], NetworkError> {
        await Result {
            guard let url = URL(string: "https://api.example.com/users") else {
                throw NetworkError.invalidURL
            }
            
            let (data, response) = try await URLSession.shared.data(from: url)
            
            guard let httpResponse = response as? HTTPURLResponse,
                  httpResponse.statusCode == 200 else {
                throw NetworkError.notFound
            }
            
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
}

// MARK: - Helper

extension Result {
    func fold<T>(onSuccess: (Success) -> T, onFailure: (Failure) -> T) -> T {
        switch self {
        case .success(let v): return onSuccess(v)
        case .failure(let e): return onFailure(e)
        }
    }
}

// MARK: - Views

struct UserListView: View {
    @StateObject private var viewModel = UserListViewModel()
    
    var body: some View {
        NavigationView {
            Group {
                switch viewModel.users {
                case .notAsked:
                    Button("Load Users") { viewModel.loadUsers() }
                case .loading:
                    ProgressView()
                case .failure(let error):
                    Text("Error: \(error.localizedDescription)")
                case .success:
                    List(viewModel.filteredUsers) { user in
                        VStack(alignment: .leading) {
                            Text(user.name).font(.headline)
                            Text(user.email).font(.caption)
                        }
                    }
                }
            }
            .navigationTitle("Users")
            .searchable(text: $viewModel.searchText)
        }
    }
}
```

## Parallel Validation (Kimi Pattern)

Validating multiple properties in parallel using a functional approach.

```swift
struct UserProfile {
    let username: String
    let email: String
    let age: Int
}

enum ValidationError: Error {
    case invalidUsername
    case invalidEmail
    case invalidAge
}

func validateUsername(_ s: String) -> Result<String, ValidationError> {
    s.count >= 3 ? .success(s) : .failure(.invalidUsername)
}

func validateEmail(_ s: String) -> Result<String, ValidationError> {
    s.contains("@") ? .success(s) : .failure(.invalidEmail)
}

func validateAge(_ i: Int) -> Result<Int, ValidationError> {
    i >= 18 ? .success(i) : .failure(.invalidAge)
}

// Applicative style validation (collects all errors or returns success)
// Note: Requires a custom 'Validated' type or similar abstraction in standard Swift
// This example conceptually shows parallel validation intent.

func validateProfile(username: String, email: String, age: Int) -> Result<UserProfile, [ValidationError]> {
    let u = validateUsername(username)
    let e = validateEmail(email)
    let a = validateAge(age)
    
    switch (u, e, a) {
    case (.success(let u), .success(let e), .success(let a)):
        return .success(UserProfile(username: u, email: e, age: a))
    default:
        var errors: [ValidationError] = []
        if case .failure(let err) = u { errors.append(err) }
        if case .failure(let err) = e { errors.append(err) }
        if case .failure(let err) = a { errors.append(err) }
        return .failure(errors)
    }
}
```
