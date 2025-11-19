---
title: Kotlin Functional Programming Examples
language: kotlin
category: code_guidelines
type: examples
applies_to: [cursor, kimi, claude, gemini]
version: 1.0.0
last_updated: 2025-11-19
---

# Kotlin Functional Programming Examples

Practical examples of functional programming in Kotlin, focusing on Android and backend scenarios.

## Android App with FP Architecture

A complete example demonstrating `RemoteData` ADT, `Either` for errors, and ViewModel integration.

```kotlin
// MARK: - Domain Types

data class User(
    val id: String,
    val name: String,
    val email: String,
    val age: Int
)

sealed class NetworkError {
    object InvalidURL : NetworkError()
    data class RequestFailed(val message: String) : NetworkError()
    data class DecodingError(val message: String) : NetworkError()
    object NotFound : NetworkError()
}

// MARK: - Remote Data ADT

sealed class RemoteData<out E, out A> {
    object NotAsked : RemoteData<Nothing, Nothing>()
    object Loading : RemoteData<Nothing, Nothing>()
    data class Failure<E>(val error: E) : RemoteData<E, Nothing>()
    data class Success<A>(val data: A) : RemoteData<Nothing, A>()
    
    fun <B> map(f: (A) -> B): RemoteData<E, B> = when (this) {
        is NotAsked -> NotAsked
        is Loading -> Loading
        is Failure -> Failure(error)
        is Success -> Success(f(data))
    }
}

// MARK: - API Layer

object UserAPI {
    suspend fun fetchUser(id: String): Either<NetworkError, User> =
        Either.catch {
            // Simulated API call
            delay(500)
            User(id, "Alice", "alice@example.com", 30)
        }.mapLeft { 
            NetworkError.RequestFailed(it.message ?: "Unknown error")
        }
    
    suspend fun fetchUsers(): Either<NetworkError, List<User>> =
        Either.catch {
            delay(500)
            listOf(
                User("1", "Alice", "alice@example.com", 30),
                User("2", "Bob", "bob@example.com", 25)
            )
        }.mapLeft {
            NetworkError.RequestFailed(it.message ?: "Unknown error")
        }
}

// MARK: - Repository

class UserRepository {
    suspend fun getUsers(): Either<NetworkError, List<User>> =
        UserAPI.fetchUsers()
            .map { users -> users.sortedBy { it.name } }
    
    suspend fun getUserById(id: String): Either<NetworkError, User> =
        UserAPI.fetchUser(id)
}

// MARK: - ViewModel

class UserListViewModel : ViewModel() {
    private val repository = UserRepository()
    
    private val _users = MutableStateFlow<RemoteData<NetworkError, List<User>>>(
        RemoteData.NotAsked
    )
    val users: StateFlow<RemoteData<NetworkError, List<User>>> = _users.asStateFlow()
    
    private val _searchQuery = MutableStateFlow("")
    val searchQuery: StateFlow<String> = _searchQuery.asStateFlow()
    
    // Pure computed property
    val filteredUsers: StateFlow<List<User>> = combine(
        users,
        searchQuery
    ) { usersState, query ->
        when (usersState) {
            is RemoteData.Success -> {
                if (query.isBlank()) {
                    usersState.data
                } else {
                    usersState.data.filter { user ->
                        user.name.contains(query, ignoreCase = true) ||
                        user.email.contains(query, ignoreCase = true)
                    }
                }
            }
            else -> emptyList()
        }
    }.stateIn(
        scope = viewModelScope,
        started = SharingStarted.WhileSubscribed(5000),
        initialValue = emptyList()
    )
    
    fun loadUsers() {
        _users.value = RemoteData.Loading
        
        viewModelScope.launch {
            val result = repository.getUsers()
            
            _users.value = when (result) {
                is Either.Right -> RemoteData.Success(result.value)
                is Either.Left -> RemoteData.Failure(result.value)
            }
        }
    }
    
    fun updateSearchQuery(query: String) {
        _searchQuery.value = query
    }
}
```
