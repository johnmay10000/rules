# Kotlin Functional Programming Style Guide
## For Android, Backend (Ktor), and Multiplatform Projects

### Core Principles

1. **Immutability by Default**: Use `val` over `var`, data classes, immutable collections
2. **Pure Functions**: Functions should have no side effects unless explicitly marked
3. **Type Safety**: Leverage Kotlin's null safety and type system
4. **Composability**: Build complex operations from small, composable functions
5. **Explicit Error Handling**: Use `Either`/`Result` instead of exceptions

---

## Required Libraries

```kotlin
// Core Kotlin provides good FP primitives
// Recommended: Arrow for advanced FP patterns

dependencies {
    // Arrow Core - Essential FP types and patterns
    implementation("io.arrow-kt:arrow-core:1.2.0")
    
    // Arrow FX - Functional effects and coroutines
    implementation("io.arrow-kt:arrow-fx-coroutines:1.2.0")
    
    // Arrow Optics - Lenses and prisms
    implementation("io.arrow-kt:arrow-optics:1.2.0")
    
    // Optional: Kotlinx Coroutines (often already included)
    implementation("org.jetbrains.kotlinx:kotlinx-coroutines-core:1.7.3")
}
```

**Note:** Arrow is the definitive FP library for Kotlin (like fp-ts for TypeScript).

---

## 1. Error Handling with Either

### ❌ Avoid: Throwing Exceptions
```kotlin
fun divide(a: Double, b: Double): Double {
    if (b == 0.0) {
        throw IllegalArgumentException("Cannot divide by zero")
    }
    return a / b
}
```

### ✅ Prefer: Either Type (Arrow)
```kotlin
import arrow.core.Either
import arrow.core.left
import arrow.core.right

sealed class DivisionError {
    object DivideByZero : DivisionError()
    data class InvalidInput(val message: String) : DivisionError()
}

fun divide(a: Double, b: Double): Either<DivisionError, Double> =
    if (b == 0.0) {
        DivisionError.DivideByZero.left()
    } else {
        (a / b).right()
    }

// Or use Kotlin's built-in Result
fun divideResult(a: Double, b: Double): Result<Double> =
    if (b == 0.0) {
        Result.failure(IllegalArgumentException("Cannot divide by zero"))
    } else {
        Result.success(a / b)
    }
```

---

## 2. Monadic Composition (Do Notation Style)

### ✅ Chain Operations with flatMap
```kotlin
import arrow.core.Either
import arrow.core.flatMap
import arrow.core.left
import arrow.core.right

sealed class ValidationError {
    object NotPositive : ValidationError()
    object DivisionByZero : ValidationError()
}

fun validatePositive(x: Double): Either<ValidationError, Double> =
    if (x > 0) x.right() else ValidationError.NotPositive.left()

fun safeSqrt(x: Double): Either<ValidationError, Double> =
    kotlin.math.sqrt(x).right()

fun safeReciprocal(x: Double): Either<ValidationError, Double> =
    if (x == 0.0) {
        ValidationError.DivisionByZero.left()
    } else {
        (1 / x).right()
    }

// Monadic composition - like Haskell's do notation
fun processValue(x: Double): Either<ValidationError, Double> =
    validatePositive(x)
        .flatMap { safeSqrt(it) }
        .flatMap { safeReciprocal(it) }
        .map { it * 100 }

// Usage
val result = processValue(16.0)
// Right(25.0)
```

### Arrow's Either.catch for Exception Handling
```kotlin
import arrow.core.Either

fun parseIntSafe(s: String): Either<Throwable, Int> =
    Either.catch { s.toInt() }

fun readFileSafe(path: String): Either<Throwable, String> =
    Either.catch { java.io.File(path).readText() }

// Chain them
fun processFile(path: String): Either<Throwable, Int> =
    readFileSafe(path)
        .flatMap { content -> parseIntSafe(content) }
        .map { it * 2 }
```

### Kotlin's Result Type (Standard Library)
```kotlin
// Kotlin has built-in Result type
fun processValueResult(x: Double): Result<Double> =
    runCatching { require(x > 0); x }
        .map { kotlin.math.sqrt(it) }
        .map { 1 / it }
        .map { it * 100 }

// Usage
processValueResult(16.0).fold(
    onSuccess = { println("Success: $it") },
    onFailure = { println("Error: ${it.message}") }
)
```

---

## 3. Immutable Data Structures

### ✅ Use Data Classes and Val
```kotlin
data class User(
    val id: String,
    val name: String,
    val email: String,
    val age: Int
) {
    // Return new instance for updates
    fun withName(newName: String): User = copy(name = newName)
    fun withEmail(newEmail: String): User = copy(email = newEmail)
}

// Usage
val user = User("123", "Alice", "alice@example.com", 30)
val updated = user.copy(name = "Alice Smith")
```

### Immutable Collections
```kotlin
// Use immutable collections
val numbers: List<Int> = listOf(1, 2, 3, 4, 5)  // Immutable
val mutableNumbers: MutableList<Int> = mutableListOf(1, 2, 3)  // Avoid

// Transformation returns new list
val doubled = numbers.map { it * 2 }
val filtered = numbers.filter { it > 2 }
```

---

## 4. Function Composition

### ✅ Build Complex Pipelines
```kotlin
// Composition functions
infix fun <A, B, C> ((A) -> B).andThen(g: (B) -> C): (A) -> C =
    { a -> g(this(a)) }

infix fun <A, B, C> ((B) -> C).compose(f: (A) -> B): (A) -> C =
    { a -> this(f(a)) }

// Pipe operator
infix fun <A, B> A.pipe(f: (A) -> B): B = f(this)

// Example usage
val normalize: (Double) -> Double = { it / 255.0 }
val sigmoid: (Double) -> Double = { 1 / (1 + kotlin.math.exp(-it)) }
val scale: (Double) -> Double = { it * 100 }

val pipeline = normalize andThen sigmoid andThen scale

val result = pipeline(200.0)

// Or with pipe
val result2 = 200.0 pipe normalize pipe sigmoid pipe scale

// Arrow provides these operators
import arrow.core.compose

val pipeline2 = normalize compose sigmoid compose scale
```

---

## 5. Currying and Partial Application

### ✅ Curry Functions for Composition
```kotlin
// Manual currying
fun <A, B, C> curry(f: (A, B) -> C): (A) -> (B) -> C =
    { a -> { b -> f(a, b) } }

fun <A, B, C, D> curry(f: (A, B, C) -> D): (A) -> (B) -> (C) -> D =
    { a -> { b -> { c -> f(a, b, c) } } }

// Example: String formatting
fun formatString(template: String, value: String): String =
    template.replace("{}", value)

val curriedFormat = curry(::formatString)
val greetingFormatter = curriedFormat("Hello, {}!")
val farewellFormatter = curriedFormat("Goodbye, {}!")

println(greetingFormatter("Alice"))  // "Hello, Alice!"
println(farewellFormatter("Bob"))    // "Goodbye, Bob!"

// Map with curried functions
val names = listOf("Alice", "Bob", "Charlie")
val greetings = names.map(greetingFormatter)

// Arrow provides currying utilities
import arrow.core.curried

val curriedAdd = { a: Int, b: Int -> a + b }.curried()
val add5 = curriedAdd(5)
println(add5(10))  // 15
```

---

## 6. Railway-Oriented Programming

### ✅ Chain Operations with Either
```kotlin
import arrow.core.Either
import arrow.core.flatMap
import arrow.core.left
import arrow.core.right

sealed class NetworkError {
    object InvalidURL : NetworkError()
    data class RequestFailed(val message: String) : NetworkError()
    data class DecodingError(val message: String) : NetworkError()
    object NotFound : NetworkError()
}

data class User(
    val id: String,
    val name: String,
    val email: String
)

// Individual steps
fun validateURL(string: String): Either<NetworkError, String> =
    if (string.startsWith("http")) {
        string.right()
    } else {
        NetworkError.InvalidURL.left()
    }

suspend fun fetchData(url: String): Either<NetworkError, String> =
    Either.catch {
        // Simulated network call
        """{"id":"123","name":"Alice","email":"alice@example.com"}"""
    }.mapLeft { NetworkError.RequestFailed(it.message ?: "Unknown error") }

fun decodeUser(json: String): Either<NetworkError, User> =
    Either.catch {
        // Simulated JSON decoding
        User("123", "Alice", "alice@example.com")
    }.mapLeft { NetworkError.DecodingError(it.message ?: "Decoding failed") }

// Composed pipeline
suspend fun fetchUser(urlString: String): Either<NetworkError, User> =
    validateURL(urlString)
        .flatMap { fetchData(it) }
        .flatMap { decodeUser(it) }

// Pattern matching on result
when (val result = fetchUser("https://api.example.com/user/123")) {
    is Either.Right -> println("Fetched user: ${result.value.name}")
    is Either.Left -> println("Error: ${result.value}")
}
```

---

## 7. Suspending Functions with Either

### ✅ Combine Either with Coroutines
```kotlin
import arrow.core.Either
import arrow.core.raise.either
import arrow.core.raise.ensure
import kotlinx.coroutines.delay

// Arrow's raise DSL for monadic comprehension
suspend fun fetchUserProfile(id: String): Either<NetworkError, User> = either {
    // Validate ID
    ensure(id.isNotBlank()) { NetworkError.InvalidURL }
    
    // Fetch user
    delay(100) // Simulated network delay
    val userData = fetchUserData(id).bind()
    
    // Fetch additional data
    val posts = fetchUserPosts(id).bind()
    
    // Combine results
    User(userData.id, userData.name, userData.email)
}

// Or use traditional flatMap
suspend fun fetchUserProfileTraditional(id: String): Either<NetworkError, User> =
    validateUserId(id)
        .flatMap { fetchUserData(it) }
        .flatMap { user ->
            fetchUserPosts(user.id).map { posts ->
                user // Return user with posts loaded
            }
        }
```

---

## 8. Pattern Matching for ADTs

### ✅ Use Sealed Classes and When
```kotlin
// ADT for remote data
sealed class RemoteData<out E, out A> {
    object NotAsked : RemoteData<Nothing, Nothing>()
    object Loading : RemoteData<Nothing, Nothing>()
    data class Failure<E>(val error: E) : RemoteData<E, Nothing>()
    data class Success<A>(val data: A) : RemoteData<Nothing, A>()
    
    // Functor
    fun <B> map(f: (A) -> B): RemoteData<E, B> = when (this) {
        is NotAsked -> NotAsked
        is Loading -> Loading
        is Failure -> Failure(error)
        is Success -> Success(f(data))
    }
    
    // Monad
    fun <B> flatMap(f: (A) -> RemoteData<E, B>): RemoteData<E, B> = when (this) {
        is NotAsked -> NotAsked
        is Loading -> Loading
        is Failure -> Failure(error)
        is Success -> f(data)
    }
}

// Usage with Jetpack Compose
@Composable
fun UserListScreen(viewModel: UserListViewModel) {
    when (val state = viewModel.users) {
        is RemoteData.NotAsked -> {
            Button(onClick = { viewModel.loadUsers() }) {
                Text("Load Users")
            }
        }
        is RemoteData.Loading -> {
            CircularProgressIndicator()
        }
        is RemoteData.Failure -> {
            Column {
                Text("Error: ${state.error}")
                Button(onClick = { viewModel.loadUsers() }) {
                    Text("Retry")
                }
            }
        }
        is RemoteData.Success -> {
            LazyColumn {
                items(state.data) { user ->
                    UserRow(user)
                }
            }
        }
    }
}
```

---

## 9. Higher-Order Functions and Abstractions

### ✅ Generic Functional Patterns
```kotlin
import arrow.core.Either
import arrow.core.left
import arrow.core.right

// Traverse for Either
fun <A, B, E> List<A>.traverse(
    f: (A) -> Either<E, B>
): Either<E, List<B>> {
    val results = mutableListOf<B>()
    for (element in this) {
        when (val result = f(element)) {
            is Either.Right -> results.add(result.value)
            is Either.Left -> return result.value.left()
        }
    }
    return results.right()
}

// Sequence for Either
fun <A, E> List<Either<E, A>>.sequence(): Either<E, List<A>> =
    traverse { it }

// Usage
fun validatePositiveInt(x: Int): Either<ValidationError, Int> =
    if (x > 0) x.right() else ValidationError.NotPositive.left()

val numbers = listOf(1, 2, 3, 4, 5)
val validated = numbers.traverse(::validatePositiveInt)
// Right([1, 2, 3, 4, 5])

val badNumbers = listOf(1, -2, 3)
val invalidated = badNumbers.traverse(::validatePositiveInt)
// Left(ValidationError.NotPositive)
```

### Arrow's Built-in Traverse
```kotlin
import arrow.core.Either
import arrow.core.traverse

// Arrow provides traverse for common structures
val result = listOf(1, 2, 3).traverse(Either.applicative<String>()) { x ->
    validatePositiveInt(x)
}
```

---

## 10. Optics (Lenses and Prisms) with Arrow

### ✅ Type-Safe Deep Updates
```kotlin
import arrow.optics.optics

@optics
data class Address(
    val street: String,
    val city: String,
    val zipCode: String
) {
    companion object
}

@optics
data class User(
    val id: String,
    val name: String,
    val address: Address
) {
    companion object
}

// Generated lenses allow immutable updates
val user = User(
    id = "123",
    name = "Alice",
    address = Address("Main St", "Springfield", "12345")
)

// Update nested field immutably
val updatedUser = User.address.city.modify(user) { "New City" }

// Or using DSL
val updatedUser2 = user.copy {
    User.address.city transform { "New City" }
}

// Prism for Optional fields
@optics
sealed class Response {
    data class Success(val data: String) : Response()
    data class Error(val message: String) : Response()
    
    companion object
}

// Access data only if Success
val successPrism = Response.success
val data = successPrism.getOrNull(Response.Success("Hello"))
// "Hello"
```

---

## Complete Example: Android App with FP

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

// MARK: - Repository (Optional abstraction layer)

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

// MARK: - Compose UI

@Composable
fun UserListScreen(
    viewModel: UserListViewModel = viewModel()
) {
    val users by viewModel.users.collectAsState()
    val searchQuery by viewModel.searchQuery.collectAsState()
    val filteredUsers by viewModel.filteredUsers.collectAsState()
    
    Scaffold(
        topBar = {
            TopAppBar(
                title = { Text("Users") },
                actions = {
                    if (users !is RemoteData.Loading) {
                        IconButton(onClick = { viewModel.loadUsers() }) {
                            Icon(Icons.Default.Refresh, "Refresh")
                        }
                    }
                }
            )
        }
    ) { padding ->
        Column(
            modifier = Modifier
                .fillMaxSize()
                .padding(padding)
        ) {
            // Search bar
            if (users is RemoteData.Success) {
                OutlinedTextField(
                    value = searchQuery,
                    onValueChange = { viewModel.updateSearchQuery(it) },
                    modifier = Modifier
                        .fillMaxWidth()
                        .padding(16.dp),
                    placeholder = { Text("Search users...") },
                    leadingIcon = { Icon(Icons.Default.Search, null) }
                )
            }
            
            // Content based on state
            when (users) {
                is RemoteData.NotAsked -> {
                    EmptyState(
                        message = "No users loaded",
                        actionLabel = "Load Users",
                        onAction = { viewModel.loadUsers() }
                    )
                }
                
                is RemoteData.Loading -> {
                    Box(
                        modifier = Modifier.fillMaxSize(),
                        contentAlignment = Alignment.Center
                    ) {
                        CircularProgressIndicator()
                    }
                }
                
                is RemoteData.Failure -> {
                    ErrorState(
                        error = (users as RemoteData.Failure).error,
                        onRetry = { viewModel.loadUsers() }
                    )
                }
                
                is RemoteData.Success -> {
                    LazyColumn(
                        modifier = Modifier.fillMaxSize(),
                        contentPadding = PaddingValues(16.dp),
                        verticalArrangement = Arrangement.spacedBy(8.dp)
                    ) {
                        items(
                            items = filteredUsers,
                            key = { it.id }
                        ) { user ->
                            UserCard(user = user)
                        }
                    }
                }
            }
        }
    }
    
    // Load users on first composition
    LaunchedEffect(Unit) {
        if (users is RemoteData.NotAsked) {
            viewModel.loadUsers()
        }
    }
}

@Composable
private fun UserCard(user: User) {
    Card(
        modifier = Modifier.fillMaxWidth(),
        elevation = CardDefaults.cardElevation(defaultElevation = 2.dp)
    ) {
        Column(
            modifier = Modifier.padding(16.dp)
        ) {
            Text(
                text = user.name,
                style = MaterialTheme.typography.titleMedium
            )
            Text(
                text = user.email,
                style = MaterialTheme.typography.bodyMedium,
                color = MaterialTheme.colorScheme.onSurfaceVariant
            )
            Text(
                text = "Age: ${user.age}",
                style = MaterialTheme.typography.bodySmall,
                color = MaterialTheme.colorScheme.onSurfaceVariant
            )
        }
    }
}

@Composable
private fun EmptyState(
    message: String,
    actionLabel: String,
    onAction: () -> Unit
) {
    Column(
        modifier = Modifier.fillMaxSize(),
        horizontalAlignment = Alignment.CenterHorizontally,
        verticalArrangement = Arrangement.Center
    ) {
        Text(
            text = message,
            style = MaterialTheme.typography.bodyLarge
        )
        Spacer(modifier = Modifier.height(16.dp))
        Button(onClick = onAction) {
            Text(actionLabel)
        }
    }
}

@Composable
private fun ErrorState(
    error: NetworkError,
    onRetry: () -> Unit
) {
    Column(
        modifier = Modifier.fillMaxSize(),
        horizontalAlignment = Alignment.CenterHorizontally,
        verticalArrangement = Arrangement.Center
    ) {
        Icon(
            imageVector = Icons.Default.Error,
            contentDescription = null,
            modifier = Modifier.size(48.dp),
            tint = MaterialTheme.colorScheme.error
        )
        Spacer(modifier = Modifier.height(16.dp))
        Text(
            text = when (error) {
                is NetworkError.InvalidURL -> "Invalid URL"
                is NetworkError.RequestFailed -> error.message
                is NetworkError.DecodingError -> error.message
                is NetworkError.NotFound -> "Resource not found"
            },
            style = MaterialTheme.typography.bodyLarge
        )
        Spacer(modifier = Modifier.height(16.dp))
        Button(onClick = onRetry) {
            Text("Retry")
        }
    }
}
```

---

## Style Rules Summary

1. **No exceptions**: Use `Either` or `Result` instead of throwing
2. **Use `val` over `var`**: Immutability by default
3. **Data classes**: For immutable domain models
4. **Sealed classes**: For ADTs and state modeling
5. **Compose functions**: Use `andThen`, `compose`, `pipe`
6. **Leverage Arrow**: For Either, Option, and advanced patterns
7. **Type everything**: Use Kotlin's type inference wisely
8. **Pattern match**: Use `when` exhaustively on sealed classes
9. **Small functions**: Each function does one thing
10. **Railway-oriented**: Chain Either, handle at boundaries

---

## File Organization

```
app/
├── domain/
│   ├── model/
│   │   ├── User.kt
│   │   └── Post.kt
│   └── error/
│       └── NetworkError.kt
├── data/
│   ├── api/
│   │   ├── UserApi.kt
│   │   └── PostApi.kt
│   └── repository/
│       └── UserRepository.kt
├── presentation/
│   ├── viewmodel/
│   │   └── UserListViewModel.kt
│   └── ui/
│       ├── screen/
│       │   └── UserListScreen.kt
│       └── component/
│           └── UserCard.kt
└── core/
    ├── fp/
    │   ├── RemoteData.kt
    │   └── Extensions.kt
    └── util/
        └── Operators.kt
```

---

## Tooling

```kotlin
// build.gradle.kts
plugins {
    kotlin("jvm") version "1.9.20"
    id("com.google.devtools.ksp") version "1.9.20-1.0.14"
}

dependencies {
    // Arrow
    implementation("io.arrow-kt:arrow-core:1.2.0")
    implementation("io.arrow-kt:arrow-fx-coroutines:1.2.0")
    ksp("io.arrow-kt:arrow-optics-ksp-plugin:1.2.0")
    
    // Coroutines
    implementation("org.jetbrains.kotlinx:kotlinx-coroutines-core:1.7.3")
    
    // Testing
    testImplementation("io.arrow-kt:arrow-fx-coroutines-test:1.2.0")
}
```

This guide transforms Kotlin into a strongly typed, pure functional language similar to Haskell while leveraging Kotlin's modern features like coroutines, null safety, and Jetpack Compose.
