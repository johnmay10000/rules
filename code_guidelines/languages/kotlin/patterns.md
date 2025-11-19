---
title: Kotlin Functional Programming Patterns Reference
language: kotlin
category: code_guidelines
type: patterns
applies_to: [cursor, kimi, claude, gemini]
version: 1.0.0
last_updated: 2025-11-19
---

# Kotlin Functional Programming Patterns Reference

Quick reference for common functional programming patterns in Kotlin.

## Error Handling Patterns

### Either Type (Arrow)

Replace exceptions with `Either<Error, Success>`.

```kotlin
import arrow.core.Either
import arrow.core.left
import arrow.core.right

sealed class DivisionError {
    object DivideByZero : DivisionError()
}

fun divide(a: Double, b: Double): Either<DivisionError, Double> =
    if (b == 0.0) {
        DivisionError.DivideByZero.left()
    } else {
        (a / b).right()
    }
```

### Result Type (Stdlib)

For simple success/failure without typed errors.

```kotlin
fun divideResult(a: Double, b: Double): Result<Double> =
    if (b == 0.0) {
        Result.failure(IllegalArgumentException("Cannot divide by zero"))
    } else {
        Result.success(a / b)
    }
```

## Monadic Composition

### FlatMap Chaining

Chain operations that return `Either`.

```kotlin
fun processValue(x: Double): Either<ValidationError, Double> =
    validatePositive(x)
        .flatMap { safeSqrt(it) }
        .flatMap { safeReciprocal(it) }
        .map { it * 100 }
```

### Arrow's `either` Block (Comprehension)

Imperative style for monadic operations.

```kotlin
import arrow.core.raise.either
import arrow.core.raise.ensure

suspend fun fetchUserProfile(id: String): Either<NetworkError, User> = either {
    ensure(id.isNotBlank()) { NetworkError.InvalidURL }
    
    val userData = fetchUserData(id).bind()
    val posts = fetchUserPosts(id).bind()
    
    User(userData.id, userData.name, userData.email)
}
```

## Immutable Data

### Data Classes

Use `data class` and `val` for immutable records.

```kotlin
data class User(
    val id: String,
    val name: String,
    val email: String
) {
    fun withName(newName: String): User = copy(name = newName)
}
```

### Immutable Collections

Prefer `List`, `Set`, `Map` over `MutableList`, etc.

```kotlin
val numbers: List<Int> = listOf(1, 2, 3)
val doubled = numbers.map { it * 2 } // Returns new list
```

## Function Composition

### Infix Composition

```kotlin
import arrow.core.compose

val pipeline = normalize compose sigmoid compose scale
val result = pipeline(200.0)
```

### Pipe Operator

```kotlin
infix fun <A, B> A.pipe(f: (A) -> B): B = f(this)

val result = 200.0 pipe normalize pipe sigmoid pipe scale
```

## Railway-Oriented Programming

Chain validation and operations.

```kotlin
fun fetchUser(urlString: String): Either<NetworkError, User> =
    validateURL(urlString)
        .flatMap { fetchData(it) }
        .flatMap { decodeUser(it) }
```

## Pattern Matching (ADTs)

Use `sealed class` and `when` expressions.

```kotlin
sealed class RemoteData<out E, out A> {
    object NotAsked : RemoteData<Nothing, Nothing>()
    object Loading : RemoteData<Nothing, Nothing>()
    data class Failure<E>(val error: E) : RemoteData<E, Nothing>()
    data class Success<A>(val data: A) : RemoteData<Nothing, A>()
}

// Exhaustive matching
when (val state = viewModel.users) {
    is RemoteData.NotAsked -> showLoadButton()
    is RemoteData.Loading -> showSpinner()
    is RemoteData.Failure -> showError(state.error)
    is RemoteData.Success -> showList(state.data)
}
```

## Optics (Lenses)

Deep immutable updates with Arrow Optics.

```kotlin
import arrow.optics.optics

@optics
data class User(val address: Address) { companion object }

@optics
data class Address(val city: String) { companion object }

val user = User(Address("New York"))
val updated = User.address.city.modify(user) { "London" }
```
