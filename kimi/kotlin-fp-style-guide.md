# Kotlin Functional Programming Style Guide for Kimi CLI

**Version**: 2.0.0  
**Last Updated**: 2025-11-14  
**Part of**: [KIMI.md](KIMI.md) Global Rule Set  
**Target**: Kotlin projects (Android, Ktor, Multiplatform)

> **📖 Global Rules**: This document extends [KIMI.md](KIMI.md) with Kotlin-specific guidance. For mandatory universal rules (Git, documentation, testing, file size), see [KIMI.md](KIMI.md).

---

## Quick Links

- **Mandatory Rules**: See [KIMI.md](KIMI.md) sections 1-4  
- **FP Principles Deep Dive**: See [KIMI_FP_PRINCIPLES.md](KIMI_FP_PRINCIPLES.md)  
- **Workflow Guide**: See [KIMI_WORKFLOW_GUIDE.md](KIMI_WORKFLOW_GUIDE.md)  
- **Integration**: See [KIMI.md Integration](#kimi-integration) below  

---

## For Android, Ktor (Backend), and Multiplatform Projects

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

**Note**: Arrow is the definitive FP library for Kotlin (like fp-ts for TypeScript).

---

## Kimi-Specific Patterns

### Parallel Validation of Kotlin Code

Kimi can validate Kotlin's null safety and FP patterns simultaneously using parallel tool calls:

```kotlin
// Kimi excels at validating Kotlin's type system and Arrow patterns
data class User(
    val id: UserId,
    val name: String,
    val email: Email,
    val metadata: Map<String, String>
) {
    // Kimi can validate: ✓ Immutability (val vs var)
    //                  ✓ Null safety
    //                  ✓ Data class correctness
    // All validations in parallel
    
    fun updateEmail(newEmail: Email): User =
        copy(email = newEmail)
        // Kimi validates: ✓ Returns new instance
        //                 ✓ No mutation
}
```

### Subagent Pattern for Coroutine Validation

Use Kimi's Task tool to spawn subagents for validating complex coroutine patterns:

```kotlin
// Complex coroutine pipeline that benefits from Kimi's subagent validation
class UserRepository {
    suspend fun getUserWithPosts(id: UserId): Either<RepositoryError, UserWithPosts> =
        either {
            // Kimi can spawn subagents to verify:
            // - Coroutine context correctness
            // - Arrow Either bind operations
            // - Error handling coverage
            val user = getUser(id).bind()  // ✓ Validated
            val posts = getPosts(user.id).bind()  // ✓ Validated (can be parallel)
            UserWithPosts(user, posts)
        }
    
    private suspend fun getUser(id: UserId): Either<RepositoryError, User> = 
        // Implementation
    
    private suspend fun getPosts(userId: UserId): Either<RepositoryError, List<Post>> =
        // Implementation
}
```

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

// Or prefer Kotlin's built-in Result
data class DivisionError(val message: String)

fun divideResult(a: Double, b: Double): Result<Double> =
    if (b == 0.0) {
        Result.failure(DivisionError("Cannot divide by zero"))
    } else {
        Result.success(a / b)
    }
```

### ✅ Chain Results (Railway Pattern)
```kotlin
// Kimi validates railway composition with Arrow
fun processUserData(raw: String): Either<ProcessingError, User> =
    either {
        val json = parseJson(raw).bind()
        val validated = validateSchema(json).bind()
        val enriched = enrichWithMetadata(validated).bind()
        transformToUser(enriched)
    }
    // Kimi can validate each step in parallel:
    // - parseJson type safety ✓
    // - validateSchema coverage ✓
    // - enrichWithMetadata composition ✓
    // - transformToUser correctness ✓

// Helper functions with explicit types
fun parseJson(raw: String): Either<ParseError, Json> = TODO()
fun validateSchema(json: Json): Either<ValidationError, ValidatedData> = TODO()
fun enrichWithMetadata(data: ValidatedData): Either<EnrichmentError, EnrichedData> = TODO()
fun transformToUser(enriched: EnrichedData): User = TODO()
```

---

## 2. Arrow Either with Coroutines

```kotlin
import arrow.core.Either
import arrow.core.continuations.either

sealed class RepositoryError {
    data class NotFound(val id: String) : RepositoryError()
    data class DatabaseError(val message: String) : RepositoryError()
    object NetworkError : RepositoryError()
}

data class User(val id: UserId, val name: String, val email: String)
data class Post(val id: PostId, val userId: UserId, val content: String)
data class UserWithPosts(val user: User, val posts: List<Post>)

class UserRepository {
    // Kimi validates coroutine + Either integration
    suspend fun getUserWithPosts(id: UserId): Either<RepositoryError, UserWithPosts> =
        either {
            // Each bind operation validated by Kimi
            val user = getUser(id).bind()  // ✓ Validated
            val posts = async { getPosts(user.id).bind() }  // ✓ Parallel validation possible
            UserWithPosts(user, posts.await())
        }
    
    private suspend fun getUser(id: UserId): Either<RepositoryError, User> =
        try {
            val user = apiClient.getUser(id.value)
            if (user == null) RepositoryError.NotFound(id.value).left()
            else user.right()
        } catch (e: Exception) {
            RepositoryError.NetworkError.left()
        }
    
    private suspend fun getPosts(userId: UserId): Either<RepositoryError, List<Post>> =
        try {
            apiClient.getPosts(userId.value).right()
        } catch (e: Exception) {
            RepositoryError.NetworkError.left()
        }
}

// Extension for Result to Either conversion
fun <T> Result<T>.toEither(): Either<Throwable, T> =
    when (val result = this) {
        is Result.Success -> result.value.right()
        is Result.Failure -> result.exception.left()
    }
```

---

## 3. Immutable Data Classes

```kotlin
// Immutable by default (val properties)
data class User(
    val id: UserId,
    val name: String,
    val email: Email,
    val preferences: UserPreferences = UserPreferences.default()
)

data class UserPreferences(
    val notificationsEnabled: Boolean,
    val theme: Theme,
    val language: Language
) {
    companion object {
        fun default() = UserPreferences(
            notificationsEnabled = true,
            theme = Theme.SYSTEM,
            language = Language.ENGLISH
        )
    }
}

enum class Theme { LIGHT, DARK, SYSTEM }
enum class Language { ENGLISH, SPANISH, FRENCH }

// Updating immutable state (returns new instance)
fun User.updateEmail(newEmail: Email): User =
    copy(email = newEmail)
    // Kimi validates: ✓ Returns new instance
    //                 ✓ No mutation of original
    //                 ✓ Only email changed

fun User.updatePreferences(
    updates: (UserPreferences) -> UserPreferences
): User = copy(preferences = updates(preferences))

// Usage
val user = User(
    id = UserId(UUID.randomUUID().toString()),
    name = "Alice",
    email = Email("alice@example.com")
)
val updatedUser = user.updateEmail(Email("new@example.com"))
// Original user unchanged ✓
```

---

## 4. Functional Collections with Arrow

```kotlin
import arrow.core.Option
import arrow.core.none
import arrow.core.some

// Kimi can validate functional collection operations in parallel
data class Product(val name: String, val price: Double, val inStock: Boolean)

fun processProducts(products: List<Product>): List<Product> =
    products
        .filter { it.inStock }  // ✓ Validated
        .map { it.copy(price = it.price * 1.1) }  // ✓ Validated (parallel)
        .sortedByDescending { it.price }  // ✓ Validated
        .take(10)  // ✓ Validated
        // Kimi's parallel validation ensures each step is type-safe

// FilterMap for combined operations
fun extractEmails(users: List<User?>): List<Email> =
    users.filterMap { user ->
        user?.email?.let { it.some() } ?: none()
    }

// Partition (separate into two lists)
fun partitionByStock(products: List<Product>): Pair<List<Product>, List<Product>> =
    products.partition { it.inStock }

// Validation with Either
fun validateProducts(products: List<Product>): Either<ValidationError, List<ValidProduct>> =
    products.traverseEither { product ->
        validateProduct(product)  // Kimi validates each validation in parallel
    }

fun validateProduct(product: Product): Either<ValidationError, ValidProduct> =
    either {
        ensure(product.price > 0) { ValidationError.InvalidPrice }
        ensure(product.name.isNotBlank()) { ValidationError.InvalidName }
        ValidProduct(product.name, product.price)
    }
```

---

## 5. Arrow Optics (Lenses)

```kotlin
import arrow.optics.optics
import arrow.optics.Lens

// Optics for nested immutable updates
@optics
data class Company(val name: String, val address: Address) {
    companion object
}

@optics
data class Address(val street: Street, val city: String, val country: String) {
    companion object
}

@optics
data class Street(val name: String, val number: Int) {
    companion object
}

// Auto-generated lenses
val companyAddress: Lens<Company, Address> = Company.address
val addressStreet: Lens<Address, Street> = Address.street
val streetName: Lens<Street, String> = Street.name

// Composed lens for deep updates
val companyStreetName: Lens<Company, String> = companyAddress
    .compose(addressStreet)
    .compose(streetName)

// Usage
val company = Company(
    name = "Acme Corp",
    address = Address(
        street = Street(name = "Main St", number = 123),
        city = "New York",
        country = "USA"
    )
)

// Immutable deep update
val updatedCompany = companyStreetName.modify(company) { "$it (Updated)" }
// Result: Street becomes "Main St (Updated)"

// Kimi validates: ✓ Lens composition correctness
//                 ✓ Type safety throughout
//                 ✓ Immutability preserved
```

---

## 6. Coroutines + Either

```kotlin
import kotlinx.coroutines.*
import arrow.core.Either
import arrow.core.continuations.either

class OrderRepository {
    // Kimi validates coroutine + Either patterns
    suspend fun processOrder(
        order: Order
    ): Either<OrderProcessingError, OrderConfirmation> = either {
        coroutineScope {
            // Parallel operations with error handling
            val inventoryCheck = async {
                checkInventory(order.items).bind()
            }
            
            val paymentAuth = async {
                authorizePayment(order.payment).bind()
            }
            
            val userVerification = async {
                verifyUser(order.userId).bind()
            }
            
            // Wait for all validations
            inventoryCheck.await()
            val authResult = paymentAuth.await()
            userVerification.await()
            
            // Create order if all succeed
            createOrder(order, authResult).bind()
        }
    }
    
    private suspend fun checkInventory(
        items: List<OrderItem>
    ): Either<InventoryError, Unit> =
        // Check inventory levels
        
    private suspend fun authorizePayment(
        payment: Payment
    ): Either<PaymentError, PaymentAuthorization> =
        // Call payment gateway
        
    private suspend fun verifyUser(
        userId: UserId
    ): Either<VerificationError, VerifiedUser> =
        // Verify user identity
        
    private suspend fun createOrder(
        order: Order,
        auth: PaymentAuthorization
    ): Either<OrderCreationError, OrderConfirmation> =
        // Create order in database
    }
    
    // Kimi can spawn subagents to verify:
    // - Coroutine context correctness
    // - Parallel error handling
    // - Either.bind() usage
    // - Resource management
}
```

---

## 7. Testing FP Code with Kimi

```kotlin
import io.kotest.core.spec.style.StringSpec
import io.kotest.matchers.shouldBe
import io.kotest.assertions.arrow.core.shouldBeRight
import io.kotest.assertions.arrow.core.shouldBeLeft

class UserRepositoryTest : StringSpec({
    "getUserWithPosts should combine user and posts successfully" {
        val mockApi = MockApiClient()
        val expectedUser = User(UserId("123"), "Alice", Email("alice@example.com"))
        val expectedPosts = listOf(Post(PostId("1"), UserId("123"), "Hello"))
        
        mockApi.getUserResult = Either.Right(expectedUser)
        mockApi.getPostsResult = Either.Right(expectedPosts)
        
        val repository = UserRepository(mockApi)
        
        runBlocking {
            val result = repository.getUserWithPosts(UserId("123"))
            
            // Kimi validates: ✓ Right side extraction
            //                 ✓ Type safety
            result.shouldBeRight() shouldBe UserWithPosts(expectedUser, expectedPosts)
        }
    }
    
    "getUserWithPosts should return left when user fetch fails" {
        val mockApi = MockApiClient()
        val expectedError = RepositoryError.NetworkError
        
        mockApi.getUserResult = Either.Left(expectedError)
        
        val repository = UserRepository(mockApi)
        
        runBlocking {
            val result = repository.getUserWithPosts(UserId("123"))
            
            // Kimi can spawn subagent to verify error handling
            result.shouldBeLeft() shouldBe expectedError
        }
    }
    
    "validateProduct should accept valid products" {
        val validProduct = Product("Widget", 29.99, inStock = true)
        
        val result = validateProduct(validProduct)
        
        // Kimi validates: ✓ Validation logic
        //                 ✓ Right side success
        result.shouldBeRight()
    }
    
    "validateProduct should reject invalid price" {
        val invalidProduct = Product("Widget", -5.0, inStock = true)
        
        val result = validateProduct(invalidProduct)
        
        // Kimi validates: ✓ Error conditions
        //                 ✓ Left side error
        result.shouldBeLeft() shouldBe ValidationError.InvalidPrice
    }
})
```

---

## 8. Ktor Backend + Either

```kotlin
import io.ktor.server.application.*
import io.ktor.server.request.*
import io.ktor.server.response.*
import io.ktor.server.routing.*
import arrow.core.Either

fun Application.configureRouting() {
    routing {
        get("/api/users/{id}") {
            val userId = call.parameters["id"] ?: return@get call.respond(
                HttpStatusCode.BadRequest,
                "Missing user ID"
            )
            
            val result = userRepository.getUser(UserId(userId))
            
            result.fold(
                ifLeft = { error ->
                    when (error) {
                        is RepositoryError.NotFound ->
                            call.respond(HttpStatusCode.NotFound, error)
                        is RepositoryError.DatabaseError ->
                            call.respond(HttpStatusCode.InternalServerError, error)
                        RepositoryError.NetworkError ->
                            call.respond(HttpStatusCode.ServiceUnavailable, error)
                    }
                },
                ifRight = { user ->
                    call.respond(HttpStatusCode.OK, user)
                }
            )
            // Kimi validates: ✓ Error mapping
            //                 ✓ HTTP status codes
            //                 ✓ Response serialization
        }
    }
}

// Type-safe route handlers with Either
suspend fun ApplicationCall.handleUserRequest(
    handler: suspend () -> Either<DomainError, ResponseData>
) {
    handler().fold(
        ifLeft = { error ->
            respond(HttpStatusCode.fromError(error), error.message)
        },
        ifRight = { data ->
            respond(HttpStatusCode.OK, data)
        }
    )
}
```

---

## 9. Kimi CLI Integration

### Project Setup

1. **Environment Variable**:
   ```bash
   export KIMI_RULES_PATH="$HOME/projects/rules"
   export KIMI_KOTLIN_RULES="$KIMI_RULES_PATH/kimi/kotlin-fp-style-guide.md"
   ```

2. **Project-Specific Rules** (`.kimirules`):
   ```markdown
   # .kimirules for Kotlin Project
   
   ## Kotlin FP Rules
   - Use `val` over `var` (immutability by default)
   - Use Arrow Either for error handling (no exceptions)
   - Data classes for immutable data structures
   - Prefer Arrow Fx for coroutines
   - Use Arrow Optics for nested updates
   - No nullable types (use Arrow Option instead)
   
   ## Kimi-Specific
   - Validate Arrow Either usage throughout
   - Check coroutine + Either integration
   - Spawn subagents for complex coroutine validation
   - Parallel verification of pure functions
   - Validate optics/lens compositions
   ```

3. **Gradle Integration**:
   ```kotlin
   // build.gradle.kts
   tasks.register("kimiVerify") {
       doLast {
           exec {
               commandLine("kimi", "verify-kotlin-project")
           }
       }
   }
   ```

4. **Documentation**: Follow Kimi's 3-tier structure:
   - Tier 1: ARCHITECTURE_PLAN.md (strategic)
   - Tier 2: docs/plans/ (tactical)
   - Tier 3: docs/YYYY_MM_DD/ (execution)

---

## 10. Multiplatform Example

```kotlin
// commonMain
expect class PlatformLogger {
    fun log(message: String)
}

// Shared FP logic
class AnalyticsTracker(private val logger: PlatformLogger) {
    fun trackEvent(event: AnalyticsEvent): Either<TrackingError, Unit> =
        either {
            validateEvent(event).bind()
            logger.log("Event: ${event.name}")
            sendToAnalytics(event).bind()
        }
    
    private fun validateEvent(event: AnalyticsEvent): Either<ValidationError, Unit> =
        either {
            ensure(event.name.isNotBlank()) { ValidationError.EmptyName }
            ensure(event.parameters.isNotEmpty()) { ValidationError.NoParameters }
        }
    
    private fun sendToAnalytics(event: AnalyticsEvent): Either<NetworkError, Unit> =
        // Platform-specific implementation
}
```

---

## Summary

Kotlin's null safety and Arrow library make it excellent for functional programming. Use immutable data classes, Arrow Either for error handling, and Arrow Fx for functional coroutines. Kimi excels at validating Arrow patterns and coroutine integration in parallel.

**Key Takeaways**:
- Use `val` over `var` for immutability
- Arrow Either for error handling (no exceptions)
- Data classes for immutable structures
- Arrow Fx for functional coroutines
- Arrow Optics for nested updates
- Kimi's parallel validation for Arrow patterns

**Next**: See [KIMI_FP_PRINCIPLES.md](KIMI_FP_PRINCIPLES.md) for deeper FP concepts.

---

**Last Updated**: 2025-11-14  
**Maintained By**: Kimi CLI Global Rules System  
**Status**: Active
