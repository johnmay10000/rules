---
title: Testing Philosophy and Requirements
+category: universal_rules
++type: testing
++applies_to: all
++version: 1.0.0
++last_updated: 2025-11-19
++---
++
++# Testing Philosophy and Requirements
++
++**Status**: MANDATORY - All code must be tested comprehensively
++
++This document defines the universal testing philosophy and requirements that apply to ALL projects regardless of technology stack, programming language, or AI assistant used (Cursor, Kimi, Claude, Gemini).
++
++---
++
++## Core Philosophy
++
++### 1. Tests Are Mandatory
++
++**Rule**: No code is complete without tests.
++
++- Every function, method, or module must have corresponding tests
++- Tests are not optional - they are part of the definition of "done"
++- Code without tests is considered incomplete and cannot be committed
++- This applies to production code, utilities, and even documentation examples
++
++**Rationale**:
++- Tests prove that code works as intended
++- Tests serve as executable documentation
++- Tests enable safe refactoring
++- Tests catch regressions early
++- Tests force better design (testable code is better code)
++
++---
++
++### 2. Test First (TDD) When Possible
++
++**Principle**: Write tests before implementation when feasible.
++
++**Red-Green-Refactor Cycle**:
++1. **Red**: Write a failing test that describes desired behavior
++2. **Green**: Write minimal code to make the test pass
++3. **Refactor**: Improve code quality while keeping tests green
++
++**Benefits**:
++- Forces clear requirements before coding
++- Ensures 100% test coverage by design
++- Produces testable code architecture
++- Prevents over-engineering (only write code to pass tests)
++
++**When TDD is Challenging**:
++- Exploratory coding (spike solutions)
++- Complex integrations (test after initial prototype)
++- UI/visual code (test behavior, not appearance)
++
++**Minimum Requirement**: Even if not pure TDD, tests must be written in the same commit as the code they test.
++
++---
++
++### 3. Comprehensive Coverage
++
++**Coverage Requirements**:
++
++- **Minimum 80% code coverage** for all projects
++- **100% coverage** for critical paths (auth, payments, security, core business logic)
++- **100% coverage** for pure functions (they're easiest to test)
++- **All branches tested** - if statements, match/case expressions, error paths
++
++**What to Test**:
++- ✅ Happy paths (normal operation)
++- ✅ Error paths (exceptions, failures, edge cases)
++- ✅ Boundary conditions (empty inputs, maximum values, null/none)
++- ✅ Integration points (APIs, databases, external services)
++- ✅ Concurrency (race conditions, thread safety)
++- ✅ Performance (slow paths, resource usage)
++
++**Coverage is Not Enough**:
++- High coverage doesn't guarantee good tests
++- Tests must be meaningful and assert correct behavior
++- Avoid testing implementation details (test behavior, not internals)
++
++---
++
++### 4. All Tests Must Pass
++
++**Rule**: 100% of tests must pass before any commit.
++
++**Pre-Commit Checklist**:
++- [ ] All unit tests pass
++- [ ] All integration tests pass
++- [ ] All end-to-end tests pass
++- [ ] No linter errors
++- [ ] No type errors (TypeScript, mypy, etc.)
++- [ ] Code coverage meets threshold (80%+)
++
++**Broken Tests**:
++- Never commit with failing tests (even "temporarily")
++- If a test is broken, fix it or remove it (document why)
++- "Skipping" tests should be rare and documented with TODO
++
++**CI/CD Enforcement**:
++- All tests must pass in CI before merge
++- No exceptions for "urgent" fixes
++- Failed build = failed commit
++
++---
++
++## Test Types and Requirements
++
++### Unit Tests
++
++**Purpose**: Test individual functions/methods in isolation.
++
++**Characteristics**:
+- Test one thing (one assertion per test, or one logical concept)
+- Fast execution (< 10ms per test)
+- No external dependencies (databases, APIs, file system)
+- Use mocks/fakes/stubs for dependencies
+- Run in parallel with other unit tests
+
++**Example Structure**:
++```python
++def test_pure_function():
++    """Test pure function with known inputs/outputs"""
++    # Arrange
++    input_data = [1, 2, 3]
++    
++    # Act
++    result = pure_function(input_data)
++    
++    # Assert
++    assert result == [2, 4, 6]
++    assert len(result) == 3
++```
++
++**Coverage Goal**: 100% of pure functions, 90%+ of utility functions
++
++---
++
++### Integration Tests
++
++**Purpose**: Test interactions between components.
++
++**Characteristics**:
+- Test component integration (service + database, API + client)
+- Use real dependencies when feasible (test database, real API)
+- May be slower than unit tests
+- Test error handling and failure modes
+- Test data flow between components
+
++**Example Structure**:
++```python
++def test_user_registration_flow():
++    """Test complete user registration workflow"""
++    # Arrange
++    user_data = {"email": "test@example.com", "password": "Secure123"}
++    
++    # Act
++    result = user_service.register(user_data)
++    
++    # Assert
++    assert result.is_success()
++    assert database.user_exists("test@example.com")
++    assert email_service.welcome_email_sent()
++```
++
++**Coverage Goal**: 80%+ of integration points, all critical workflows
++
++---
++
++### End-to-End (E2E) Tests
++
++**Purpose**: Test complete user workflows from start to finish.
++
++**Characteristics**:
+- Test real user scenarios (login, purchase, data export)
+- Use production-like environment
+- May involve browser automation, API calls, database state
+- Slower execution (seconds to minutes)
+- Fewer in number than unit/integration tests
+- Test critical user journeys only
+
++**Example Structure**:
++```typescript
++test('complete purchase workflow', async () => {
++    // Arrange: Setup test user and product
++    const user = await createTestUser()
++    const product = await createTestProduct()
++    
++    // Act: Execute purchase workflow
++    await login(user.email, user.password)
++    await addToCart(product.id)
++    await checkout()
++    
++    // Assert: Verify purchase completed
++    expect(await getOrderConfirmation()).toBeTruthy()
++    expect(await getUserOrders()).toContain(product.id)
++})
++```
++
++**Coverage Goal**: All critical user journeys (login, checkout, payment, etc.)
++
++**Test Pyramid Ratio**:
++- Unit tests: 70% of test suite
++- Integration tests: 20% of test suite
++- E2E tests: 10% of test suite
++
++---
++
++### Property-Based Tests
++
++**Purpose**: Test properties that should hold for all inputs.
++
++**Characteristics**:
+- Generate random test inputs
+- Verify invariants and algebraic properties
+- Find edge cases human testers miss
+- Excellent for pure functions and algorithms
+
++**Example**:
++```python
++from hypothesis import given, strategies as st
++
++@given(st.integers(), st.integers())
++def test_addition_is_commutative(a, b):
++    """Property: a + b == b + a for all integers"""
++    assert add(a, b) == add(b, a)
++
++@given(st.lists(st.integers()))
++def test_reverse_is_involutive(lst):
++    """Property: reverse(reverse(lst)) == lst"""
++    assert reverse(reverse(lst)) == lst
++```
++
++**When to Use**: Pure functions, algorithms, data structures, mathematical operations
++
++---
++
++## Testing Best Practices
++
++### Test Naming
++
++**Good**:
++- `test_user_registration_succeeds_with_valid_data`
++- `test_divide_by_zero_returns_error`
++- `test_empty_input_returns_empty_result`
++- `test_concurrent_access_is_thread_safe`
++
++**Bad**:
++- `test1`, `test2` (no description)
++- `test_function` (vague)
++- `test_stuff` (meaningless)
++
++**Convention**: `test_<what>_<when>_<expected_result>`
++
++---
++
++### Arrange-Act-Assert (AAA) Pattern
++
++Structure every test with three clear sections:
++
++```python
++def test_user_can_login_with_valid_credentials():
++    # Arrange: Set up test conditions
++    user = create_test_user(email="test@example.com", password="Secure123")
++    login_page = LoginPage()
++    
++    # Act: Perform the action being tested
++    result = login_page.login(user.email, user.password)
++    
++    # Assert: Verify the expected outcome
++    assert result.is_success()
++    assert result.user_id == user.id
++    assert "Welcome" in result.message
++```
++
++**Benefits**:
+- Clear structure, easy to read
+- Consistent across all tests
+- Makes test intent obvious
++- Easy to debug (know which phase failed)
++
++---
++
++### Test Independence
++
++**Rule**: Tests must be independent and isolated.
++
++**Requirements**:
+- Tests can run in any order
+- Tests don't depend on each other
+- Tests clean up after themselves
+- Tests use fresh data/fixtures
+- Parallel test execution must be safe
+
++**Anti-Pattern** (DO NOT DO):
++```python
++# BAD: Tests depend on each other
++def test_create_user():
++    global user_id
++    user_id = create_user()  # Sets global state
++
++def test_update_user():
++    # Depends on user_id from previous test
++    update_user(user_id, {"name": "New Name"})
++```
++
++**Correct Approach**:
++```python
++@pytest.fixture
++def test_user():
++    """Create fresh user for each test"""
++    return create_test_user()
++
++def test_create_user(test_user):
++    assert test_user.id is not None
++
++def test_update_user(test_user):
++    update_user(test_user.id, {"name": "New Name"})
++    assert test_user.name == "New Name"
++```
++
++---
++
++### Test Data Management
++
++**Use Fixtures and Factories**:
++
++```python
++# Factory for creating test data
++def create_test_user(overrides=None):
++    defaults = {
++        "email": f"test-{uuid()}@example.com",
++        "password": "SecurePass123!",
++        "name": "Test User"
++    }
++    if overrides:
++        defaults.update(overrides)
++    return User.create(**defaults)
++
++# Pytest fixture
++@pytest.fixture
++def user():
++    """Fresh test user for each test"""
++    user = create_test_user()
++    yield user
++    user.delete()  # Cleanup
++```
++
++**Avoid Hardcoded Data**:
++- Use random/fake data to prevent conflicts
++- Clean up after tests run
++- Make test data explicit and visible
++
++---
++
++### Test Doubles (Mocks, Stubs, Fakes)
++
++**When to Use**:
+- External services (APIs, databases, message queues)
+- Slow operations (file I/O, network calls)
+- Unreliable dependencies (third-party services)
+- Complex setup requirements
+
++**Types**:
++1. **Fake**: Working implementation (in-memory database)
++2. **Stub**: Returns canned responses
++3. **Mock**: Verifies interactions
++4. **Spy**: Records calls for verification
+
++**Example**:
++```python
++# Fake: In-memory repository for testing
++class InMemoryUserRepository(UserRepository):
++    def __init__(self):
++        self.users = {}
++    
++    def save(self, user):
++        self.users[user.id] = user
++        return user
++    
++    def find(self, user_id):
++        return self.users.get(user_id)
++
++# Mock: Verify interactions
++def test_user_created_event_is_published():
++    mock_event_bus = Mock()
++    user_service = UserService(event_bus=mock_event_bus)
++    
++    user_service.create_user(email="test@example.com")
++    
++    mock_event_bus.publish.assert_called_once_with(
++        UserCreatedEvent(email="test@example.com")
++    )
++```
++
++**Best Practice**: Prefer fakes over mocks when possible (test behavior, not interactions).
++
++---
++
++## Functional Programming Testing
++
++### Testing Pure Functions
++
++Pure functions are the easiest to test:
++
++```python
++# Pure function: same input → same output, no side effects
++def add(a: int, b: int) -> int:
++    return a + b
++
++# Test: Just verify input-output mapping
++def test_add():
++    assert add(2, 3) == 5
++    assert add(-1, 1) == 0
++    assert add(0, 0) == 0
++    
++    # Property: commutative
++    assert add(2, 3) == add(3, 2)
++    
++    # Property: associative
++    assert add(add(1, 2), 3) == add(1, add(2, 3))
++```
++
++**Benefits**:
++- No setup/teardown needed
++- Can test in parallel
++- Property-based testing ideal
++- No mocks required
++
++---
++
++### Testing Functions with Result/Option
++
++```python
++from returns.result import Result, Success, Failure
++
++def divide(a: float, b: float) -> Result[float, str]:
++    if b == 0:
++        return Failure("Division by zero")
++    return Success(a / b)
++
++def test_divide():
++    # Test success path
++    result = divide(10.0, 2.0)
++    assert result == Success(5.0)
++    
++    # Test error path
++    error_result = divide(10.0, 0.0)
++    assert error_result == Failure("Division by zero")
++    
++    # Test property: division is inverse of multiplication (for non-zero)
++    assert divide(10.0, 2.0).map(lambda x: x * 2) == Success(10.0)
++```
++
++---
++
++### Testing Immutable Data Structures
++
++```python
++from dataclasses import dataclass
++
++@dataclass(frozen=True)
++class Config:
++    host: str
++    port: int
++    
++    def with_port(self, new_port: int) -> 'Config':
++        return Config(self.host, new_port)
++
++def test_immutable_update():
++    config = Config("localhost", 8080)
++    new_config = config.with_port(9090)
++    
++    # Original unchanged
++    assert config.port == 8080
++    
++    # New config has updated value
++    assert new_config.port == 9090
++    assert new_config.host == "localhost"
++```
++
++---
++
++## AI Assistant-Specific Testing
++
++### For Kimi CLI
++
++Kimi provides enhanced testing capabilities:
++- **Parallel Test Execution**: Run multiple test suites simultaneously
++- **Subagent Testing**: Spawn subagents for complex integration tests
++- **SetTodoList Integration**: Track test coverage goals
++- **Test Generation**: Generate tests from implementation automatically
++
++**Kimi-Specific Workflow**:
++```bash
++# Run tests in parallel
++kimi test --parallel unit integration e2e
++
++# Generate tests for new module
++kimi generate-tests src/new_module.py --coverage 80
++
++# Check coverage
++kimi coverage --threshold 80
++```
++
++### For Cursor
++
++Cursor provides IDE-integrated testing:
++- **Inline Test Runner**: Run tests from editor
++- **Coverage Visualization**: See uncovered lines inline
++- **Test Generation**: Generate tests from function signatures
++- **Debug Integration**: Debug failing tests easily
++
++**Cursor-Specific Features**:
++- Real-time test status in editor
++- Automatic test discovery
++- Coverage gutter indicators
++- Test result visualization
++
++### For Claude
++
++Claude excels at test design:
++- **Test Strategy**: Design comprehensive test plans
++- **Edge Case Identification**: Find obscure edge cases
++- **Property-Based Testing**: Generate property tests
++- **Test Refactoring**: Improve existing test suites
++
++**Claude-Specific Patterns**:
++- Generate test matrices for complex scenarios
++- Design property-based tests for algorithms
++- Create integration test scenarios
++
++### For Gemini
++
++Gemini provides comprehensive testing guidance:
++- **Best Practices**: Detailed testing methodologies
++- **Framework Guidance**: Optimal test framework selection
++- **Example Generation**: Extensive test examples
++- **Coverage Analysis**: Identify untested code paths
++
++---
++
++## Test Coverage Requirements
++
++### Minimum Thresholds
++
+| Project Type | Line Coverage | Branch Coverage | Critical Path Coverage |
++|--------------|---------------|-----------------|------------------------|
++| Library/Package | 90% | 85% | 100% |
++| Application | 80% | 75% | 100% |
++| Prototype/Spike | 60% | 50% | 80% |
++| Documentation | 100% (examples) | N/A | 100% |
++
++### Critical Paths (Must be 100%)
++
+- Authentication and authorization
+- Payment processing
+- Data validation and sanitization
+- Error handling and recovery
+- Security-sensitive code
+- Core business logic
+- Data persistence
+
++### Measuring Coverage
++
++**Tools by Language**:
++- **Python**: `pytest --cov`, `coverage.py`
++- **TypeScript**: `jest --coverage`, `c8`
++- **Rust**: `cargo tarpaulin`, `cargo llvm-cov`
++- **Kotlin**: `kover`, `jacoco`
++- **Swift**: `xccov`, `slather`
++- **Haskell**: `hpc` (Haskell Program Coverage)
++
++**CI Integration**:
++```yaml
++# Example GitHub Actions
++- name: Run tests with coverage
++  run: pytest --cov=src --cov-report=xml --cov-fail-under=80
++  
++- name: Upload coverage
++  uses: codecov/codecov-action@v3
++  with:
++    file: ./coverage.xml
++    fail_ci_if_error: true
++```
++
++---
++
++## Test Maintenance
++
++### Keep Tests Fast
++
++**Execution Time Targets**:
++- Unit tests: < 10ms per test
++- Integration tests: < 1 second per test
++- E2E tests: < 30 seconds per test
++- Full suite: < 5 minutes total
++
++**Optimization Strategies**:
++- Parallel test execution
++- Test database in memory (SQLite, H2)
++- Mock external services
++- Cache expensive fixtures
++- Split slow test suites
++
++### Keep Tests Reliable
++
++**Flaky Test Prevention**:
++- Avoid sleep()/wait() - use proper synchronization
++- Isolate tests (no shared state)
++- Use fresh test data for each test
++- Mock unreliable external dependencies
++- Set appropriate timeouts
++
++**Flaky Test Protocol**:
++1. If a test fails intermittently, investigate immediately
++2. Add logging to understand failure mode
++3. Fix or disable (with TODO and ticket)
++4. Never ignore flaky tests
++
++### Keep Tests Readable
++
++**Readability Guidelines**:
++- Clear test names (describe what and why)
++- Minimal setup (use fixtures)
++- Obvious assertions (use descriptive matchers)
++- No complex logic in tests
++- Comments for non-obvious test scenarios
++
++**Example**:
++```python
++# GOOD: Clear and readable
++def test_expired_auth_token_is_rejected():
++    token = create_expired_token()
++    result = auth_service.validate(token)
++    assert result.is_error()
++    assert result.error_code == "TOKEN_EXPIRED"
++
++# BAD: Complex and unclear
++def test_auth_2():
++    t = create_token(delta=-3600)  # What does this mean?
++    r = auth.validate(t)
++    assert not r.ok  # Why is it not ok?
++```
++
++---
++
++## Documentation
++
++### Test Documentation Requirements
++
++Every test file should include:
++1. **Purpose**: What is being tested and why
++2. **Setup Requirements**: Any special setup needed
++3. **Running Instructions**: How to run these tests
++4. **Expected Results**: What success looks like
++
++**Example**:
++```python
++"""
++User Authentication Tests
++
++Purpose: Tests for user login, logout, and session management.
++
++Setup: Requires test database and mock email service.
++       Run: docker-compose up test-db
++
++Run Tests: pytest tests/auth/
++
++Expected: All tests should pass with 100% coverage of auth module.
++"""
++```
++
++---
++
++## Further Reading
++
++- **Unit Testing**: `unit_testing.md` - Detailed unit test patterns
++- **Integration Testing**: `integration_testing.md` - Integration test strategies
++- **Test Coverage**: `test_coverage_requirements.md` - Coverage thresholds and tools
++- **FP Testing**: `code_guidelines/principles/functional_programming.md` - FP-specific patterns
++
++---
++
++**Last Updated**: 2025-11-19  
++**Maintained By**: Global AI Rules System  
++**Status**: Active  
++**Applies To**: All AI assistants (Cursor, Kimi, Claude, Gemini) and all human developers