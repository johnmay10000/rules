---
title: Unit Testing Requirements
+category: universal_rules
+type: testing
+applies_to: all
+version: 1.0.0
+last_updated: 2025-11-19
+---
+
+# Unit Testing Requirements
+
+**Status**: MANDATORY - All code must have unit tests
+
+Unit testing is a non-negotiable requirement for all code across all projects, languages, and AI assistants. This document defines universal unit testing standards that apply regardless of technology stack.
+
+---
+
+## Core Principles
+
+### 1. Tests Are Mandatory
+
+**Rule**: No code is complete without corresponding unit tests.
+
+- Every function, method, or logic unit must have tests
+- Tests are written **before** or **concurrently with** implementation (TDD preferred)
+- Pull requests without tests will be rejected
+- Test coverage must meet minimum thresholds (see Coverage Requirements below)
+
+**Rationale**: 
+- Tests prove code works as intended
+- Tests serve as executable documentation
+- Tests enable safe refactoring
+- Tests catch regressions early
+
+---
+
+### 2. Tests Must Be Independent and Isolated
+
+**Rule**: Each test must be able to run in any order, alone, or in parallel.
+
+- **No shared state** between tests
+- **No dependencies** on test execution order
+- **No external dependencies** (databases, APIs, file systems) without isolation
+- **Clean up** after each test (if any mutable state is created)
+
+**Anti-Pattern**:
+```python
+# BAD: Shared state between tests
+shared_counter = 0
+
+def test_increment():
+    global shared_counter
+    shared_counter += 1
+    assert shared_counter == 1
+
+def test_increment_again():
+    global shared_counter
+    shared_counter += 1
+    assert shared_counter == 2  # Fails if tests run in different order
+```
+
+**Correct Pattern**:
+```python
+# GOOD: Each test is isolated
+def test_increment():
+    counter = 0
+    result = increment(counter)
+    assert result == 1
+
+def test_increment_again():
+    counter = 0
+    result = increment(counter)
+    assert result == 1  # Always passes, independent of other tests
+```
+
+---
+
+### 3. Tests Must Be Deterministic
+
+**Rule**: Same input → same output, every time, regardless of environment.
+
+- **No randomness** in test logic (unless explicitly controlled)
+- **No time-based dependencies** (use fixed timestamps)
+- **No network calls** to external services
+- **No filesystem I/O** (use in-memory alternatives)
+- **No concurrency** unless testing concurrency itself
+
+**Anti-Pattern**:
+```typescript
+// BAD: Non-deterministic test
+test('generates random token', () => {
+  const token = generateToken() // Uses Math.random()
+  expect(token.length).toBe(10) // Might pass or fail randomly
+})
+```
+
+**Correct Pattern**:
+```typescript
+// GOOD: Deterministic test with controlled randomness
+test('generates token with fixed seed', () => {
+  const rng = createRNG('fixed-seed-123')
+  const token = generateToken(rng) // Deterministic with seed
+  expect(token).toBe('a3f9b2c1d4') // Always the same
+})
+```
+
+---
+
+### 4. Tests Must Be Fast
+
+**Rule**: Unit tests should run in milliseconds, full suite in seconds.
+
+- **Target**: < 10ms per test
+- **Maximum**: < 100ms per test
+- **Full suite**: < 10 seconds for 100 tests
+- **Slow tests** are marked and run separately (integration tests)
+
+**Why Speed Matters**:
+- Fast tests run frequently (before every commit)
+- Fast tests don't interrupt development flow
+- Fast tests enable TDD
+- Fast tests provide rapid feedback
+
+**Anti-Pattern**:
+```python
+# BAD: Slow test (takes 2 seconds)
+def test_with_database():
+    db = connect_to_database()  # Slow connection
+    result = db.query("SELECT * FROM users")
+    assert len(result) > 0
+```
+
+**Correct Pattern**:
+```python
+# GOOD: Fast test with fake (in-memory) database
+def test_with_fake_database():
+    db = FakeDatabase()  # In-memory, instant
+    db.add(User(id=1, name="Alice"))
+    result = db.find_all()
+    assert len(result) == 1
+```
+
+---
+
+## Test Structure: AAA Pattern
+
+All unit tests **MUST** follow the **AAA** pattern:
+
+1. **Arrange**: Set up test data and context
+2. **Act**: Execute the code being tested
+3. **Assert**: Verify the expected outcome
+
+### Example: AAA Pattern
+
+```rust
+#[test]
+fn test_calculate_discount() {
+    // ARRANGE: Set up test data
+    let price = 100.0;
+    let discount_percent = 20.0;
+    
+    // ACT: Execute the code
+    let result = calculate_discount(price, discount_percent);
+    
+    // ASSERT: Verify the outcome
+    assert_eq!(result, 80.0);
+}
+```
+
+**Benefits of AAA**:
+- Clear structure makes tests readable
+- Easy to identify what is being tested
+- Consistent pattern across all tests
+- Easy to debug when tests fail
+
+---
+
+## Coverage Requirements
+
+### Minimum Coverage Thresholds
+
+| Code Type | Coverage Requirement | Rationale |
+|-----------|---------------------|-----------|
+| **Business Logic** | 100% | Critical paths must be fully tested |
+| **Utilities/Helpers** | 100% | Reused code must be reliable |
+| **Domain Models** | 100% | Core domain must be fully validated |
+| **API Endpoints** | 95% | High coverage for external interfaces |
+| **Error Handling** | 100% | Error paths are critical |
+| **Overall** | **Minimum 80%** | Project-wide baseline |
+| **Critical Paths** | **100%** | No untested critical code |
+
+### What Counts Toward Coverage
+
+- ✅ Unit tests for functions and methods
+- ✅ Property-based tests
+- ✅ Parameterized tests
+- ✅ Edge case tests
+- ✅ Error path tests
+
+### What Does NOT Count
+
+- ❌ Integration tests (separate category)
+- ❌ Manual testing
+- ❌ Code without assertions
+- ❌ Tests that don't verify behavior
+
+### Measuring Coverage
+
+**Python**:
+```bash
+pytest --cov=src --cov-report=html --cov-fail-under=80
+```
+
+**TypeScript**:
+```bash
+vitest --coverage --coverage.thresholds.lines=80
+```
+
+**Rust**:
+```bash
+cargo tarpaulin --out Html --fail-under 80
+```
+
+**All Languages**: Coverage must be measured automatically in CI/CD
+
+---
+
+## Test Naming Conventions
+
+### Format: `test_[unit_under_test]_[scenario]_[expected_outcome]`
+
+**Examples**:
+
+```python
+# Good: Clear, descriptive names
+def test_calculate_discount_with_valid_percentage():
+    # ...
+
+def test_calculate_discount_with_zero_percent_returns_original():
+    # ...
+
+def test_calculate_discount_with_negative_percent_raises_error():
+    # ...
+
+def test_user_repository_find_by_id_returns_user_when_exists():
+    # ...
+
+def test_user_repository_find_by_id_returns_none_when_not_found():
+    # ...
+```
+
+**Anti-Patterns**:
+```python
+# BAD: Vague names
+def test_discount():
+    # What scenario? What outcome?
+    pass
+
+def test_user():
+    # What about the user?
+    pass
+
+def test_1():
+    # Completely meaningless
+    pass
+```
+
+---
+
+## Test Organization
+
### Directory Structure
+
+```
+project/
+├── src/                      # Source code
+│   ├── domain/
+│   ├── services/
+│   └── utils/
+├── tests/                    # Test files
+│   ├── unit/                 # Unit tests only
+│   │   ├── test_domain.py
+│   │   ├── test_services.py
+│   │   └── test_utils.py
+│   ├── integration/          # Integration tests
+│   └── conftest.py           # Test fixtures
+└── docs/
+```
+
+**Rule**: Mirror source structure in test directory
+
+```
+src/
+  domain/
+    user.py
+tests/
+  unit/
+    domain/
+      test_user.py  # Mirrors src structure
+```
+
+---
+
+## Test Doubles (Mocks, Stubs, Fakes)
+
+### When to Use Test Doubles
+
+Use test doubles when testing code that depends on:
+- External services (APIs, databases)
+- File system I/O
+- Network calls
+- Time-based operations
+- Random number generation
+- System resources
+
+### Types of Test Doubles
+
+1. **Fake**: Working implementation for testing (in-memory database)
+2. **Stub**: Returns predefined responses
+3. **Mock**: Records interactions for verification
+4. **Spy**: Wraps real object to record calls
+
+### Example: Using Fakes
+
+```typescript
+// Production code
+interface EmailService {
+  send(to: string, subject: string, body: string): Promise<void>
+}
+
+class RealEmailService implements EmailService {
+  async send(to: string, subject: string, body: string): Promise<void> {
+    // Calls real email API
+    await fetch('https://api.sendgrid.com/v3/mail/send', {
+      method: 'POST',
+      body: JSON.stringify({ to, subject, body })
+    })
+  }
+}
+
+// Test fake (in-memory, fast)
+class FakeEmailService implements EmailService {
+  private sentEmails: Array<{ to: string; subject: string; body: string }> = []
+  
+  async send(to: string, subject: string, body: string): Promise<void> {
+    this.sentEmails.push({ to, subject, body })
+  }
+  
+  getSentEmails() {
+    return this.sentEmails
+  }
+}
+
+// Usage in test
+test('sends welcome email to new user', async () => {
+  const emailService = new FakeEmailService()
+  const userService = new UserService(emailService)
+  
+  await userService.register('alice@example.com', 'Alice')
+  
+  const sent = emailService.getSentEmails()
+  expect(sent).toHaveLength(1)
+  expect(sent[0].to).toBe('alice@example.com')
+  expect(sent[0].subject).toBe('Welcome!')
+})
+```
+
+---
+
+## Edge Cases and Boundary Conditions
+
+### Must Test These Scenarios
+
+1. **Empty inputs**: Empty arrays, strings, objects
+2. **Null/None values**: Missing optional values
+3. **Boundary values**: Min/max values, off-by-one errors
+4. **Invalid inputs**: Malformed data, wrong types
+5. **Large inputs**: Performance with maximum size data
+6. **Unicode/special characters**: Non-ASCII text, emojis
+7. **Concurrent access**: Race conditions (if applicable)
+
+### Example: Boundary Testing
+
+```rust
+#[test]
+fn test_array_bounds() {
+    // Empty array
+    assert_eq!(sum(&[]), 0);
+    
+    // Single element
+    assert_eq!(sum(&[5]), 5);
+    
+    // Two elements
+    assert_eq!(sum(&[1, 2]), 3);
+    
+    // Many elements
+    let large: Vec<i32> = (1..=1000).collect();
+    assert_eq!(sum(&large), 500500);
+}
+
+#[test]
+fn test_string_edge_cases() {
+    // Empty string
+    assert_eq!(reverse(""), "");
+    
+    // Single character
+    assert_eq!(reverse("a"), "a");
+    
+    // Unicode
+    assert_eq!(reverse("hello 🌍"), "🌍 olleh");
+}
+```
+
+---
+
+## Property-Based Testing
+
+### What is Property-Based Testing?
+
+Instead of testing specific examples, test **properties** that should hold for all inputs.
+
+### Example: Property-Based Test
+
+```python
+from hypothesis import given, strategies as st
+
+def reverse(s: str) -> str:
+    return s[::-1]
+
+# Property: reversing twice returns original
+@given(st.text())
+def test_reverse_is_involution(s: str):
+    assert reverse(reverse(s)) == s
+
+# Property: length is preserved
+@given(st.text())
+def test_reverse_preserves_length(s: str):
+    assert len(reverse(s)) == len(s)
+
+# Property: concatenation reverses in reverse order
+@given(st.text(), st.text())
+def test_reverse_of_concat(a: str, b: str):
+    assert reverse(a + b) == reverse(b) + reverse(a)
+```
+
+**Benefits**:
+- Finds edge cases automatically
+- Tests many more cases than manual examples
+- Reveals assumptions in code
+- Excellent for pure functions
+
+---
+
+## Common Anti-Patterns (DO NOT DO)
+
+### ❌ Test Without Assertions
+
+```python
+# BAD: No verification
+def test_process_data():
+    data = [1, 2, 3]
+    process_data(data)  # What are we testing?
+```
+
+**Correct**:
+```python
+# GOOD: Clear assertion
+def test_process_data_doubles_values():
+    data = [1, 2, 3]
+    result = process_data(data)
+    assert result == [2, 4, 6]
+```
+
+### ❌ Testing Implementation Details
+
+```typescript
+// BAD: Tests internal structure
+test('user has private _id field', () => {
+  const user = new User('123', 'Alice')
+  expect(user._id).toBe('123')  # Tests private field
+})
+
+// GOOD: Tests public behavior
+test('user.getId returns correct id', () => {
+  const user = new User('123', 'Alice')
+  expect(user.getId()).toBe('123')
+})
+```
+
+### ❌ Tests with Side Effects
+
+```python
+# BAD: Test modifies global state
+def test_creates_file():
+    with open('test.txt', 'w') as f:
+        f.write('test')
+    assert os.path.exists('test.txt')
+    # File left behind - affects other tests
+```
+
+**Correct**:
+```python
+# GOOD: Cleanup after test
+def test_creates_file():
+    filename = 'test.txt'
+    try:
+        with open(filename, 'w') as f:
+            f.write('test')
+        assert os.path.exists(filename)
+    finally:
+        if os.path.exists(filename):
+            os.remove(filename)
+```
+
+### ❌ Flaky Tests
+
+```typescript
+// BAD: Depends on timing
+test('debounce calls function after delay', async () => {
+  const fn = jest.fn()
+  const debounced = debounce(fn, 100)
+  
+  debounced()
+  await sleep(100)  # Might be flaky on slow CI
+  
+  expect(fn).toHaveBeenCalled()
+})
+```
+
+**Correct**:
+```typescript
+// GOOD: Use fake timers
+test('debounce calls function after delay', () => {
+  jest.useFakeTimers()
+  const fn = jest.fn()
+  const debounced = debounce(fn, 100)
+  
+  debounced()
+  jest.advanceTimersByTime(100)  # Deterministic
+  
+  expect(fn).toHaveBeenCalled()
+  jest.useRealTimers()
+})
+```
+
+---
+
+## Test Documentation
+
+### Document Complex Tests
+
+```rust
+/// Test: validate_email accepts valid formats
+/// 
+/// Valid formats tested:
+/// - simple@example.com
+/// - user.name+tag@example.co.uk
+/// - x@y.z (single character local part)
+/// 
+/// This test ensures our email validation regex
+/// matches RFC 5322 compliant addresses.
+#[test]
+fn test_validate_email_accepts_valid_formats() {
+    let valid_emails = vec![
+        "simple@example.com",
+        "user.name+tag@example.co.uk",
+        "x@y.z"
+    ];
+    
+    for email in valid_emails {
+        assert!(validate_email(email).is_ok(),
+            "Should accept valid email: {}", email);
+    }
+}
+```
+
+---
+
+## Integration with CI/CD
+
+### Automated Test Execution
+
+All tests **MUST** run automatically on:
+- Every commit
+- Every pull request
+- Before merging to main
+- Nightly builds
+
+### CI Configuration Example
+
+```yaml
+# .github/workflows/test.yml
+name: Tests
+
+on: [push, pull_request]
+
+jobs:
+  test:
+    runs-on: ubuntu-latest
+    steps:
+      - uses: actions/checkout@v3
+      
+      - name: Setup environment
+        run: ./scripts/setup.sh
+      
+      - name: Run unit tests
+        run: ./scripts/test-unit.sh
+      
+      - name: Check coverage
+        run: ./scripts/check-coverage.sh --min=80
+      
+      - name: Upload coverage
+        uses: codecov/codecov-action@v3
+```
+
+### Required Checks
+
+Before merging:
+- ✅ All unit tests pass
+- ✅ Coverage meets minimum threshold (80%)
+- ✅ No flaky tests
+- ✅ Tests are fast (< 10ms average)
+- ✅ No test warnings or deprecations
+
+---
+
+## Tool-Specific Notes
+
+### For Kimi Users
+
+Kimi provides enhanced testing support:
+- **Parallel Test Generation**: Generate multiple test cases simultaneously
+- **Property-Based Test Suggestions**: Identify properties to test automatically
+- **Coverage Analysis**: Analyze which code paths lack tests
+- **Test Refactoring**: Improve existing test quality
+
+### For Cursor Users
+
+Cursor provides IDE-integrated testing:
+- **Inline Test Generation**: Generate tests from function signatures
+- **Coverage Visualization**: See uncovered lines in editor
+- **Debug Integration**: Debug tests directly in IDE
+- **Test Runner**: Run tests with inline results
+
+### For Claude Users
+
+Claude excels at test design:
+- **Test Strategy**: Design comprehensive test suites
+- **Edge Case Identification**: Find obscure edge cases
+- **Test Refactoring**: Improve test maintainability
+- **Best Practices**: Detailed testing guidance
+
+### For Gemini Users
+
+Gemini provides comprehensive testing education:
+- **Testing Theory**: Deep explanations of testing concepts
+- **Multi-Language**: Testing patterns across languages
+- **Advanced Techniques**: Property-based testing, TDD, BDD
+- **Anti-Pattern Detection**: Identify test smells
+
+---
+
+## Further Reading
+
+- **Testing Philosophy**: `testing_philosophy.md`
+- **Integration Testing**: `integration_testing.md`
+- **Test Coverage**: `test_coverage_requirements.md`
+- **Language-Specific Guides**: See `code_guidelines/languages/[lang]/`
+
+---
+
+**Last Updated**: 2025-11-19  
+**Maintained By**: Global AI Rules System  
+**Status**: Active  
+**Applies To**: All AI assistants (Cursor, Kimi, Claude, Gemini) and all programming languages