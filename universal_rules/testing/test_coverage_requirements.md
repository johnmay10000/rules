---
title: Test Coverage Requirements
+category: universal_rules
+type: testing
+applies_to: all
+version: 1.0.0
+last_updated: 2025-11-19
+---
+
+# Test Coverage Requirements
+
+**Status**: MANDATORY - All code must meet minimum coverage thresholds
+
+Universal test coverage requirements applicable to all projects, languages, and AI assistants (Cursor, Kimi, Claude, Gemini).
+
+---
+
+## Coverage Thresholds
+
+### Minimum Requirements
+
+| Coverage Type | Minimum | Target | Critical Paths |
+|---------------|---------|--------|----------------|
+| **Overall Coverage** | 80% | 90% | 100% |
+| **Branch Coverage** | 75% | 85% | 100% |
+| **Function Coverage** | 80% | 90% | 100% |
+| **Line Coverage** | 80% | 90% | 100% |
+
+### Critical Paths (Must be 100%)
+
+The following must have **100% coverage**:
+
+- ✅ Authentication and authorization
+- ✅ Payment processing
+- ✅ Data validation and sanitization
+- ✅ Error handling and recovery
+- ✅ Security-sensitive code (encryption, hashing)
+- ✅ Core business logic
+- ✅ API endpoints
+- ✅ Database queries and transactions
+- ✅ External service integrations
+- ✅ Configuration and environment handling
+
+---
+
+## Types of Tests Required
+
+### 1. Unit Tests (Mandatory)
+
+**Definition**: Tests individual functions/methods in isolation
+
+**Requirements**:
+- Every public function must have unit tests
+- Test both success and error paths
+- Use property-based testing where applicable
+- Mock external dependencies (database, API calls, file I/O)
+- Test edge cases and boundary conditions
+
+**Example Structure**:
+```python
+# Python with pytest
+def test_function_name_condition_expected():
+    """Test that function X does Y when Z"""
+    # Arrange
+    input = ...
+    
+    # Act
+    result = function_to_test(input)
+    
+    # Assert
+    assert result == expected
+    assert isinstance(result, ExpectedType)
+```
+
+**Coverage Target**: 100% of pure functions, 90% of impure functions
+
+---
+
+### 2. Integration Tests (Mandatory)
+
+**Definition**: Tests interaction between multiple components
+
+**Requirements**:
+- Test database integration
+- Test API endpoints
+- Test external service calls
+- Test authentication flows
+- Test complete user workflows
+- Use test databases and sandboxed environments
+
+**Example Structure**:
+```typescript
+// TypeScript with vitest
+describe('User Registration Flow', () => {
+  test('should create user and send welcome email', async () => {
+    // Arrange: Setup test database
+    const testDb = await createTestDatabase()
+    
+    // Act: Execute the workflow
+    const result = await registerUser(testUserData)
+    
+    // Assert: Verify all components worked
+    expect(result.user).toBeDefined()
+    expect(result.emailSent).toBe(true)
+    expect(await testDb.findUser(result.user.id)).toBeDefined()
+  })
+})
+```
+
+**Coverage Target**: 80% of integration points, 100% of critical workflows
+
+---
+
+### 3. End-to-End Tests (Recommended)
+
+**Definition**: Tests complete user journeys from start to finish
+
+**Requirements**:
+- Test critical user paths (login, purchase, data export)
+- Use realistic test data
+- Run in production-like environment
+- Test on multiple browsers/devices (for web apps)
+- Include performance assertions
+
+**Example Structure**:
+```typescript
+// Playwright for web E2E
+test('complete purchase flow', async ({ page }) => {
+  // Login
+  await page.goto('/login')
+  await page.fill('#email', 'test@example.com')
+  await page.fill('#password', 'password123')
+  await page.click('button[type="submit"]')
+  
+  // Add item to cart
+  await page.goto('/products/123')
+  await page.click('#add-to-cart')
+  
+  // Checkout
+  await page.goto('/checkout')
+  await page.fill('#card-number', '4242424242424242')
+  await page.click('#complete-purchase')
+  
+  // Verify success
+  await expect(page.locator('#success-message')).toBeVisible()
+})
+```
+
+**Coverage Target**: 100% of critical user journeys
+
+---
+
+### 4. Property-Based Tests (Recommended)
+
+**Definition**: Tests properties that should hold for all inputs
+
+**Requirements**:
+- Test algebraic laws (associativity, identity, commutativity)
+- Test invariants and postconditions
+- Use for pure functions and data structures
+- Generate diverse test cases automatically
+
+**Example Structure**:
+```python
+# Python with hypothesis
+from hypothesis import given, strategies as st
+
+@given(st.integers(), st.integers())
+def test_addition_is_commutative(a, b):
+    assert add(a, b) == add(b, a)
+
+@given(st.lists(st.integers()))
+def test_reversing_twice_returns_original(items):
+    assert reverse(reverse(items)) == items
+```
+
+**Coverage Target**: All pure functions with algebraic properties
+
+---
+
+## What to Test
+
+### ✅ Must Test
+
+- **All public functions/methods**
+- **All error handling paths**
+- **All conditional branches** (if/else, match/case)
+- **Boundary conditions** (empty collections, max values, null/None)
+- **Edge cases** (negative numbers, zero, empty strings)
+- **Type conversions and validations**
+- **Concurrency and race conditions**
+- **Performance-critical paths**
+
+### ✅ Should Test
+
+- **Private functions** (if complex logic)
+- **Integration points** (APIs, databases, external services)
+- **Configuration variations**
+- **Failure scenarios** (network errors, timeouts, disk full)
+- **Security boundaries** (authentication, authorization, input sanitization)
+
+### ❌ Don't Test
+
+- **Simple getters/setters** (unless they have logic)
+- **Third-party libraries** (test at integration boundaries only)
+- **Generated code** (test the generator, not the output)
+- **Configuration files** (test the parser, not static data)
+- **Declarative code** (HTML, CSS, JSON schemas - unless dynamic)
+
+---
+
+## Test Quality Requirements
+
+### Test Structure
+
+Every test must follow **AAA pattern**:
+
+1. **Arrange**: Set up test data and dependencies
+2. **Act**: Execute the code being tested
+3. **Assert**: Verify the expected outcomes
+
+```typescript
+// Good: Clear AAA structure
+test('should calculate total price with tax', () => {
+  // Arrange
+  const items = [{ price: 10, quantity: 2 }, { price: 5, quantity: 1 }]
+  const taxRate = 0.08
+  
+  // Act
+  const result = calculateTotal(items, taxRate)
+  
+  // Assert
+  expect(result).toBe(27.0) // (10*2 + 5*1) * 1.08
+  expect(result).toBeGreaterThan(0)
+})
+```
+
+### Test Naming
+
+Tests must be **descriptive and specific**:
+
+```typescript
+// BAD: Vague test name
+test('test1', () => { ... })
+test('user', () => { ... })
+
+// GOOD: Descriptive test name
+test('should return error when email is invalid format', () => { ... })
+test('should create user with valid data and send welcome email', () => { ... })
+```
+
+**Format**: `should [expected behavior] when [condition]`
+
+### Test Independence
+
+- Each test must be **independent**
+- No shared state between tests
+- Use `beforeEach`/`afterEach` for setup/teardown
+- Tests can run in any order
+
+```typescript
+// BAD: Shared state
+let sharedUser: User
+
+test('create user', () => {
+  sharedUser = createUser('test@example.com')
+})
+
+test('update user', () => {
+  updateUser(sharedUser.id, { name: 'New Name' }) // Fails if test order changes!
+})
+
+// GOOD: Independent tests
+test('create user', () => {
+  const user = createUser('test@example.com')
+  expect(user).toBeDefined()
+})
+
+test('update user', () => {
+  const user = createUser('test@example.com') // Create fresh user
+  const updated = updateUser(user.id, { name: 'New Name' })
+  expect(updated.name).toBe('New Name')
+})
+```
+
+---
+
+## Coverage Enforcement
+
+### Automated Enforcement
+
+Add to CI/CD pipeline:
+
+```yaml
+# GitHub Actions example
+- name: Run tests with coverage
+  run: npm test -- --coverage
+
+- name: Check coverage threshold
+  run: |
+    COVERAGE=$(cat coverage/lcov.info | grep -oP 'lines:\K\d+' | head -1)
+    if [ "$COVERAGE" -lt 80 ]; then
+      echo "Coverage $COVERAGE% is below 80% threshold"
+      exit 1
+    fi
+```
+
+### Pre-commit Hooks
+
+```bash
+#!/bin/bash
+# .git/hooks/pre-commit
+
+# Run tests
+npm test -- --coverage --watchAll=false
+
+# Check coverage
+COVERAGE=$(cat coverage/lcov.info | grep -oP 'lines:\K\d+' | head -1)
+if [ "$COVERAGE" -lt 80 ]; then
+  echo "❌ Coverage $COVERAGE% is below 80% threshold"
+  echo "Add tests before committing"
+  exit 1
+fi
+
+echo "✅ Coverage check passed"
+```
+
+### Package.json Scripts
+
+```json
+{
+  "scripts": {
+    "test": "jest",
+    "test:coverage": "jest --coverage",
+    "test:coverage:check": "jest --coverage --coverageThreshold='{\"global\":{\"lines\":80,\"branches\":75,\"functions\":80}}'"
+  }
+}
+```
+
+---
+
+## Language-Specific Examples
+
+### Python (pytest + coverage)
+
+```python
+# test_calculator.py
+import pytest
+from calculator import add, divide
+
+def test_add_positive_numbers():
+    assert add(2, 3) == 5
+
+def test_add_negative_numbers():
+    assert add(-2, -3) == -5
+
+def test_add_mixed_numbers():
+    assert add(-2, 3) == 1
+
+def test_divide_by_zero():
+    with pytest.raises(ValueError):
+        divide(10, 0)
+
+def test_divide_valid():
+    assert divide(10, 2) == 5
+```
+
+**Run with coverage**:
+```bash
+pytest --cov=. --cov-report=html --cov-fail-under=80
+```
+
+### TypeScript (vitest + c8)
+
+```typescript
+// calculator.test.ts
+import { describe, it, expect } from 'vitest'
+import { add, divide } from './calculator'
+
+describe('Calculator', () => {
+  it('should add positive numbers', () => {
+    expect(add(2, 3)).toBe(5)
+  })
+
+  it('should add negative numbers', () => {
+    expect(add(-2, -3)).toBe(-5)
+  })
+
+  it('should handle division by zero', () => {
+    expect(() => divide(10, 0)).toThrow('Division by zero')
+  })
+
+  it('should divide valid numbers', () => {
+    expect(divide(10, 2)).toBe(5)
+  })
+})
+```
+
+**Run with coverage**:
+```bash
+vitest --coverage --coverage.thresholds.lines=80
+```
+
+### Rust (cargo test + tarpaulin)
+
+```rust
+// src/calculator.rs
+pub fn add(a: i32, b: i32) -> i32 {
+    a + b
+}
+
+pub fn divide(a: f64, b: f64) -> Result<f64, String> {
+    if b == 0.0 {
+        Err("Division by zero".to_string())
+    } else {
+        Ok(a / b)
+    }
+}
+
+#[cfg(test)]
+mod tests {
+    use super::*;
+
+    #[test]
+    fn test_add_positive() {
+        assert_eq!(add(2, 3), 5);
+    }
+
+    #[test]
+    fn test_add_negative() {
+        assert_eq!(add(-2, -3), -5);
+    }
+
+    #[test]
+    fn test_divide_by_zero() {
+        let result = divide(10.0, 0.0);
+        assert!(result.is_err());
+    }
+
+    #[test]
+    fn test_divide_valid() {
+        let result = divide(10.0, 2.0);
+        assert_eq!(result.unwrap(), 5.0);
+    }
+}
+```
+
+**Run with coverage**:
+```bash
+cargo tarpaulin --out Html --output-dir ./coverage --fail-under 80
+```
+
+---
+
+## Tool-Specific Notes
+
+### For Kimi Users
+
+Kimi provides enhanced testing support:
+- **Parallel Test Execution**: Run multiple test suites simultaneously
+- **Coverage Analysis**: Automatic coverage report generation
+- **Test Generation**: Can generate tests from specifications
+- **SetTodoList Integration**: Track test completion in TODO lists
+
+Example Kimi workflow:
+```bash
+# Run tests in parallel
+kimi test --parallel --coverage
+
+# Generate tests for new function
+kimi generate-tests --function=calculateTotal --coverage=80
+```
+
+### For Cursor Users
+
+Cursor provides IDE-integrated testing:
+- **Inline Test Runner**: Run tests from the editor
+- **Coverage Visualization**: See uncovered lines in real-time
+- **Test Generation**: Generate tests from implementation
+- **Debug Integration**: Debug tests directly in IDE
+
+### For Claude Users
+
+Claude excels at test strategy:
+- **Test Planning**: Design comprehensive test suites
+- **Edge Case Identification**: Find corner cases to test
+- **Test Review**: Review existing tests for gaps
+- **Best Practices**: Provide testing guidance
+
+### For Gemini Users
+
+Gemini provides comprehensive testing guidance:
+- **Multi-Language Support**: Testing patterns across languages
+- **Framework Comparison**: Choose optimal testing tools
+- **Advanced Techniques**: Property-based testing, fuzzing
+- **CI/CD Integration**: Set up automated testing pipelines
+
+---
+
+## Pre-Commit Checklist
+
+Before committing code, verify:
+
+- [ ] All unit tests pass
+- [ ] All integration tests pass
+- [ ] Coverage meets 80% threshold overall
+- [ ] Coverage is 100% for critical paths
+- [ ] No flaky tests (run tests 3 times to verify)
+- [ ] Tests are independent (can run in any order)
+- [ ] Test names are descriptive
+- [ ] Edge cases are covered
+- [ ] Error paths are tested
+- [ ] Mocked dependencies are properly reset
+- [ ] Test data is realistic but not production data
+- [ ] Performance tests pass (if applicable)
+
+---
+
+## Common Pitfalls
+
+### ❌ Testing Implementation Details
+
+```typescript
+// BAD: Tests internal implementation
+test('should set internal flag', () => {
+  const user = createUser('test@example.com')
+  expect(user._internalFlag).toBe(true) // Tests private detail
+})
+
+// GOOD: Tests public behavior
+test('should create active user', () => {
+  const user = createUser('test@example.com')
+  expect(user.isActive).toBe(true) // Tests public API
+})
+```
+
+### ❌ Brittle Tests
+
+```typescript
+// BAD: Tests exact string match
+test('should format error message', () => {
+  const error = validateEmail('invalid')
+  expect(error).toBe('Email is not valid') // Brittle if message changes
+})
+
+// GOOD: Tests structure, not exact text
+test('should return validation error', () => {
+  const error = validateEmail('invalid')
+  expect(error.type).toBe('ValidationError')
+  expect(error.field).toBe('email')
+})
+```
+
+### ❌ Not Testing Error Paths
+
+```typescript
+// BAD: Only tests happy path
+test('should process payment', () => {
+  const result = processPayment(validCard)
+  expect(result.success).toBe(true)
+})
+
+// GOOD: Tests both success and failure
+test('should process payment with valid card', () => {
+  const result = processPayment(validCard)
+  expect(result.success).toBe(true)
+})
+
+test('should reject payment with invalid card', () => {
+  const result = processPayment(invalidCard)
+  expect(result.success).toBe(false)
+  expect(result.error).toBeDefined()
+})
+
+test('should handle network timeout', () => {
+  const result = processPayment(slowCard)
+  expect(result.success).toBe(false)
+  expect(result.error).toBe('Timeout')
+})
+```
+
+---
+
+## Related Documents
+
+- **Testing Philosophy**: `testing_philosophy.md`
+- **Unit Testing**: `unit_testing.md`
+- **Integration Testing**: `integration_testing.md`
+- **Git Checkpoint Rules**: `../git/git_checkpoint_rules.md`
+
+---
+
+**Last Updated**: 2025-11-19  
+**Maintained By**: Global AI Rules System  
+**Status**: Active  
+**Applies To**: All AI assistants (Cursor, Kimi, Claude, Gemini) and all programming languages