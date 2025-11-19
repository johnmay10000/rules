---
title: Integration Testing Requirements
+category: universal_rules
++type: testing
++applies_to: all
++version: 1.0.0
++last_updated: 2025-11-19
++---
++
++# Integration Testing Requirements
++
++**Status**: MANDATORY - All integration points must be tested
++
++Integration tests verify that multiple components work together correctly. Unlike unit tests that test components in isolation, integration tests validate the interactions between components, external services, databases, and APIs.
++
++---
++
++## Core Principles
++
++### 1. Test Integration Points
++
++**Rule**: Every integration point must have corresponding integration tests.
++
++**Integration Points Include**:
++- Database operations (read, write, transactions)
++- External API calls (REST, GraphQL, gRPC)
++- Message queues (RabbitMQ, Kafka, SQS)
++- File system operations
++- Authentication/authorization services
++- Third-party SDKs and libraries
++- Microservice communications
++
++**Rationale**:
++- Unit tests don't catch integration issues
++- Configuration problems surface at integration level
++- Network issues, timeouts, and failures must be tested
++- Data format mismatches are caught early
++
++---
++
++### 2. Use Real Dependencies When Possible
++
++**Rule**: Prefer real databases, real APIs (sandboxed), and real services over mocks.
++
++**Exceptions** (when to use mocks/stubs):
++- External services that cost money per call
++- Services with rate limits that can't be bypassed
++- Unreliable third-party services
++- Services that don't provide sandbox/test environments
++- Extremely slow operations (> 5 seconds)
++
++**Best Practice**: Use test-specific instances:
++- Test database (PostgreSQL in Docker, SQLite in-memory)
++- Sandbox API keys
++- Local message queue instances
++- Test file system directories (cleaned up after tests)
++
++**Example Setup**:
++```yaml
++# docker-compose.test.yml
++version: '3.8'
++services:
++  test-db:
++    image: postgres:15
++    environment:
++      POSTGRES_DB: testdb
++      POSTGRES_USER: testuser
++      POSTGRES_PASSWORD: testpass
++    ports:
++      - "5433:5432"
++  test-redis:
++    image: redis:7
++    ports:
++      - "6380:6379"
++```
++
++---
++
++### 3. Test Error Paths and Failure Modes
++
++**Rule**: Integration tests must verify error handling, not just happy paths.
++
++**Test These Failure Scenarios**:
++- Network timeouts
++- Service unavailable (503 errors)
++- Authentication failures (401, 403 errors)
++- Invalid request data (400 errors)
++- Database connection failures
++- Transaction rollbacks
++- Message queue failures
++- File system errors (disk full, permission denied)
++
++**Example**:
++```python
++def test_api_timeout_is_handled_gracefully():
++    # Arrange: Configure very short timeout
++    api_client = ApiClient(timeout=0.001)
++    
++    # Act & Assert: Timeout should raise appropriate error
++    with pytest.raises(TimeoutError):
++        api_client.fetch_data()
++    
++    # Verify error is logged and system remains stable
++    assert len(error_logs) == 1
++    assert "API timeout" in error_logs[0].message
++```
++
++---
++
++### 4. Make Tests Independent and Isolated
++
++**Rule**: Each integration test must set up its own data and clean up afterward.
++
++**Requirements**:
++- Tests don't share database state
++- Tests don't share file system state
++- Tests clean up after themselves (even on failure)
++- Tests can run in parallel
++- Tests can run in any order
++
++**Setup/Teardown Pattern**:
++```python
++@pytest.fixture
++def test_database():
++    """Create fresh database for each test"""
++    db = create_test_db()
++    yield db  # Provide to test
++    db.cleanup()  # Clean up after test
++
++def test_user_creation(test_database):
++    user = User(email="test@example.com")
++    test_database.save(user)
++    assert test_database.find(user.id) is not None
++    # Database is cleaned up automatically after test
++```
++
++---
++
++## Integration Test Structure
++
++### AAA Pattern for Integration Tests
++
++Integration tests follow the same AAA pattern as unit tests, but setup is more extensive:
++
++```typescript
++test('should create order and process payment', async () => {
++  // ARRANGE: Set up test environment
++  const testDb = await createTestDatabase()
++  const paymentGateway = new MockPaymentGateway()
++  const emailService = new FakeEmailService()
++  const orderService = new OrderService(testDb, paymentGateway, emailService)
++  
++  const user = await testDb.createUser({ email: 'test@example.com' })
++  const product = await testDb.createProduct({ price: 1000 })
++  
++  // ACT: Execute the workflow
++  const result = await orderService.createOrder({
++    userId: user.id,
++    productIds: [product.id],
++    paymentMethod: 'credit_card'
++  })
++  
++  // ASSERT: Verify all components interacted correctly
++  expect(result.success).toBe(true)
++  expect(result.order.status).toBe('paid')
++  
++  // Verify database state
++  const savedOrder = await testDb.findOrder(result.order.id)
++  expect(savedOrder).toBeDefined()
++  expect(savedOrder.totalAmount).toBe(1000)
++  
++  // Verify payment was processed
++  expect(paymentGateway.charges).toHaveLength(1)
++  expect(paymentGateway.charges[0].amount).toBe(1000)
++  
++  // Verify email was sent
++  expect(emailService.sentEmails).toHaveLength(1)
++  expect(emailService.sentEmails[0].to).toBe('test@example.com')
++  expect(emailService.sentEmails[0].subject).toContain('Order Confirmation')
++})
++```
++
++---
++
++## Test Categories
++
++### 1. Database Integration Tests
++
++**Test These Scenarios**:
++- CRUD operations
++- Transaction rollbacks
++- Connection pooling
++- Query performance
++- Schema migrations
++- Index usage
++- Concurrent access
++
++**Example**:
++```python
++def test_database_transaction_rollback_on_error():
++    # Arrange
++    db = Database()
++    initial_count = db.count('users')
++    
++    # Act & Assert: Error should rollback transaction
++    with pytest.raises(DatabaseError):
++        with db.transaction():
++            db.insert('users', {'email': 'test@example.com'})
++            db.insert('users', {'email': 'invalid'})  # Will fail
++    
++    # Assert: No users were added due to rollback
++    assert db.count('users') == initial_count
++```
++
++---
++
++### 2. API Integration Tests
++
++**Test These Scenarios**:
++- Successful requests (200 OK)
++- Authentication/authorization (401, 403)
++- Invalid requests (400, 422)
++- Not found (404)
++- Server errors (500)
++- Timeout handling
++- Rate limiting
++- Response parsing
++
++**Example**:
++```typescript
++describe('User API Integration', () => {
++  test('should create user and return 201', async () => {
++    const response = await fetch('http://api:3000/users', {
++      method: 'POST',
++      headers: { 'Content-Type': 'application/json' },
++      body: JSON.stringify({
++        email: 'new@example.com',
++        name: 'Alice'
++      })
++    })
++    
++    expect(response.status).toBe(201)
++    const user = await response.json()
++    expect(user.id).toBeDefined()
++    expect(user.email).toBe('new@example.com')
++  })
++  
++  test('should return 400 for invalid email', async () => {
++    const response = await fetch('http://api:3000/users', {
++      method: 'POST',
++      headers: { 'Content-Type': 'application/json' },
++      body: JSON.stringify({
++        email: 'invalid-email',
++        name: 'Alice'
++      })
++    })
++    
++    expect(response.status).toBe(400)
++    const error = await response.json()
++    expect(error.message).toContain('Invalid email format')
++  })
++  
++  test('should handle timeout gracefully', async () => {
++    const controller = new AbortController()
++    setTimeout(() => controller.abort(), 100)
++    
++    await expect(
++      fetch('http://api:3000/slow-endpoint', {
++        signal: controller.signal
++      })
++    ).rejects.toThrow()
++  })
++})
++```
++
++---
++
++### 3. Message Queue Integration Tests
++
++**Test These Scenarios**:
++- Message publishing
++- Message consumption
++- Queue durability
++- Error handling and dead-letter queues
++- Message ordering (if important)
++- Concurrent consumers
++
++**Example**:
++```python
++def test_order_created_event_is_published_and_consumed():
++    # Arrange
++    queue = RabbitMQQueue()
++    order_service = OrderService(queue)
++    email_service = EmailService()
++    
++    # Start consumer in background
++    consumer = queue.consume('order.created', email_service.send_confirmation)
++    
++    # Act: Create order (publishes event)
++    order = order_service.create_order(user_id=123, items=[1, 2, 3])
++    
++    # Wait for async processing
++    time.sleep(0.5)
++    
++    # Assert: Email was sent
++    assert email_service.sent_emails == 1
++    assert email_service.last_email.to == order.user.email
++```
++
++---
++
++### 4. File System Integration Tests
++
++**Test These Scenarios**:
++- File reading and writing
++- Directory creation
++- File permissions
++- Disk full errors
++- File locking
++- Cleanup on errors
++
++**Example**:
++```rust
++#[test]
++fn test_file_processing_cleans_up_on_error() {
++    let temp_dir = TempDir::new().unwrap();
++    let input_file = temp_dir.path().join("input.txt");
++    let output_file = temp_dir.path().join("output.txt");
++    
++    // Create invalid input file
++    fs::write(&input_file, "invalid data").unwrap();
++    
++    // Act: Processing should fail
++    let result = process_file(&input_file, &output_file);
++    assert!(result.is_err());
++    
++    // Assert: Output file should be cleaned up
++    assert!(!output_file.exists());
++    
++    // Temp directory cleaned up automatically when dropped
++}
++```
++
++---
++
++## Test Data Management
++
++### Use Realistic Test Data
++
++```python
++# Good: Realistic test data
++def create_test_user(overrides=None):
++    defaults = {
++        'email': f'test-{uuid4()}@example.com',
++        'name': 'Alice Smith',
++        'age': 30
++    }
++    if overrides:
++        defaults.update(overrides)
++    return User(**defaults)
++
++# Bad: Unrealistic test data
++def create_test_user_bad():
++    return User(
++        email='a@b.c',  # Too simple, doesn't catch validation bugs
++        name='Test',    # Too short
++        age=999         # Unrealistic
++    )
++```
++
++### Avoid Hardcoded IDs
++
++```typescript
++// BAD: Hardcoded IDs can conflict
++test('should find user', async () => {
++  await db.insert('users', { id: 1, name: 'Alice' })
++  const user = await findUser(1)
++  expect(user.name).toBe('Alice')
++})
++
++// GOOD: Generate unique IDs
++test('should find user', async () => {
++  const userId = generateUuid()
++  await db.insert('users', { id: userId, name: 'Alice' })
++  const user = await findUser(userId)
++  expect(user.name).toBe('Alice')
++})
++```
++
++---
++
++## Performance Testing
++
++Integration tests should verify performance characteristics:
++
++```python
++def test_api_response_time_is_acceptable():
++    start_time = time.time()
++    
++    response = requests.post('http://api:3000/orders', json=order_data)
++    
++    end_time = time.time()
++    duration = end_time - start_time
++    
++    assert response.status_code == 201
++    assert duration < 1.0  # Must respond within 1 second
++```
++
++---
++
++## CI/CD Integration
++
++### Run Integration Tests in CI
++
++```yaml
++# .github/workflows/integration-tests.yml
++name: Integration Tests
++
++on: [push, pull_request]
++
++jobs:
++  integration:
++    runs-on: ubuntu-latest
++    
++    services:
++      postgres:
++        image: postgres:15
++        env:
++          POSTGRES_PASSWORD: test
++        options: >-
++          --health-cmd pg_isready
++          --health-interval 10s
++          --health-timeout 5s
++          --health-retries 5
++        ports:
++          - 5432:5432
++    
++    steps:
++      - uses: actions/checkout@v3
++      
++      - name: Setup environment
++        run: ./scripts/setup-integration-tests.sh
++      
++      - name: Run integration tests
++        run: pytest tests/integration/
++        env:
++          DATABASE_URL: postgresql://postgres:test@localhost:5432/testdb
++          API_BASE_URL: http://localhost:3000
++```
++
++---
++
++## Best Practices
++
++### ✅ DO:
++- Test one integration per test (when possible)
++- Use descriptive test names
++- Clean up test data after each test
++- Use transactions to rollback database changes
++- Test both success and failure paths
++- Mock external services that are unreliable or expensive
++- Use test fixtures for common setup
++- Make tests independent and parallelizable
++- Document complex test setups
++
++### ❌ DON'T:
++- Share state between tests
++- Depend on test execution order
++- Leave test data behind
++- Test multiple integrations in one test (hard to debug)
++- Use production databases or services
++- Commit API keys or credentials
++- Ignore intermittent test failures (flaky tests)
++- Skip cleanup on test failure
++
++---
++
++## Common Pitfalls
++
++### Flaky Integration Tests
++
++**Causes**:
++- Timing issues (race conditions)
++- Shared state between tests
++- Unreliable external services
++- Insufficient waits for async operations
++
++**Solutions**:
++- Use proper synchronization (async/await, promises)
++- Increase timeout values
++- Use retries for external service calls
++- Isolate tests completely
++- Use test containers for dependencies
++
++### Slow Integration Tests
++
++**Causes**:
++- Real network calls to external services
++- Database setup/teardown for each test
++- File system I/O
++- No parallelization
++
++**Solutions**:
++- Use in-memory databases when possible
++- Reuse test fixtures across tests
++- Run tests in parallel
++- Mock slow external services
++- Use connection pooling
++
++---
++
++## Tool-Specific Notes
++
++### For Kimi Users
++
++Kimi provides enhanced integration testing support:
++- **Parallel Test Execution**: Run multiple integration tests simultaneously
++- **Subagent Testing**: Spawn subagents for complex multi-service tests
++- **SetTodoList Integration**: Track integration test completion
++- **Test Generation**: Generate integration tests from API specs
++
++**Kimi-Specific Workflow**:
++```bash
++# Run integration tests in parallel
++kimi test --type integration --parallel
++
++# Generate integration tests from OpenAPI spec
++kimi generate-integration-tests api-spec.yaml --output tests/integration/
++
++# Test complete workflow
++kimi test-workflow --name user-registration
++```
++
++### For Cursor Users
++
++Cursor provides IDE-integrated integration testing:
++- **Multi-File Debugging**: Debug across service boundaries
++- **Environment Management**: Switch between test environments
++- **Test Data Generation**: Generate realistic test data
++- **Coverage Visualization**: See integration coverage
++
++### For Claude Users
++
++Claude excels at integration test design:
++- **Workflow Testing**: Design complete user journey tests
++- **Failure Scenario Design**: Plan comprehensive error testing
++- **Service Mocking**: Design effective service mocks
++- **Best Practices**: Integration testing patterns
++
++### For Gemini Users
++
++Gemini provides comprehensive integration testing guidance:
++- **Multi-Service Testing**: Test microservice architectures
++- **Contract Testing**: Design API contract tests
++- **Performance Testing**: Load and stress testing
++- **Observability**: Test logging, metrics, tracing
++
++---
++
++## Related Documents
++
++- **Testing Philosophy**: `testing_philosophy.md` - Core testing principles
++- **Unit Testing**: `unit_testing.md` - Unit test requirements
++- **Test Coverage**: `test_coverage_requirements.md` - Coverage thresholds
++- **Git Checkpoint Rules**: `../git/git_checkpoint_rules.md` - When to commit
++
++---
++
++**Last Updated**: 2025-11-19  
++**Maintained By**: Global AI Rules System  
++**Status**: Active  
++**Applies To**: All AI assistants (Cursor, Kimi, Claude, Gemini) and all programming languages