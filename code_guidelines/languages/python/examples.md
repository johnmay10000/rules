---
title: Python Functional Programming Examples
+language: python
+category: code_guidelines
+type: examples
+applies_to: [cursor, kimi, claude, gemini]
+version: 2.0.0
+last_updated: 2025-11-19
+---
+
+# Python Functional Programming Examples
+
+Comprehensive code examples demonstrating functional programming patterns in Python.
+
+---
+
+## Example 1: Error Handling with Result Types
+
+### Before (Imperative with Exceptions)
+
+```python
+def divide(a: float, b: float) -> float:
+    """Dangerous: can raise unexpected exceptions"""
+    return a / b  # ZeroDivisionError if b == 0
+
+def process_data(data: List[float]) -> List[float]:
+    results = []
+    for value in data:
+        try:
+            result = divide(10.0, value)
+            results.append(result)
+        except ZeroDivisionError:
+            print(f"Error: Cannot divide by zero for value {value}")
+            continue
+    return results
+
+# Usage
+data = [2.0, 0.0, 5.0, 0.0]
+results = process_data(data)  # Prints errors, continues silently
+print(results)  # [5.0, 2.0]
+```
+
+### After (Functional with Result)
+
+```python
+from returns.result import Result, Success, Failure
+from returns.pipeline import flow
+from returns.pointfree import bind
+from typing import List
+
+def divide(a: float, b: float) -> Result[float, str]:
+    """Pure function: errors are part of the return type"""
+    if b == 0:
+        return Failure("Cannot divide by zero")
+    return Success(a / b)
+
+def process_data_safe(data: List[float]) -> List[Result[float, str]]:
+    """Returns list of Results - callers must handle errors explicitly"""
+    return [divide(10.0, value) for value in data]
+
+# Usage
+data = [2.0, 0.0, 5.0, 0.0]
+results = process_data_safe(data)
+
+# Explicit error handling
+for i, result in enumerate(results):
+    match result:
+        case Success(value):
+            print(f"Result {i}: {value}")
+        case Failure(error):
+            print(f"Error {i}: {error}")
+
+# Output:
+# Result 0: 5.0
+# Error 1: Cannot divide by zero
+# Result 2: 2.0
+# Error 3: Cannot divide by zero
+
+# Railway-oriented: stop on first error
+def process_all_or_fail(data: List[float]) -> Result[List[float], str]:
+    def combine(current: Result[List[float], str], value: float) -> Result[List[float], str]:
+        return current.bind(lambda acc: divide(10.0, value).map(lambda x: acc + [x]))
+    
+    return flow(
+        Success([]),
+        lambda acc: reduce(combine, data, acc)
+    )
+
+final_result = process_all_or_fail([2.0, 5.0])  # Success([5.0, 2.0])
+final_result = process_all_or_fail([2.0, 0.0])  # Failure("Cannot divide by zero")
+```
+
+---
+
+## Example 2: Monadic Composition for Data Validation
+
+### Scenario: User Registration Validation
+
+```python
+from dataclasses import dataclass
+from returns.result import Result, Success, Failure
+from returns.pipeline import flow
+from returns.pointfree import bind
+from typing import Optional
+
+@dataclass(frozen=True)
+class Email:
+    value: str
+
+@dataclass(frozen=True)
+class Password:
+    value: str
+
+@dataclass(frozen=True)
+class User:
+    email: Email
+    password: Password
+
+def validate_email(raw: str) -> Result[Email, str]:
+    """Validate email format"""
+    if "@" not in raw:
+        return Failure("Invalid email format")
+    if len(raw) > 255:
+        return Failure("Email too long")
+    return Success(Email(raw))
+
+def validate_password(raw: str) -> Result[Password, str]:
+    """Validate password strength"""
+    if len(raw) < 8:
+        return Failure("Password must be at least 8 characters")
+    if not any(c.isupper() for c in raw):
+        return Failure("Password must contain uppercase letter")
+    return Success(Password(raw))
+
+def check_email_not_taken(email: Email) -> Result[Email, str]:
+    """Simulate database check (would be IO in real app)"""
+    taken_emails = ["test@example.com", "admin@example.com"]
+    if email.value in taken_emails:
+        return Failure("Email already registered")
+    return Success(email)
+
+def create_user(email: Email, password: Password) -> Result[User, str]:
+    """Create user object"""
+    return Success(User(email, password))
+
+def register_user(
+    raw_email: str,
+    raw_password: str
+) -> Result[User, str]:
+    """
+    Railway-oriented validation pipeline
+    Each step returns Result - errors short-circuit
+    """
+    return flow(
+        raw_email,
+        validate_email,
+        bind(check_email_not_taken),
+        bind(lambda email: validate_password(raw_password).bind(
+            lambda password: create_user(email, password)
+        ))
+    )
+
+# Usage
+user1 = register_user("new@example.com", "SecurePass123")
+# Success(User(email=Email('new@example.com'), password=Password('SecurePass123')))
+
+user2 = register_user("invalid", "pass")
+# Failure("Invalid email format")
+
+user3 = register_user("test@example.com", "SecurePass123")
+# Failure("Email already registered")
+
+user4 = register_user("new@example.com", "weak")
+# Failure("Password must be at least 8 characters")
+```
+
+---
+
+## Example 3: Currying and Partial Application
+
+### Scenario: Configuration Management
+
+```python
+from returns.curry import curry
+from dataclasses import dataclass
+from typing import Callable
+
+@dataclass(frozen=True)
+class DatabaseConfig:
+    host: str
+    port: int
+    username: str
+    password: str
+    pool_size: int
+
+@curry
+def create_db_connection(
+    host: str,
+    port: int,
+    username: str,
+    password: str,
+    pool_size: int
+) -> 'DatabaseConnection':
+    """Curried function for creating database connections"""
+    return DatabaseConnection(
+        host=host,
+        port=port,
+        username=username,
+        password=password,
+        pool_size=pool_size
+    )
+
+# Partial application for different environments
+create_prod_connection = create_db_connection(
+    host="prod-db.example.com",
+    port=5432,
+    username="prod_user",
+    password="prod_pass",
+    pool_size=20
+)
+
+create_staging_connection = create_db_connection(
+    host="staging-db.example.com",
+    port=5432,
+    username="staging_user",
+    password="staging_pass",
+    pool_size=5
+)
+
+# Or create specialized functions
+connect_to_localhost = create_db_connection(
+    host="localhost",
+    port=5432
+)
+
+local_dev_connection = connect_to_localhost(
+    username="dev",
+    password="dev_pass",
+    pool_size=2
+)
+
+# Usage in higher-order functions
+def with_connection(
+    config: DatabaseConfig,
+    operation: Callable[['DatabaseConnection'], Result[T, E]]
+) -> Result[T, E]:
+    """Execute operation with a connection"""
+    conn = create_db_connection(
+        config.host,
+        config.port,
+        config.username,
+        config.password,
+        config.pool_size
+    )
+    return operation(conn)
+
+# Reusable connection patterns
+def query_users(conn: DatabaseConnection) -> Result[List[User], DbError]:
+    # Execute query...
+    pass
+
+def query_orders(conn: DatabaseConnection) -> Result[List[Order], DbError]:
+    # Execute query...
+    pass
+
+# Use same connection pattern for different queries
+users_result = with_connection(prod_config, query_users)
+orders_result = with_connection(prod_config, query_orders)
+```
+
+---
+
+## Example 4: Function Composition for Data Processing
+
+### Scenario: ML Data Preprocessing Pipeline
+
+```python
+from toolz import compose, pipe
+from typing import List, Dict, Any
+from returns.result import Result, Success, Failure
+
+# Pure transformation functions
+def load_raw_data(filepath: str) -> Result[List[Dict[str, Any]], str]:
+    """Load data from JSON file"""
+    try:
+        import json
+        with open(filepath, 'r') as f:
+            data = json.load(f)
+        return Success(data)
+    except Exception as e:
+        return Failure(f"Failed to load data: {e}")
+
+def validate_schema(data: List[Dict[str, Any]]) -> Result[List[Dict[str, Any]], str]:
+    """Validate required fields"""
+    required_fields = ['id', 'features', 'label']
+    for item in data:
+        if not all(field in item for field in required_fields):
+            return Failure(f"Missing required fields in item: {item}")
+    return Success(data)
+
+def normalize_features(data: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
+    """Normalize feature values to [0, 1] range"""
+    import numpy as np
+    
+    # Extract all features
+    all_features = [item['features'] for item in data]
+    features_array = np.array(all_features)
+    
+    # Calculate min/max
+    min_vals = features_array.min(axis=0)
+    max_vals = features_array.max(axis=0)
+    
+    # Normalize
+    normalized = (features_array - min_vals) / (max_vals - min_vals)
+    
+    # Return new data with normalized features
+    return [
+        {**item, 'features': normalized[i].tolist()}
+        for i, item in enumerate(data)
+    ]
+
+def augment_data(data: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
+    """Data augmentation: add noise to features"""
+    import random
+    
+    augmented = []
+    for item in data:
+        # Original item
+        augmented.append(item)
+        
+        # Augmented version (add small noise)
+        noise = [random.gauss(0, 0.01) for _ in item['features']]
+        noisy_features = [
+++            f + n for f, n in zip(item['features'], noise)
+++        ]
+++        augmented.append({
+++            **item,
+++            'id': f"{item['id']}_aug",
+++            'features': noisy_features,
+++            'augmented': True
+++        })
+++    
+++    return augmented
+
+def convert_to_tensors(data: List[Dict[str, Any]]) -> Result[Dict[str, Any], str]:
+    """Convert to PyTorch tensors"""
+    try:
+        import torch
+        
+        features = torch.tensor([item['features'] for item in data])
+        labels = torch.tensor([item['label'] for item in data])
+        
+        return Success({
+            'features': features,
+            'labels': labels,
+            'count': len(data)
+        })
+    except Exception as e:
+        return Failure(f"Failed to convert to tensors: {e}")
+
+# Compose into pipeline (right-to-left composition)
+preprocess_pipeline = compose(
+    convert_to_tensors,
+    augment_data,
+    normalize_features,
+    lambda r: r.bind(validate_schema),  # Wrap in lambda for Result handling
+    load_raw_data
+)
+
+# Or using pipe (left-to-right, more readable)
+def preprocess_with_pipe(filepath: str) -> Result[Dict[str, Any], str]:
+    return pipe(
+        filepath,
+        load_raw_data,
+        lambda r: r.bind(validate_schema),
+        normalize_features,
+        augment_data,
+        convert_to_tensors
+    )
+
+# Usage
+result = preprocess_pipeline("./data/train.json")
+# Success({'features': tensor(...), 'labels': tensor(...), 'count': 1000})
+
+# Railway-oriented error handling
+def safe_preprocess(filepath: str) -> Result[Dict[str, Any], str]:
+    return flow(
+        filepath,
+        load_raw_data,
+        bind(validate_schema),
+        lambda data: Success(normalize_features(data)),
+        lambda data: Success(augment_data(data)),
+        bind(convert_to_tensors)
+    )
+
+# All errors handled uniformly
+bad_result = safe_preprocess("./data/missing.json")
+# Failure("Failed to load data: ...")
+
+invalid_result = safe_preprocess("./data/invalid.json")
+# Failure("Missing required fields in item: ...")
+```
+
+---
+
+## Example 5: Immutable State Management
+
+### Scenario: Game State Management
+
+```python
+from dataclasses import dataclass
+from typing import Dict, List, Tuple
+from returns.result import Result, Success, Failure
+
+@dataclass(frozen=True)
+class Position:
+    x: int
+    y: int
+    
+    def move(self, dx: int, dy: int) -> 'Position':
+        """Return new position (immutable update)"""
+        return Position(self.x + dx, self.y + dy)
+
+@dataclass(frozen=True)
+class Player:
+    id: str
+    name: str
+    position: Position
+    health: int
+    
+    def move_to(self, new_position: Position) -> 'Player':
+        """Return new player with updated position"""
+        return Player(
+            id=self.id,
+            name=self.name,
+            position=new_position,
+            health=self.health
+        )
+    
+    def take_damage(self, amount: int) -> Result['Player', str]:
+        """Return new player with reduced health"""
+        new_health = self.health - amount
+        if new_health <= 0:
+            return Failure("Player defeated")
+        return Success(Player(
+            id=self.id,
+            name=self.name,
+            position=self.position,
+            health=new_health
+        ))
+    
+    def heal(self, amount: int) -> 'Player':
+        """Return new player with increased health"""
+        return Player(
+            id=self.id,
+            name=self.name,
+            position=self.position,
+            health=min(self.health + amount, 100)
+        )
+
+@dataclass(frozen=True)
+class GameState:
+    """Immutable game state"""
+    players: Dict[str, Player]
+    turn: int
+    winner: Optional[str]
+    
+    def update_player(self, player_id: str, new_player: Player) -> 'GameState':
+        """Return new game state with updated player"""
+        new_players = {**self.players, player_id: new_player}
+        return GameState(
+            players=new_players,
+            turn=self.turn,
+            winner=self.winner
+        )
+    
+    def next_turn(self) -> 'GameState':
+        """Return new state with incremented turn"""
+        return GameState(
+            players=self.players,
+            turn=self.turn + 1,
+            winner=self.winner
+        )
+    
+    def set_winner(self, player_id: str) -> 'GameState':
+        """Return new state with winner set"""
+        return GameState(
+            players=self.players,
+            turn=self.turn,
+            winner=player_id
+        )
+
+def handle_player_move(
+    state: GameState,
+    player_id: str,
+    new_position: Position
+) -> Result[GameState, str]:
+    """Pure function: handle player movement"""
+    if state.winner is not None:
+        return Failure("Game already finished")
+    
+    player = state.players.get(player_id)
+    if player is None:
+        return Failure("Player not found")
+    
+    # Calculate new player state
+    moved_player = player.move_to(new_position)
+    
+    # Check for collisions, combat, etc.
+    # ... game logic here ...
+    
+    # Return new game state
+    new_state = state.update_player(player_id, moved_player)
+    return Success(new_state)
+
+def handle_combat(
+    state: GameState,
+    attacker_id: str,
+    defender_id: str,
+    damage: int
+) -> Result[GameState, str]:
+    """Pure function: resolve combat"""
+    if attacker_id not in state.players or defender_id not in state.players:
+        return Failure("Invalid player ID")
+    
+    attacker = state.players[attacker_id]
+    defender = state.players[defender_id]
+    
+    # Apply damage to defender
+    damage_result = defender.take_damage(damage)
+    
+    match damage_result:
+        case Success(updated_defender):
+            # Defender survived
+            new_state = state.update_player(defender_id, updated_defender)
+            return Success(new_state)
+        case Failure("Player defeated"):
+            # Defender defeated - remove from game
+            new_players = {
+                pid: player 
+                for pid, player in state.players.items() 
+                if pid != defender_id
+            }
+            new_state = GameState(
+                players=new_players,
+                turn=state.turn,
+                winner=attacker_id if len(new_players) == 1 else None
+            )
+            return Success(new_state)
+        case Failure(error):
+            return Failure(error)
+
+# Game loop with immutable state
+def run_game_loop(initial_state: GameState) -> GameState:
+    """Pure: Run game until completion"""
+    def game_step(state: GameState) -> GameState:
+        if state.winner is not None:
+            return state  # Game over
+        
+        # Process turn for current player
+        # ... game logic ...
+        
+        return state.next_turn()
+    
+    # Fold over turns
+    from functools import reduce
+    return reduce(
+        lambda state, _: game_step(state),
+        range(1000),  # Max turns
+        initial_state
+    )
+
+# Usage
+initial_state = GameState(
+    players={
+        "p1": Player("p1", "Alice", Position(0, 0), 100),
+        "p2": Player("p2", "Bob", Position(10, 10), 100)
+    },
+    turn=0,
+    winner=None
+)
+
+final_state = run_game_loop(initial_state)
+
+if final_state.winner:
+    print(f"Winner: {final_state.winner}")
+else:
+    print("Game ended without winner")
+
+# Key benefits of immutable state:
+# 1. Easy to test (pure functions)
+# 2. Time-travel debugging (keep old states)
+# 3. Concurrent access safe
+# 4. Undo/redo functionality
+# 5. Predictable behavior
+```
+
+---
+
+## Example 6: Higher-Order Functions and Generic Patterns
+
+### Scenario: Validation Framework
+
+```python
+from typing import TypeVar, Callable, List, Generic, Protocol
+from returns.result import Result, Success, Failure
+
+A = TypeVar('A')
+B = TypeVar('B')
+E = TypeVar('E')
+
+class Validator(Protocol):
+    def validate(self, value: A) -> Result[A, str]:
+        ...
+
+class NonEmptyValidator:
+    def validate(self, value: List[A]) -> Result[List[A], str]:
+        if not value:
+            return Failure("List cannot be empty")
+        return Success(value)
+
+class LengthValidator:
+    def __init__(self, min_length: int, max_length: int):
+        self.min_length = min_length
+        self.max_length = max_length
+    
+    def validate(self, value: List[A]) -> Result[List[A], str]:
+        if len(value) < self.min_length:
+            return Failure(f"List too short: {len(value)} < {self.min_length}")
+        if len(value) > self.max_length:
+            return Failure(f"List too long: {len(value)} > {self.max_length}")
+        return Success(value)
+
+def validate_all(
+    value: A,
+    validators: List[Callable[[A], Result[A, str]]]
+) -> Result[A, str]:
+    """Apply multiple validators sequentially"""
+    result: Result[A, str] = Success(value)
+    for validator in validators:
+        result = result.bind(validator)
+    return result
+
+def validate_any(
+    value: A,
+    validators: List[Callable[[A], Result[A, str]]]
+) -> Result[A, str]:
+    """Pass if any validator succeeds"""
+    errors = []
+    for validator in validators:
+        match validator(value):
+            case Success(v):
+                return Success(v)  # Return first success
+            case Failure(e):
+                errors.append(e)
+    
+    return Failure(f"All validators failed: {errors}")
+
+# Usage
+def is_email(value: str) -> Result[str, str]:
+    if "@" in value:
+        return Success(value)
+    return Failure("Invalid email format")
+
+def is_phone(value: str) -> Result[str, str]:
+    if value.isdigit() and len(value) == 10:
+        return Success(value)
+    return Failure("Invalid phone format")
+
+# Validate that contact is either email or phone
+contact_validator = [is_email, is_phone]
+
+result1 = validate_any("test@example.com", contact_validator)
+# Success("test@example.com")
+
+result2 = validate_any("1234567890", contact_validator)
+# Success("1234567890")
+
+result3 = validate_any("invalid", contact_validator)
+# Failure("All validators failed: ['Invalid email format', 'Invalid phone format']")
+
+# Generic traverse function
+def traverse_result(
+    items: List[A],
+    func: Callable[[A], Result[B, E]]
+) -> Result[List[B], E]:
+    """
+    Apply function to each item, collecting results.
+    Short-circuits on first error.
+    """
+    result: List[B] = []
+    for item in items:
+        item_result = func(item)
+        if isinstance(item_result, Failure):
+            return item_result
+        result.append(item_result.unwrap())
+    return Success(result)
+
+# Usage: Validate list of emails
+emails = ["test@example.com", "invalid", "user@domain.org"]
+validated = traverse_result(emails, is_email)
+# Failure("Invalid email format")
+
+emails = ["test@example.com", "user@domain.org"]
+validated = traverse_result(emails, is_email)
+# Success(["test@example.com", "user@domain.org"])
+```
+
+---
+
+## Example 7: Real-World ML Training Pipeline
+
+```python
+"""
+Complete example: Functional ML training pipeline
+Demonstrates immutability, error handling, composition, and state management
+"""
+
+from dataclasses import dataclass
+from returns.result import Result, Success, Failure
+from returns.pipeline import flow
+from returns.pointfree import bind
+from returns.curry import curry
+from typing import List, Dict, Any, Tuple
+import json
+
+# Domain Models (Immutable)
+@dataclass(frozen=True)
+class ModelConfig:
+    learning_rate: float
+    batch_size: int
+    epochs: int
+    dropout_rate: float
+
+@dataclass(frozen=True)
+class TrainingData:
+    features: List[List[float]]
+    labels: List[int]
+    feature_names: List[str]
+
+@dataclass(frozen=True)
+class ModelState:
+    """Immutable model state"""
+    weights: Dict[str, List[float]]
+    epoch: int
+    loss: float
+    accuracy: float
+    
+    def update(
+        self,
+        new_weights: Dict[str, List[float]],
+        loss: float,
+        accuracy: float
+    ) -> 'ModelState':
+        """Return new state (immutable update)"""
+        return ModelState(
+            weights=new_weights,
+            epoch=self.epoch + 1,
+            loss=loss,
+            accuracy=accuracy
+        )
+
+@dataclass(frozen=True)
+class TrainingMetrics:
+    final_loss: float
+    final_accuracy: float
+    epochs_trained: int
+    best_epoch: int
+
+# Pure Functions
+def load_config(path: str) -> Result[ModelConfig, str]:
+    """Pure: Load configuration from file"""
+    try:
+        with open(path, 'r') as f:
+            data = json.load(f)
+        return Success(ModelConfig(**data))
+    except Exception as e:
+        return Failure(f"Failed to load config: {e}")
+
+def load_data(path: str) -> Result[TrainingData, str]:
+    """Pure: Load training data"""
+    try:
+        with open(path, 'r') as f:
+            data = json.load(f)
+        return Success(TrainingData(**data))
+    except Exception as e:
+        return Failure(f"Failed to load data: {e}")
+
+def validate_data(data: TrainingData) -> Result[TrainingData, str]:
+    """Pure: Validate data quality"""
+    if not data.features:
+        return Failure("No features found")
+    if not data.labels:
+        return Failure("No labels found")
+    if len(data.features) != len(data.labels):
+        return Failure("Features and labels count mismatch")
+    return Success(data)
+
+def initialize_model(config: ModelConfig) -> Result[ModelState, str]:
+    """Pure: Initialize model weights"""
+    try:
+        # Simulate weight initialization
+        weights = {
+            "layer1": [0.1] * 10,
+            "layer2": [0.2] * 5,
+            "output": [0.3] * 2
+        }
+        return Success(ModelState(
+            weights=weights,
+            epoch=0,
+            loss=0.0,
+            accuracy=0.0
+        ))
+    except Exception as e:
+        return Failure(f"Failed to initialize model: {e}")
+
+@curry
+def train_epoch(
+    config: ModelConfig,
+    state: ModelState,
+    data: TrainingData
+) -> Result[ModelState, str]:
+    """Pure: Train for one epoch"""
+    try:
+        # Simulate training step
+        import random
+        loss = random.uniform(0.1, 0.5) * (0.95 ** state.epoch)
+        accuracy = min(0.95, 0.5 + 0.01 * state.epoch)
+        
+        # Simulate weight updates
+        new_weights = {
+            k: [w * (1 - config.learning_rate) for w in v]
+            for k, v in state.weights.items()
+        }
+        
+        return Success(state.update(new_weights, loss, accuracy))
+    except Exception as e:
+        return Failure(f"Training epoch failed: {e}")
+
+def evaluate_model(
+    state: ModelState,
+    test_data: TrainingData
+) -> Result[float, str]:
+    """Pure: Evaluate model on test data"""
+    try:
+        # Simulate evaluation
+        test_accuracy = min(0.98, state.accuracy + 0.02)
+        return Success(test_accuracy)
+    except Exception as e:
+        return Failure(f"Evaluation failed: {e}")
+
+def save_model(state: ModelState, path: str) -> Result[None, str]:
+    """Pure: Save model to disk (returns Result, but IO happens at boundary)"""
+    try:
+        # In real app, this would be IOResult
+        with open(path, 'w') as f:
+            json.dump({
+                "weights": state.weights,
+                "epoch": state.epoch,
+                "loss": state.loss,
+                "accuracy": state.accuracy
+            }, f)
+        return Success(None)
+    except Exception as e:
+        return Failure(f"Failed to save model: {e}")
+
+# Railway-Oriented Training Pipeline
+def run_training_pipeline(
+    config_path: str,
+    train_data_path: str,
+    test_data_path: str,
+    output_path: str
+) -> Result[TrainingMetrics, str]:
+    """
+    Complete training pipeline as pure function composition
+    Each step returns Result - errors short-circuit the pipeline
+    """
+    return flow(
+        config_path,
+        load_config,
+        bind(lambda config: flow(
+            train_data_path,
+            load_data,
+            bind(validate_data),
+            bind(lambda train_data: flow(
+                initialize_model(config),
+                bind(lambda model: train_multiple_epochs(config, model, train_data)),
+                bind(lambda trained_model: flow(
+                    test_data_path,
+                    load_data,
+                    bind(lambda test_data: evaluate_model(trained_model, test_data)),
+                    bind(lambda test_acc: save_and_return_metrics(
+                        trained_model, test_acc, output_path
+                    ))
+                ))
+            ))
+        ))
+    )
+
+def train_multiple_epochs(
+    config: ModelConfig,
+    initial_state: ModelState,
+    data: TrainingData
+) -> Result[ModelState, str]:
+    """Train for multiple epochs using fold"""
+    from functools import reduce
+    
+    def epoch_step(
+        current_state: Result[ModelState, str],
+        epoch_num: int
+    ) -> Result[ModelState, str]:
+        return current_state.bind(lambda state: train_epoch(config, state, data))
+    
+    return reduce(
+        epoch_step,
+        range(config.epochs),
+        Success(initial_state)
+    )
+
+def save_and_return_metrics(
+    state: ModelState,
+    test_accuracy: float,
+    output_path: str
+) -> Result[TrainingMetrics, str]:
+    """Save model and return metrics"""
+    return flow(
+        save_model(state, output_path),
+        lambda _: Success(TrainingMetrics(
+            final_loss=state.loss,
+            final_accuracy=test_accuracy,
+            epochs_trained=state.epoch,
+            best_epoch=state.epoch
+        ))
+    )
+
+# Usage
+result = run_training_pipeline(
+    config_path="./config.json",
+    train_data_path="./train_data.json",
+    test_data_path="./test_data.json",
+    output_path="./model.json"
+)
+
+match result:
+    case Success(metrics):
+        print(f"Training complete!")
+        print(f"Final accuracy: {metrics.final_accuracy:.2%}")
+        print(f"Epochs: {metrics.epochs_trained}")
+    case Failure(error):
+        print(f"Training failed: {error}")
+
+# Key Benefits of This Approach:
+# 1. Pure functions - easy to test individually
+# 2. Railway-oriented - errors handled uniformly
+# 3. Immutable state - predictable, thread-safe
+# 4. Composable - easy to modify pipeline
+# 5. Type-safe - errors caught at compile time
+# 6. Maintainable - each function has single responsibility
+```
+
+---
+
+**Last Updated**: 2025-11-19  
+**Maintained By**: Global AI Rules System  
+**Status**: Active  
+**Next**: See `patterns.md` for advanced patterns and `libraries.md` for library-specific guidance