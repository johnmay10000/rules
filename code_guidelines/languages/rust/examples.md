---
title: Rust Functional Programming Examples
language: rust
category: code_guidelines
type: examples
applies_to: [cursor, kimi, claude, gemini]
version: 1.0.0
last_updated: 2025-11-19
---

# Rust Functional Programming Examples

This document provides practical examples of functional programming patterns in Rust.

## 1. Functional Web Service

A complete example of a railway-oriented service with immutable domain models and pure functions.

```rust
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::{Arc, Mutex};

// Domain types (immutable)
#[derive(Clone, Debug, Serialize, Deserialize)]
struct User {
    id: String,
    email: String,
    name: String,
}

// Error types
#[derive(Debug)]
enum UserError {
    NotFound(String),
    AlreadyExists(String),
    InvalidEmail(String),
}

// Repository trait (pure interface)
trait UserRepository {
    fn find(&self, id: &str) -> Result<Option<User>, UserError>;
    fn save(&self, user: User) -> Result<User, UserError>;
    fn exists(&self, email: &str) -> Result<bool, UserError>;
}

// In-memory implementation
struct InMemoryRepository {
    users: Arc<Mutex<HashMap<String, User>>>,
}

impl UserRepository for InMemoryRepository {
    fn find(&self, id: &str) -> Result<Option<User>, UserError> {
        let users = self.users.lock().unwrap();
        Ok(users.get(id).cloned())
    }
    
    fn save(&self, user: User) -> Result<User, UserError> {
        let mut users = self.users.lock().unwrap();
        users.insert(user.id.clone(), user.clone());
        Ok(user)
    }
    
    fn exists(&self, email: &str) -> Result<bool, UserError> {
        let users = self.users.lock().unwrap();
        Ok(users.values().any(|u| u.email == email))
    }
}

// Pure validation functions
fn validate_email(email: &str) -> Result<String, UserError> {
    if email.contains('@') && email.len() > 3 {
        Ok(email.to_string())
    } else {
        Err(UserError::InvalidEmail("Invalid email format".to_string()))
    }
}

fn validate_user_data(email: &str, name: &str) -> Result<(String, String), UserError> {
    let valid_email = validate_email(email)?;
    if name.len() < 2 {
        return Err(UserError::InvalidEmail("Name too short".to_string()));
    }
    Ok((valid_email, name.to_string()))
}

// Railway-oriented service logic
fn create_user(
    repo: &dyn UserRepository,
    email: &str,
    name: &str,
) -> Result<User, UserError> {
    // Validate input
    let (valid_email, valid_name) = validate_user_data(email, name)?;
    
    // Check if email exists
    if repo.exists(&valid_email)? {
        return Err(UserError::AlreadyExists(valid_email));
    }
    
    // Create and save user
    let user = User {
        id: generate_id(),
        email: valid_email,
        name: valid_name,
    };
    
    repo.save(user)
}

// Usage
fn main() {
    let repo = InMemoryRepository {
        users: Arc::new(Mutex::new(HashMap::new())),
    };
    
    // Create a user (railway-oriented)
    match create_user(&repo, "alice@example.com", "Alice") {
        Ok(user) => println!("Created: {:?}", user),
        Err(e) => println!("Error: {:?}", e),
    }
    
    // Try to create duplicate (will fail)
    match create_user(&repo, "alice@example.com", "Alice") {
        Ok(user) => println!("Created: {:?}", user),
        Err(UserError::AlreadyExists(email)) => {
            println!("User already exists: {}", email)
        }
        Err(e) => println!("Error: {:?}", e),
    }
}

fn generate_id() -> String {
    use uuid::Uuid;
    Uuid::new_v4().to_string()
}
```

## 2. Async Functional Pipeline

Composing async operations with Result types.

```rust
use futures::{future, Future, TryFutureExt};
use std::io;

// Async function returning Result
async fn fetch_data(url: &str) -> Result<String, io::Error> {
    // Implementation
    Ok("data".to_string())
}

// Compose async operations
async fn process_multiple(urls: &[&str]) -> Result<Vec<String>, io::Error> {
    // Map each URL to a future
    let futures: Vec<_> = urls.iter()
        .map(|url| fetch_data(url))
        .collect();
    
    // Wait for all to complete (fail fast)
    let results = future::try_join_all(futures).await?;
    Ok(results)
}

// Railway-oriented async
async fn load_and_process(url: &str) -> Result<String, io::Error> {
    let data = fetch_data(url).await?;  // ? propagates errors
    Ok(process_data(&data))
}

fn process_data(data: &str) -> String {
    data.to_uppercase()
}
```
