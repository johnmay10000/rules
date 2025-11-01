// src/types.rs - Type definitions and error types

use thiserror::Error;

/// Application error type (ADT using enum)
#[derive(Error, Debug, Clone)]
pub enum AppError {
    #[error("Invalid input: {0}")]
    InvalidInput(String),
    
    #[error("Validation error: {0}")]
    ValidationError(String),
    
    #[error("I/O error: {0}")]
    IoError(String),
    
    #[error("Parse error: {0}")]
    ParseError(String),
}

/// Domain type example: User
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct User {
    pub id: u64,
    pub name: String,
    pub email: String,
    pub age: u32,
}

/// Domain type example: ProcessResult
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct ProcessResult {
    pub status: String,
    pub data: Vec<i32>,
    pub timestamp: String,
}

/// Result type alias for convenience
pub type AppResult<T> = Result<T, AppError>;

