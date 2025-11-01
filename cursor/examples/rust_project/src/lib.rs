// src/lib.rs - Library root (exports public API)

pub mod types;
pub use types::*;

use rayon::prelude::*;
use futures::future::try_join_all;

/// Railway-oriented programming with ? operator
pub async fn process_data_pipeline(input: &str) -> Result<String, AppError> {
    let parsed = parse_input(input)?;
    let validated = validate_data(&parsed)?;
    let transformed = transform_data(&validated)?;
    let result = save_result(&transformed).await?;
    Ok(result)
}

pub fn parse_input(input: &str) -> Result<String, AppError> {
    if input.is_empty() {
        Err(AppError::InvalidInput("Empty input".to_string()))
    } else {
        Ok(input.to_uppercase())
    }
}

pub fn validate_data(data: &str) -> Result<String, AppError> {
    if data.len() < 5 {
        Err(AppError::ValidationError("Too short".to_string()))
    } else {
        Ok(data.to_string())
    }
}

pub fn transform_data(data: &str) -> Result<String, AppError> {
    Ok(format!("Transformed: {}", data))
}

pub async fn save_result(data: &str) -> Result<String, AppError> {
    // Simulate async I/O
    tokio::time::sleep(tokio::time::Duration::from_millis(10)).await;
    Ok(format!("Saved: {}", data))
}

/// Traversable with collect() and Result
pub fn validate_all(numbers: &[i32]) -> Result<Vec<i32>, AppError> {
    numbers
        .iter()
        .map(|&n| validate_positive(n))
        .collect()  // Stops at first Err! Native Traversable!
}

pub fn validate_positive(n: i32) -> Result<i32, AppError> {
    if n > 0 {
        Ok(n)
    } else {
        Err(AppError::ValidationError(format!("{} is not positive", n)))
    }
}

/// Parallel processing with rayon
pub fn parallel_compute(numbers: &[i32]) -> Result<i32, AppError> {
    let sum: i32 = numbers
        .par_iter()
        .map(|&x| x * 2)
        .sum();
    
    Ok(sum)
}

/// Async parallel operations with tokio
pub async fn fetch_multiple_async() -> Result<Vec<String>, AppError> {
    let ids = vec![1, 2, 3, 4, 5];
    
    let futures: Vec<_> = ids
        .iter()
        .map(|&id| fetch_data(id))
        .collect();
    
    // All run concurrently, stops at first error
    try_join_all(futures).await
}

pub async fn fetch_data(id: i32) -> Result<String, AppError> {
    // Simulate async HTTP request
    tokio::time::sleep(tokio::time::Duration::from_millis(50)).await;
    Ok(format!("Data {}", id))
}

