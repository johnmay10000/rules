// src/main.rs - Example Rust FP Project Entry Point

use std::error::Error;

mod types;
use types::*;

/// Main entry point demonstrating FP patterns
#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
    println!("🦀 Rust FP Example - High-Performance Data Processing");
    println!("================================================\n");

    // Example 1: Result type with ? operator (railway-oriented)
    println!("Example 1: Railway-Oriented Programming");
    let result = process_data_pipeline("sample input").await;
    match result {
        Ok(output) => println!("✅ Success: {}", output),
        Err(e) => println!("❌ Error: {}", e),
    }
    println!();

    // Example 2: Foldable (reduce/fold)
    println!("Example 2: Foldable Pattern");
    let numbers = vec![1, 2, 3, 4, 5];
    let sum: i32 = numbers.iter().sum();
    println!("Sum: {}", sum);
    
    let product = numbers.iter().fold(1, |acc, &x| acc * x);
    println!("Product: {}", product);
    println!();

    // Example 3: Traversable (collect with Result)
    println!("Example 3: Traversable Pattern");
    let validated = validate_all(&[1, 2, 3, 4, 5]);
    println!("Validation: {:?}", validated);
    
    let invalid = validate_all(&[1, -2, 3]);
    println!("Invalid: {:?}", invalid);
    println!();

    // Example 4: Parallel operations (rayon)
    println!("Example 4: Parallel Processing");
    let large_vec: Vec<i32> = (1..=100).collect();
    let parallel_result = parallel_compute(&large_vec);
    println!("Parallel sum: {:?}", parallel_result);
    println!();

    // Example 5: Async operations (tokio)
    println!("Example 5: Async Operations");
    let async_result = fetch_multiple_async().await;
    println!("Async results: {:?}", async_result);
    println!();

    println!("🎉 All examples complete!");
    
    Ok(())
}

/// Example: Railway-oriented programming with ? operator
async fn process_data_pipeline(input: &str) -> Result<String, AppError> {
    let parsed = parse_input(input)?;
    let validated = validate_data(&parsed)?;
    let transformed = transform_data(&validated)?;
    let result = save_result(&transformed).await?;
    Ok(result)
}

fn parse_input(input: &str) -> Result<String, AppError> {
    if input.is_empty() {
        Err(AppError::InvalidInput("Empty input".to_string()))
    } else {
        Ok(input.to_uppercase())
    }
}

fn validate_data(data: &str) -> Result<String, AppError> {
    if data.len() < 5 {
        Err(AppError::ValidationError("Too short".to_string()))
    } else {
        Ok(data.to_string())
    }
}

fn transform_data(data: &str) -> Result<String, AppError> {
    Ok(format!("Transformed: {}", data))
}

async fn save_result(data: &str) -> Result<String, AppError> {
    // Simulate async I/O
    tokio::time::sleep(tokio::time::Duration::from_millis(10)).await;
    Ok(format!("Saved: {}", data))
}

/// Example: Traversable with collect() and Result
fn validate_all(numbers: &[i32]) -> Result<Vec<i32>, AppError> {
    numbers
        .iter()
        .map(|&n| validate_positive(n))
        .collect()  // Stops at first Err! Native Traversable!
}

fn validate_positive(n: i32) -> Result<i32, AppError> {
    if n > 0 {
        Ok(n)
    } else {
        Err(AppError::ValidationError(format!("{} is not positive", n)))
    }
}

/// Example: Parallel processing with rayon
fn parallel_compute(numbers: &[i32]) -> Result<i32, AppError> {
    use rayon::prelude::*;
    
    let sum: i32 = numbers
        .par_iter()
        .map(|&x| x * 2)
        .sum();
    
    Ok(sum)
}

/// Example: Async parallel operations with tokio
async fn fetch_multiple_async() -> Result<Vec<String>, AppError> {
    use futures::future::try_join_all;
    
    let ids = vec![1, 2, 3, 4, 5];
    
    let futures: Vec<_> = ids
        .iter()
        .map(|&id| fetch_data(id))
        .collect();
    
    // All run concurrently, stops at first error
    try_join_all(futures).await
}

async fn fetch_data(id: i32) -> Result<String, AppError> {
    // Simulate async HTTP request
    tokio::time::sleep(tokio::time::Duration::from_millis(50)).await;
    Ok(format!("Data {}", id))
}

