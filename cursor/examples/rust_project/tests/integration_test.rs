// tests/integration_test.rs - Integration tests demonstrating FP patterns

use rust_project::*;

#[tokio::test]
async fn test_railway_oriented_success() {
    // Valid input should succeed through entire pipeline
    let input = "valid input data";
    let result = process_data_pipeline(input).await;
    
    assert!(result.is_ok());
    let output = result.unwrap();
    assert!(output.contains("VALID INPUT DATA"));
}

#[tokio::test]
async fn test_railway_oriented_failure_empty() {
    // Empty input should fail at parse stage
    let input = "";
    let result = process_data_pipeline(input).await;
    
    assert!(result.is_err());
    match result {
        Err(AppError::InvalidInput(_)) => {}, // Expected
        _ => panic!("Expected InvalidInput error"),
    }
}

#[tokio::test]
async fn test_railway_oriented_failure_short() {
    // Short input should fail at validation stage
    let input = "hi";
    let result = process_data_pipeline(input).await;
    
    assert!(result.is_err());
    match result {
        Err(AppError::ValidationError(_)) => {}, // Expected
        _ => panic!("Expected ValidationError"),
    }
}

#[test]
fn test_traversable_all_valid() {
    // Traversable: All valid should succeed
    let numbers = vec![1, 2, 3, 4, 5];
    let result = validate_all(&numbers);
    
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), numbers);
}

#[test]
fn test_traversable_early_exit() {
    // Traversable: Should stop at first invalid (early exit)
    let numbers = vec![1, 2, -3, 4, 5];
    let result = validate_all(&numbers);
    
    assert!(result.is_err());
    match result {
        Err(AppError::ValidationError(msg)) => {
            assert!(msg.contains("-3"));
        },
        _ => panic!("Expected ValidationError"),
    }
}

#[test]
fn test_foldable_sum() {
    // Foldable: Sum numbers
    let numbers = vec![1, 2, 3, 4, 5];
    let sum: i32 = numbers.iter().sum();
    
    assert_eq!(sum, 15);
}

#[test]
fn test_foldable_product() {
    // Foldable: Product using fold
    let numbers = vec![1, 2, 3, 4, 5];
    let product = numbers.iter().fold(1, |acc, &x| acc * x);
    
    assert_eq!(product, 120);
}

#[test]
fn test_parallel_compute() {
    // Parallel: Compute with rayon
    let numbers: Vec<i32> = (1..=100).collect();
    let result = parallel_compute(&numbers);
    
    assert!(result.is_ok());
    // Sum of (2*1 + 2*2 + ... + 2*100) = 2 * (1 + 2 + ... + 100) = 2 * 5050 = 10100
    assert_eq!(result.unwrap(), 10100);
}

#[tokio::test]
async fn test_async_parallel_success() {
    // Async: All succeed
    let result = fetch_multiple_async().await;
    
    assert!(result.is_ok());
    let data = result.unwrap();
    assert_eq!(data.len(), 5);
    assert_eq!(data[0], "Data 1");
    assert_eq!(data[4], "Data 5");
}

// Property-based testing example (using quickcheck)
#[cfg(test)]
mod property_tests {
    use super::*;
    use quickcheck::quickcheck;
    
    quickcheck! {
        fn prop_positive_validation_idempotent(n: u32) -> bool {
            let positive = n as i32 + 1; // Ensure positive
            let first = validate_positive(positive);
            let second = validate_positive(positive);
            first.is_ok() && second.is_ok() && first == second
        }
        
        fn prop_fold_sum_equals_iter_sum(numbers: Vec<i32>) -> bool {
            let fold_sum = numbers.iter().fold(0, |acc, &x| acc + x);
            let iter_sum: i32 = numbers.iter().sum();
            fold_sum == iter_sum
        }
    }
}

// Helper function to expose private functions for testing
// In real project, these would be in lib.rs or public modules
async fn process_data_pipeline(input: &str) -> Result<String, AppError> {
    rust_project::process_data_pipeline(input).await
}

fn validate_all(numbers: &[i32]) -> Result<Vec<i32>, AppError> {
    rust_project::validate_all(numbers)
}

fn validate_positive(n: i32) -> Result<i32, AppError> {
    rust_project::validate_positive(n)
}

fn parallel_compute(numbers: &[i32]) -> Result<i32, AppError> {
    rust_project::parallel_compute(numbers)
}

async fn fetch_multiple_async() -> Result<Vec<String>, AppError> {
    rust_project::fetch_multiple_async().await
}

