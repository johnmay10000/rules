# Rust FP Example: CLI Tool with Railway-Oriented Programming

**Project Type**: Command-Line Application
**Language**: Rust 1.70+
** demonstrates**: Railway-oriented programming, Result types, error handling, monadic composition

---

## 📋 Project Overview

This example demonstrates functional programming patterns in Rust using its native `Result` type for a command-line tool that processes files. The tool follows railway-oriented programming principles with explicit error handling and composable operations.

### Key FP Concepts Demonstrated

- **Result<T, E>**: Rust's built-in railway type (Ok/Err)
- ** Railway composition**: Using `and_then`, `map`, `map_err` for chaining
- **Immutability**: All data structures are immutable by default
- **Type safety**: Strong compile-time guarantees
- **Pattern matching**: Exhaustive error handling
- **Traits for composition**: Custom traits for railway operations

---

## 🏗️ Project Structure

```
rust_project/
├── Cargo.toml                          # Rust project configuration
├── README.md                           # This file
├── ARCHITECTURE_PLAN.md               # Tier 1: Strategic overview
├── docs/
│   ├── plans/
│   │   ├── CLI_DESIGN.md              # Tier 2: Technical design
│   │   └── IMPLEMENTATION_TODO.md     # Tier 2: Task tracking
│   └── 2025_11_14/                    # Tier 3: Execution logs
│       ├── 20251114_0001_PROJECT_SETUP.md
│       ├── 20251114_0002_CLI_IMPLEMENTATION.md
│       └── 20251114_0003_ERROR_HANDLING.md
└── src/
    ├── main.rs                         # CLI entry point
    ├── lib.rs                          # Library root
    ├── processor.rs                    # File processing with Result
    ├── validator.rs                    # Data validation
    ├── formatter.rs                    # Output formatting
    └── error.rs                        # Custom error types
```

---

## 🚀 Quick Start

### Installation

```bash
# Clone and build
cd rust_project
cargo build --release

# Run tests
cargo test --all

# Run the tool
./target/release/rust_cli --input data/input.txt --output results/
```

### Usage Examples

```bash
# Process a single file
rust_cli process file.txt --format json

# Validate and transform data
rust_cli transform input.csv --schema schemas/data.json

# Batch processing with parallel validation
rust_cli batch data/ --workers 4

# Generate report
rust_cli report --input results/ --output summary.html
```

---

## 💡 FP Patterns Demonstrated

### 1. Result Type (Railway Programming)

Rust's native Result type for error handling:

```rust
use std::fmt;
use std::fs::File;
use std::io::{self, Read};

// Custom error type
#[derive(Debug)]
pub enum ProcessingError {
    IoError(io::Error),
    ValidationError(String),
    FormatError(String),
}

impl fmt::Display for ProcessingError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ProcessingError::IoError(e) => write!(f, "IO error: {}", e),
            ProcessingError::ValidationError(msg) => write!(f, "Validation error: {}", msg),
            ProcessingError::FormatError(msg) => write!(f, "Format error: {}", msg),
        }
    }
}

// Railway composition with Result
pub fn process_file(path: &str) -> Result<String, ProcessingError> {
    read_file(path)
        .and_then(validate_content)
        .and_then(transform_content)
        .and_then(format_output)
}

fn read_file(path: &str) -> Result<String, ProcessingError> {
    let mut file = File::open(path).map_err(ProcessingError::IoError)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)
        .map_err(ProcessingError::IoError)?;
    Ok(contents)
}

fn validate_content(content: String) -> Result<String, ProcessingError> {
    if content.trim().is_empty() {
        return Err(ProcessingError::ValidationError(
            "Content is empty".to_string()
        ));
    }
    Ok(content)
}

fn transform_content(content: String) -> Result<String, ProcessingError> {
    // Transform content
    Ok(content.to_uppercase())
}

fn format_output(content: String) -> Result<String, ProcessingError> {
    if content.len() > 1000 {
        Ok(format!("Processed {} characters", content.len()))
    } else {
        Ok(content)
    }
}
```

### 2. Railway Composition with Custom Traits

Extending Result for more expressive composition:

```rust
// Custom trait for railway operations
trait Railway: Sized {
    fn tap<F>(self, f: F) -> Self
    where
        F: FnOnce(&Self);
}

impl<T, E> Railway for Result<T, E> {
    fn tap<F>(self, f: F) -> Self
    where
        F: FnOnce(&Self),
    {
        f(&self);
        self
    }
}

// Usage in pipeline
pub fn process_with_logging(path: &str) -> Result<String, ProcessingError> {
    read_file(path)
        .tap(|result| println!("File read: {:?}", result.is_ok()))
        .and_then(validate_content)
        .tap(|_| println!("Validation complete"))
        .and_then(transform_content)
        .tap(|_| println!("Transformation complete"))
        .and_then(format_output)
        .tap(|result| {
            if let Ok(ref output) = result {
                println!("Final output length: {}", output.len());
            }
        })
}
```

### 3. Monadic Error Accumulation

Collect multiple errors instead of failing fast:

```rust
use std::collections::VecDeque;

#[derive(Debug)]
pub struct ValidationErrors {
    errors: VecDeque<String>,
}

impl ValidationErrors {
    pub fn new() -> Self {
        Self {
            errors: VecDeque::new(),
        }
    }
    
    pub fn add(&mut self, error: String) {
        self.errors.push_back(error);
    }
    
    pub fn is_empty(&self) -> bool {
        self.errors.is_empty()
    }
    
    pub fn into_result<T>(self, value: T) -> Result<T, Self> {
        if self.is_empty() {
            Ok(value)
        } else {
            Err(self)
        }
    }
}

pub fn validate_all(content: &str) -> Result<(), ValidationErrors> {
    let mut errors = ValidationErrors::new();
    
    if content.is_empty() {
        errors.add("Content is empty".to_string());
    }
    
    if content.len() > 10000 {
        errors.add("Content too large".to_string());
    }
    
    if !content.is_ascii() {
        errors.add("Content must be ASCII".to_string());
    }
    
    errors.into_result(())
}
```

### 4. Immutable Data Structures

Using Rust's ownership and lifetimes:

```rust
#[derive(Debug, Clone)]  // Immutable by default
pub struct ProcessedData {
    original: String,
    transformed: String,
    metadata: Metadata,
}

#[derive(Debug, Clone)]
pub struct Metadata {
    line_count: usize,
    word_count: usize,
    timestamp: std::time::SystemTime,
}

impl ProcessedData {
    pub fn new(original: String, transformed: String) -> Result<Self, ProcessingError> {
        let metadata = Metadata::calculate(&original, &transformed)?;
        
        Ok(Self {
            original,
            transformed,
            metadata,
        })
    }
    
    pub fn add_annotation(&self, annotation: String) -> Self {
        // Returns new instance - original unchanged
        Self {
            original: self.original.clone(),
            transformed: format!("{}\n<!-- {} -->", self.transformed, annotation),
            metadata: Metadata::calculate(&self.original, &self.transformed)
                .unwrap_or_else(|_| self.metadata.clone()),
        }
    }
}
```

### 5. Higher-Order Functions and Composition

Combining operations functionally:

```rust
// Type aliases for clarity
type ValidationFn = fn(&str) -> Result<(), String>;
type TransformationFn = fn(String) -> Result<String, ProcessingError>;

pub struct Pipeline {
    validators: Vec<ValidationFn>,
    transformers: Vec<TransformationFn>,
}

impl Pipeline {
    pub fn new() -> Self {
        Self {
            validators: Vec::new(),
            transformers: Vec::new(),
        }
    }
    
    pub fn add_validator(mut self, validator: ValidationFn) -> Self {
        self.validators.push(validator);
        self
    }
    
    pub fn add_transformer(mut self, transformer: TransformationFn) -> Self {
        self.transformers.push(transformer);
        self
    }
    
    pub fn execute(&self, input: &str) -> Result<String, ProcessingError> {
        // Validate all
        for validator in &self.validators {
            if let Err(e) = validator(input) {
                return Err(ProcessingError::ValidationError(e));
            }
        }
        
        // Transform through pipeline
        let mut result = Ok(input.to_string());
        for transformer in &self.transformers {
            result = result.and_then(|data| transformer(data));
        }
        
        result
    }
}

// Usage
let pipeline = Pipeline::new()
    .add_validator(|s| if s.len() > 0 { Ok(()) } else { Err("Empty".to_string()) })
    .add_validator(|s| if s.is_ascii() { Ok(()) } else { Err("Non-ASCII".to_string()) })
    .add_transformer(|s| Ok(s.trim().to_string()))
    .add_transformer(|s| Ok(s.to_uppercase()));

let result = pipeline.execute("  hello world  ");
assert_eq!(result.unwrap(), "HELLO WORLD");
```

---

## 🎛️ CLI Design

### Command Structure

```rust
// CLI using clap with Result-based error handling
use clap::{Parser, Subcommand};

#[derive(Parser)]
#[command(name = "rust_cli")]
#[command(about = "FP CLI tool for file processing", long_about = None)]
pub struct Cli {
    #[command(subcommand)]
    command: Commands,
    
    /// Verbose output
    #[arg(short, long)]
    verbose: bool,
}

#[derive(Subcommand)]
pub enum Commands {
    /// Process a single file
    Process {
        /// Input file path
        input: String,
        
        /// Output format
        #[arg(short, long, default_value = "json")]
        format: String,
        
        /// Validate before processing
        #[arg(short, long)]
        validate: bool,
    },
    
    /// Transform data according to schema
    Transform {
        /// Input file path
        input: String,
        
        /// Schema file for validation
        #[arg(short, long)]
        schema: String,
        
        /// Output directory
        #[arg(short, long, default_value = "output/")]
        output: String,
    },
    
    /// Process multiple files in parallel
    Batch {
        /// Input directory
        input: String,
        
        /// Number of worker threads
        #[arg(short, long, default_value = "2")]
        workers: usize,
    },
}

pub fn run_cli(cli: Cli) -> Result<(), ProcessingError> {
    match cli.command {
        Commands::Process { input, format, validate } => {
            if validate {
                validate_content(std::fs::read_to_string(&input)
                    .map_err(ProcessingError::IoError)?)?;
            }
            let result = process_file(&input)?;
            println!("Processed result: {}", result);
            Ok(())
        }
        Commands::Transform { input, schema, output } => {
            let content = read_file(&input)?;
            let transformed = transform_content(content)?;
            std::fs::write(&output, transformed)
                .map_err(ProcessingError::IoError)?;
            Ok(())
        }
        Commands::Batch { input, workers } => {
            process_batch(&input, workers)
        }
    }
}
```

---

## 🧪 Testing Strategy

### Unit Tests with Result Types

```rust
#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_read_file_success() {
        let result = read_file("test_data/valid.txt");
        assert!(result.is_ok());
        assert!(!result.unwrap().is_empty());
    }
    
    #[test]
    fn test_read_file_not_found() {
        let result = read_file("nonexistent.txt");
        assert!(result.is_err());
        match result {
            Err(ProcessingError::IoError(_)) => (),
            _ => panic!("Expected IoError"),
        }
    }
    
    #[test]
    fn test_validate_content_empty() {
        let result = validate_content("".to_string());
        assert!(result.is_err());
        match result {
            Err(ProcessingError::ValidationError(msg)) => {
                assert_eq!(msg, "Content is empty")
            }
            _ => panic!("Expected ValidationError"),
        }
    }
    
    #[test]
    fn test_pipeline_composition() {
        let pipeline = Pipeline::new()
            .add_validator(|s| if !s.is_empty() { Ok(()) } else { Err("Empty".to_string()) })
            .add_transformer(|s| Ok(s.to_uppercase()));
        
        let result = pipeline.execute("hello");
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), "HELLO");
    }
    
    #[test]
    fn test_railway_trait_logging() {
        let result = read_file("test_data/valid.txt")
            .tap(|r| println!("File read result: {:?}", r.is_ok()))
            .and_then(validate_content);
        
        assert!(result.is_ok());
    }
}
```

---

## 📊 Pipeline Architecture

```
Command Input
    ↓
[Parse CLI Arguments] → Result<Cli, Error>
    ↓
[Read File] → Result<String, IoError>
    ↓
[Validate Content] → Result<String, ValidationError>
    ↓
[Transform Content] → Result<String, ProcessingError>
    ↓
[Format Output] → Result<String, FormatError>
    ↓
[Write/Print Result] → Result<(), IoError>
```

**Key Characteristics:**
- Every operation returns Result<T, E>
- Short-circuit on Err (railway pattern)
- Type-safe error handling at compile time
- Immutable data throughout pipeline
- Explicit error types with context

---

## 🖧 Parallel Processing Example

Using Rayon for parallel data processing:

```rust
use rayon::prelude::*;

pub fn process_batch_parallel(
    input_dir: &str,
    _workers: usize
) -> Result<Vec<(String, Result<String, ProcessingError>)>, ProcessingError> {
    let entries = std::fs::read_dir(input_dir)
        .map_err(ProcessingError::IoError)?;
    
    let results: Vec<_> = entries
        .par_bridge()  // Parallel iterator
        .filter_map(|entry| {
            let entry = entry.ok()?;
            let path = entry.path();
            if path.extension()?.to_str()? == "txt" {
                Some((
                    path.to_string_lossy().to_string(),
                    process_file(path.to_str()?)
                ))
            } else {
                None
            }
        })
        .collect();
    
    Ok(results)
}
```

---

## 🎯 Real-World Scenarios

### Scenario 1: Graceful Error Recovery
```rust
pub fn process_with_fallback(
    primary_path: &str,
    fallback_path: &str
) -> Result<String, ProcessingError> {
    process_file(primary_path)
        .or_else(|_| {
            eprintln!("Primary file failed, trying fallback");
            process_file(fallback_path)
        })
}
```

### Scenario 2: Partial Success Handling
```rust
pub fn batch_process_with_report(
    input_dir: &str
) -> Result<ProcessingReport, ProcessingError> {
    let files = std::fs::read_dir(input_dir)?;
    let mut successes = Vec::new();
    let mut failures = Vec::new();
    
    for file in files {
        if let Ok(entry) = file {
            if let Some(path) = entry.path().to_str() {
                match process_file(path) {
                    Ok(result) => successes.push((path.to_string(), result)),
                    Err(e) => failures.push((path.to_string(), e)),
                }
            }
        }
    }
    
    Ok(ProcessingReport {
        successes,
        failures,
    })
}
```

### Scenario 3: Configurable Validation
```rust
pub fn validate_with_rules(
    content: &str,
    rules: &[ValidationRule]
) -> Result<(), ValidationErrors> {
    let mut errors = ValidationErrors::new();
    
    for rule in rules {
        if let Err(e) = rule.validate(content) {
            errors.add(format!("Rule {} failed: {}", rule.name, e));
        }
    }
    
    errors.into_result(())
}
```

---

## 🔧 Kimi-Specific Integration

### Using SetTodoList for CLI Development

```rust
// Example of tracking tasks in development

fn development_todo() -> Vec<Task> {
    vec![
        Task::new("Setup Cargo project structure", Status::Done),
        Task::new("Implement error error.rs", Status::Done),
        Task::new("Implement data model", Status::InProgress),
        Task::new("Implement processor.rs with Result types", Status::Pending),
        Task::new("Implement CLI with clap", Status::Pending),
        Task::new("Write comprehensive tests", Status::Pending),
        Task::new("Create documentation (3 tiers)", Status::Pending),
    ]
}
```

### Parallel Tool Calls

Kimi can validate multiple Rust components:

```bash
# Type check multiple files
ReadFile: src/main.rs
ReadFile: src/processor.rs
ReadFile: src/validator.rs
ReadFile: src/formatter.rs

# Run tests in parallel
cargo test --bin rust_cli &
cargo test --lib &
cargo check --all-features &
cargo clippy -- -W warnings &
wait
```

---

## 📈 Benefits of FP Approach

### Compared to Traditional Rust Error Handling:

| Aspect | Traditional | Railway FP |
|--------|-------------|------------|
| Error handling | `match` or `?` | Chained operations |
| Composition | Manual nesting | `and_then`/`map` |
| Error types | Any type | Explicit error enum |
| Testing | Check panic | Check Err branches |
| Debugging | Unwrap/panic | Explicit paths |

---

## 🔗 Cross-References

**Project Documentation:**
- [ARCHITECTURE_PLAN.md](./ARCHITECTURE_PLAN.md) - Strategic overview
- [docs/plans/CLI_DESIGN.md](./docs/plans/CLI_DESIGN.md) - Technical design
- [docs/plans/IMPLEMENTATION_TODO.md](./docs/plans/IMPLEMENTATION_TODO.md) - Task tracking

**Kimi Guides:**
- [kimi/KIMI_WORKFLOW_GUIDE.md](../../KIMI_WORKFLOW_GUIDE.md) - Workflow patterns
- [kimi/rust-fp-style-guide.md](../../kimi/rust-fp-style-guide.md) - Rust FP patterns
- [kimi/DATA_STRUCTURE_PATTERNS.md](../../kimi/DATA_STRUCTURE_PATTERNS.md) - FP data structures

---

## ✅ Completion Checklist

This example is complete when:
- [ ] All source files implemented with Result types
- [ ] Comprehensive tests written and passing
- [ ] Documentation hierarchy established (3 tiers)
- [ ] SetTodoList usage demonstrated
- [ ] Parallel validation examples created
- [ ] Cross-references functional
- [ ] Follows KIMI.md conventions
- [ ] Git checkpoint with completion summary

---

**Status**: 🔄 IN PROGRESS (baseline established)
**Last Updated**: 2025-11-14 20:00
**Maintained By**: Kimi CLI Global Rules System

🤖 Generated with [Kimi](https://kimi.ai)

Co-Authored-By: Kimi <noreply@kimi.ai>

---

*This is a functional Rust CLI example using native Result types for railway-oriented programming. See [kimi/KIMI_WORKFLOW_GUIDE.md](../../kimi/KIMI_WORKFLOW_GUIDE.md) for Kimi-specific usage patterns.*
