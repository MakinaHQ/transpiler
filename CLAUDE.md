# Claude Instructions

This file contains instructions for Claude (AI assistant) when working on this codebase.

## Project Overview

This is a Rust-based transpiler that converts YAML instruction files into TOML rootfiles for the Makina protocol. It handles DeFi position management with weiroll-based bytecode generation.

## Adding New Transpiler Utility Functions

Transpiler utility functions are evaluated at transpile-time (not runtime) and can be used in YAML with the syntax `${function_name(args)}`.

### Files to Modify

All changes go in `src/core/parser/transpiler_utils.rs`:

### Step-by-Step Guide

#### 1. Add the function variant to `TranspilerUtilKind`

```rust
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TranspilerUtilKind {
    Keccak256,
    NewFunction,  // Add your new variant here
}
```

#### 2. Implement `name()` - the function name as used in YAML

```rust
pub const fn name(&self) -> &'static str {
    match self {
        Self::Keccak256 => "keccak256",
        Self::NewFunction => "new_function",
    }
}
```

#### 3. Implement `output_type()` - what type the function returns

```rust
pub const fn output_type(&self) -> TranspilerUtilOutputType {
    match self {
        Self::Keccak256 => TranspilerUtilOutputType::Bytes32,
        Self::NewFunction => TranspilerUtilOutputType::Uint256,  // or Bytes4, Address, Bool
    }
}
```

#### 4. Implement `output_type_name()` - human-readable output type

```rust
pub const fn output_type_name(&self) -> &'static str {
    match self {
        Self::Keccak256 => "bytes32",
        Self::NewFunction => "uint256",
    }
}
```

#### 5. Update `from_name()` - parse the function name from YAML

```rust
pub fn from_name(name: &str) -> Option<Self> {
    match name {
        "keccak256" => Some(Self::Keccak256),
        "new_function" => Some(Self::NewFunction),
        _ => None,
    }
}
```

#### 6. Update `all()` - list of all available functions

```rust
pub const fn all() -> &'static [Self] {
    &[Self::Keccak256, Self::NewFunction]
}
```

#### 7. Add documentation in `doc()`

```rust
pub const fn doc(&self) -> TranspilerUtilDoc {
    match self {
        Self::Keccak256 => { /* existing */ },
        Self::NewFunction => TranspilerUtilDoc {
            name: "new_function",
            description: "Brief description of what the function does.",
            input_type: "string",  // or "number", "hex", etc.
            input_description: "Detailed description of valid input format and constraints.",
            output_type: TranspilerUtilOutputType::Uint256,
            examples: &[
                TranspilerUtilExample {
                    yaml: r#"field:
  type: "uint256"
  value: "${new_function(input)}""#,
                    description: "Example description",
                },
            ],
        },
    }
}
```

#### 8. Add validation in `validate_argument()`

```rust
fn validate_argument(arg: &str, kind: TranspilerUtilKind) -> Result<(), TranspilerUtilError> {
    match kind {
        TranspilerUtilKind::Keccak256 => { /* existing validation */ },
        TranspilerUtilKind::NewFunction => {
            if arg.is_empty() {
                return Err(TranspilerUtilError::InvalidArgument {
                    function: "new_function",
                    message: "input cannot be empty".to_string(),
                });
            }
            // Add more validation as needed
            Ok(())
        }
    }
}
```

#### 9. Implement evaluation in `evaluate()`

```rust
pub fn evaluate(&self) -> Result<TranspilerUtilValue, TranspilerUtilError> {
    match self.kind {
        TranspilerUtilKind::Keccak256 => { /* existing */ },
        TranspilerUtilKind::NewFunction => {
            // Compute the result from self.argument
            let result = U256::from_str(&self.argument).unwrap();
            Ok(TranspilerUtilValue::Uint256(result))
        }
    }
}
```

#### 10. Add tests

Add tests in the `#[cfg(test)]` module at the bottom of the file:

```rust
#[test]
fn test_parse_new_function() {
    let result = TranspilerUtil::try_parse("${new_function(123)}");
    assert!(result.is_some());
    let util = result.unwrap().unwrap();
    assert_eq!(util.kind, TranspilerUtilKind::NewFunction);
}

#[test]
fn test_evaluate_new_function() {
    let util = TranspilerUtil::try_parse("${new_function(123)}")
        .unwrap()
        .unwrap();
    let value = util.evaluate().unwrap();
    // Assert expected result
}
```

### Available Output Types

- `TranspilerUtilOutputType::Bytes32` - 32-byte fixed value (e.g., hashes)
- `TranspilerUtilOutputType::Bytes4` - 4-byte fixed value (e.g., function selectors)
- `TranspilerUtilOutputType::Uint256` - 256-bit unsigned integer
- `TranspilerUtilOutputType::Address` - 20-byte Ethereum address
- `TranspilerUtilOutputType::Bool` - Boolean value

### Corresponding Value Types

When returning from `evaluate()`, use the matching `TranspilerUtilValue` variant:

- `TranspilerUtilValue::Bytes32(FixedBytes<32>)`
- `TranspilerUtilValue::Bytes4(FixedBytes<4>)`
- `TranspilerUtilValue::Uint256(U256)`
- `TranspilerUtilValue::Address(Address)`
- `TranspilerUtilValue::Bool(bool)`

### Testing Your Changes

```bash
# Check compilation
cargo check --all

# Run tests
cargo test transpiler_utils

# Test the CLI documentation
cargo run -- list-utils --verbose
```

### Important Notes

1. **Type safety**: The `TranspilerUtilOutputType` must match what you return in `evaluate()`. The type checking in `into_dyn_sol_value()` will catch mismatches.

2. **Validation**: Always validate input in `validate_argument()` before evaluation. This gives users clear error messages.

3. **Documentation**: Always add documentation in `doc()` - it's used by the CLI `list-utils` command and will be used for online docs.

4. **Tests**: Add unit tests for parsing and evaluation. Integration tests through `parse_sol_value` go in `sol_types.rs`.
