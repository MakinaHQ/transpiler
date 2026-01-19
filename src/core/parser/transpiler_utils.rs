//! Transpiler-time utility functions.
//!
//! These functions are evaluated during transpilation (not at runtime).
//! Use them in YAML with the syntax: `${function_name(args)}`
//!
//! # Available Functions
//!
//! | Function | Description | Output Type |
//! |----------|-------------|-------------|
//! | `keccak256(input)` | Computes the Keccak-256 hash of the input string | `bytes32` |
//!
//! # Examples
//!
//! ```yaml
//! # Compute a storage slot identifier
//! slot:
//!   type: "bytes32"
//!   value: "${keccak256(makina.mainnet.vault.position)}"
//!
//! # Quotes are optional but recommended for clarity
//! slot:
//!   type: "bytes32"
//!   value: "${keccak256(\"my.identifier\")}"
//! ```
//!
//! # Adding New Utility Functions
//!
//! To add a new utility function:
//! 1. Add a variant to [`TranspilerUtilKind`]
//! 2. Add a variant to [`TranspilerUtilOutputType`] if needed
//! 3. Implement `output_type()` for your function in [`TranspilerUtilKind`]
//! 4. Implement the evaluation logic in [`TranspilerUtil::evaluate`]
//! 5. Add validation in [`TranspilerUtil::validate_argument`]
//! 6. Update `available_functions_list()` and documentation above

use alloy::{
    dyn_abi::{DynSolType, DynSolValue},
    primitives::{Address, FixedBytes, U256, keccak256},
};
use regex::Regex;
use std::sync::LazyLock;
use thiserror::Error;

/// Errors that can occur when parsing or evaluating transpiler utility functions.
#[derive(Debug, Error, PartialEq)]
pub enum TranspilerUtilError {
    /// The function name is not recognized.
    #[error(
        "Unknown transpiler util function: '{name}'. Available functions: {}",
        TranspilerUtilKind::available_functions_list()
    )]
    UnknownFunction { name: String },

    /// The function was called with invalid arguments.
    #[error("{function}(): {message}")]
    InvalidArgument {
        function: &'static str,
        message: String,
    },

    /// The function output type doesn't match the expected type.
    #[error("{function}() returns {returns}, but field expects {expected}")]
    TypeMismatch {
        function: &'static str,
        returns: &'static str,
        expected: String,
    },

    /// The syntax looks like a utility call but is malformed.
    #[error("Invalid transpiler util syntax: '{0}'")]
    InvalidSyntax(String),
}

/// The kind of transpiler utility function.
///
/// Each variant represents a different utility function available
/// for use in YAML files during transpilation.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TranspilerUtilKind {
    /// `keccak256(input)` - Computes Keccak-256 hash, returns bytes32
    Keccak256,
}

impl TranspilerUtilKind {
    /// Returns the function name as used in YAML.
    pub const fn name(&self) -> &'static str {
        match self {
            Self::Keccak256 => "keccak256",
        }
    }

    /// Returns the native output type of this function.
    pub const fn output_type(&self) -> TranspilerUtilOutputType {
        match self {
            Self::Keccak256 => TranspilerUtilOutputType::Bytes32,
        }
    }

    /// Returns a human-readable description of the output type.
    pub const fn output_type_name(&self) -> &'static str {
        match self {
            Self::Keccak256 => "bytes32",
        }
    }

    /// Parse a function name string into a TranspilerUtilKind.
    pub fn from_name(name: &str) -> Option<Self> {
        match name {
            "keccak256" => Some(Self::Keccak256),
            _ => None,
        }
    }

    /// Returns a comma-separated list of all available function names.
    pub fn available_functions_list() -> String {
        Self::all()
            .iter()
            .map(|k| k.name())
            .collect::<Vec<_>>()
            .join(", ")
    }

    /// Returns all available utility function kinds.
    pub const fn all() -> &'static [Self] {
        &[Self::Keccak256]
    }

    /// Returns the documentation for this utility function.
    pub const fn doc(&self) -> TranspilerUtilDoc {
        match self {
            Self::Keccak256 => TranspilerUtilDoc {
                name: "keccak256",
                description: "Computes the Keccak-256 hash of the input string. \
                    This is the same hash function used by Solidity's keccak256().",
                input_type: "string",
                input_description: "An identifier-like string (alphanumeric, dots, underscores, hyphens). \
                    No leading/trailing dots, no consecutive dots.",
                output_type: TranspilerUtilOutputType::Bytes32,
                examples: &[
                    TranspilerUtilExample {
                        yaml: r#"slot:
  type: "bytes32"
  value: "${keccak256(makina.mainnet.vault.position)}""#,
                        description: "Compute a storage slot identifier",
                    },
                    TranspilerUtilExample {
                        yaml: r#"key:
  type: "bytes32"
  value: "${keccak256(\"my.custom.key\")}""#,
                        description: "Using quoted argument for clarity",
                    },
                ],
            },
        }
    }
}

/// Documentation for a transpiler utility function.
///
/// This struct provides all the metadata needed to document a utility function,
/// including its name, description, input/output types, and usage examples.
#[derive(Debug, Clone, Copy)]
pub struct TranspilerUtilDoc {
    /// The function name as used in YAML.
    pub name: &'static str,
    /// A description of what the function does.
    pub description: &'static str,
    /// The type of input the function expects (human-readable).
    pub input_type: &'static str,
    /// Description of the input format and constraints.
    pub input_description: &'static str,
    /// The output type of the function.
    pub output_type: TranspilerUtilOutputType,
    /// Usage examples.
    pub examples: &'static [TranspilerUtilExample],
}

impl TranspilerUtilDoc {
    /// Format the documentation as a human-readable string.
    pub fn format(&self) -> String {
        let mut output = String::new();

        output.push_str(&format!("## ${{{0}(input)}}\n\n", self.name));
        output.push_str(&format!("{}\n\n", self.description));
        output.push_str("### Signature\n\n");
        output.push_str(&format!(
            "- **Input:** `{}` - {}\n",
            self.input_type, self.input_description
        ));
        output.push_str(&format!("- **Output:** `{}`\n\n", self.output_type.name()));

        if !self.examples.is_empty() {
            output.push_str("### Examples\n\n");
            for example in self.examples {
                output.push_str(&format!("**{}**\n\n", example.description));
                output.push_str(&format!("```yaml\n{}\n```\n\n", example.yaml));
            }
        }

        output
    }

    /// Format the documentation as a compact single-line summary.
    pub fn format_short(&self) -> String {
        format!(
            "${{{name}(input)}} -> {output}  |  {desc}",
            name = self.name,
            output = self.output_type.name(),
            desc = self.description.lines().next().unwrap_or("")
        )
    }
}

/// An example of how to use a transpiler utility function.
#[derive(Debug, Clone, Copy)]
pub struct TranspilerUtilExample {
    /// The YAML code showing the usage.
    pub yaml: &'static str,
    /// A description of what this example demonstrates.
    pub description: &'static str,
}

/// Get all available utility function documentation.
pub fn all_utils_docs() -> Vec<TranspilerUtilDoc> {
    TranspilerUtilKind::all().iter().map(|k| k.doc()).collect()
}

/// Format all utility function documentation as a single string.
pub fn format_all_utils_docs() -> String {
    let mut output = String::new();
    output.push_str("# Transpiler Utility Functions\n\n");
    output.push_str("These functions are evaluated during transpilation (not at runtime).\n");
    output.push_str("Use them in YAML with the syntax: `${function_name(args)}`\n\n");
    output.push_str("---\n\n");

    for doc in all_utils_docs() {
        output.push_str(&doc.format());
        output.push_str("---\n\n");
    }

    output
}

/// Format a compact list of all utility functions.
pub fn format_utils_list() -> String {
    let mut output = String::new();
    output.push_str("Available transpiler utility functions:\n\n");

    for doc in all_utils_docs() {
        output.push_str(&format!("  {}\n", doc.format_short()));
    }

    output
}

/// The native output type of a transpiler utility function.
///
/// This enum represents the possible types that utility functions can return.
/// Each variant corresponds to a Solidity type that can be used in YAML fields.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TranspilerUtilOutputType {
    /// 32-byte fixed value (e.g., keccak256 hash)
    Bytes32,
    /// 4-byte fixed value (e.g., function selector)
    Bytes4,
    /// 256-bit unsigned integer
    Uint256,
    /// 20-byte Ethereum address
    Address,
    /// Boolean value
    Bool,
}

impl TranspilerUtilOutputType {
    /// Check if this output type is compatible with the given Solidity type.
    pub fn is_compatible_with(&self, sol_type: &DynSolType) -> bool {
        matches!(
            (self, sol_type),
            (Self::Bytes32, DynSolType::FixedBytes(32))
                | (Self::Bytes4, DynSolType::FixedBytes(4))
                | (Self::Uint256, DynSolType::Uint(256))
                | (Self::Address, DynSolType::Address)
                | (Self::Bool, DynSolType::Bool)
        )
    }

    /// Returns a human-readable name for this output type.
    pub const fn name(&self) -> &'static str {
        match self {
            Self::Bytes32 => "bytes32",
            Self::Bytes4 => "bytes4",
            Self::Uint256 => "uint256",
            Self::Address => "address",
            Self::Bool => "bool",
        }
    }
}

/// A parsed transpiler utility function call.
///
/// Represents a fully parsed `${function(args)}` expression ready for evaluation.
#[derive(Debug, Clone, PartialEq)]
pub struct TranspilerUtil {
    /// The kind of utility function.
    pub kind: TranspilerUtilKind,
    /// The raw argument string (content between parentheses).
    pub argument: String,
}

/// The result of evaluating a transpiler utility function.
///
/// This enum holds the evaluated value from a utility function.
/// Use [`into_dyn_sol_value`](Self::into_dyn_sol_value) to convert
/// to a [`DynSolValue`] for use in the parser.
#[derive(Debug, Clone, PartialEq)]
pub enum TranspilerUtilValue {
    /// A 32-byte fixed value (e.g., from keccak256).
    Bytes32(FixedBytes<32>),
    /// A 4-byte fixed value (e.g., function selector).
    Bytes4(FixedBytes<4>),
    /// A 256-bit unsigned integer.
    Uint256(U256),
    /// A 20-byte Ethereum address.
    Address(Address),
    /// A boolean value.
    Bool(bool),
}

impl TranspilerUtilValue {
    /// Returns the output type of this value.
    pub const fn output_type(&self) -> TranspilerUtilOutputType {
        match self {
            Self::Bytes32(_) => TranspilerUtilOutputType::Bytes32,
            Self::Bytes4(_) => TranspilerUtilOutputType::Bytes4,
            Self::Uint256(_) => TranspilerUtilOutputType::Uint256,
            Self::Address(_) => TranspilerUtilOutputType::Address,
            Self::Bool(_) => TranspilerUtilOutputType::Bool,
        }
    }

    /// Convert this value to a [`DynSolValue`], validating against the expected type.
    ///
    /// Returns an error if the value's type doesn't match the expected Solidity type.
    pub fn into_dyn_sol_value(
        self,
        expected_type: &DynSolType,
        function_name: &'static str,
    ) -> Result<DynSolValue, TranspilerUtilError> {
        let output_type = self.output_type();

        if !output_type.is_compatible_with(expected_type) {
            return Err(TranspilerUtilError::TypeMismatch {
                function: function_name,
                returns: output_type.name(),
                expected: expected_type.to_string(),
            });
        }

        Ok(match self {
            Self::Bytes32(v) => DynSolValue::FixedBytes(v, 32),
            Self::Bytes4(v) => {
                // Pad bytes4 to 32 bytes for FixedBytes representation
                let mut padded = FixedBytes::<32>::ZERO;
                padded[..4].copy_from_slice(&v[..]);
                DynSolValue::FixedBytes(padded, 4)
            }
            Self::Uint256(v) => DynSolValue::Uint(v, 256),
            Self::Address(v) => DynSolValue::Address(v),
            Self::Bool(v) => DynSolValue::Bool(v),
        })
    }

    /// Extract as bytes32. Panics if wrong type.
    #[allow(dead_code)]
    pub fn into_bytes32(self) -> FixedBytes<32> {
        match self {
            Self::Bytes32(v) => v,
            _ => panic!("Expected Bytes32 value"),
        }
    }

    /// Extract as bytes4. Panics if wrong type.
    #[allow(dead_code)]
    pub fn into_bytes4(self) -> FixedBytes<4> {
        match self {
            Self::Bytes4(v) => v,
            _ => panic!("Expected Bytes4 value"),
        }
    }

    /// Extract as uint256. Panics if wrong type.
    #[allow(dead_code)]
    pub fn into_uint256(self) -> U256 {
        match self {
            Self::Uint256(v) => v,
            _ => panic!("Expected Uint256 value"),
        }
    }

    /// Extract as address. Panics if wrong type.
    #[allow(dead_code)]
    pub fn into_address(self) -> Address {
        match self {
            Self::Address(v) => v,
            _ => panic!("Expected Address value"),
        }
    }

    /// Extract as bool. Panics if wrong type.
    #[allow(dead_code)]
    pub fn into_bool(self) -> bool {
        match self {
            Self::Bool(v) => v,
            _ => panic!("Expected Bool value"),
        }
    }
}

/// Regex to match the full `${function(args)}` syntax.
/// Group 1: function name
/// Group 2: arguments (may include quotes)
static UTIL_SYNTAX_REGEX: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r#"^\$\{([a-zA-Z_][a-zA-Z0-9_]*)\(([^)]*)\)\}$"#).expect("valid regex")
});

/// Regex to extract content from optionally quoted arguments.
/// Supports: `content` or `"content"`
/// Group 1: quoted content (if quotes present)
/// Group 2: unquoted content (if no quotes)
static ARGUMENT_CONTENT_REGEX: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r#"^(?:"([^"]*)"|([^"]*))$"#).expect("valid regex"));

impl TranspilerUtil {
    /// Check if a string looks like a transpiler utility call.
    ///
    /// Returns `true` if the string starts with `${` and ends with `)}`,
    /// suggesting it might be a utility function call.
    pub fn looks_like_util_call(s: &str) -> bool {
        s.starts_with("${") && s.ends_with(")}")
    }

    /// Try to parse a string as a transpiler utility function call.
    ///
    /// Returns `None` if the string doesn't match the `${function(args)}` syntax.
    /// Returns `Some(Err(...))` if it matches the syntax but has errors (unknown function, etc).
    /// Returns `Some(Ok(...))` if successfully parsed.
    ///
    /// # Examples
    ///
    /// ```ignore
    /// // Valid calls
    /// TranspilerUtil::try_parse("${keccak256(test)}");           // Ok
    /// TranspilerUtil::try_parse("${keccak256(\"test\")}");       // Ok
    ///
    /// // Not a util call (returns None)
    /// TranspilerUtil::try_parse("0x1234");                       // None
    /// TranspilerUtil::try_parse("${config.value}");              // None
    ///
    /// // Invalid util call (returns Some(Err))
    /// TranspilerUtil::try_parse("${unknown_func(test)}");        // Some(Err)
    /// ```
    pub fn try_parse(s: &str) -> Option<Result<Self, TranspilerUtilError>> {
        // Quick check: must look like a util call
        if !Self::looks_like_util_call(s) {
            return None;
        }

        // Try to match the full syntax
        let caps = match UTIL_SYNTAX_REGEX.captures(s) {
            Some(caps) => caps,
            None => {
                // Looks like a util call but doesn't match - could be a template like ${config.x}
                // Check if it contains a '(' to distinguish
                if s.contains('(') {
                    return Some(Err(TranspilerUtilError::InvalidSyntax(s.to_string())));
                }
                return None;
            }
        };

        let function_name = caps.get(1).map(|m| m.as_str()).unwrap_or("");
        let raw_args = caps.get(2).map(|m| m.as_str()).unwrap_or("");

        // Look up the function
        let kind = match TranspilerUtilKind::from_name(function_name) {
            Some(k) => k,
            None => {
                return Some(Err(TranspilerUtilError::UnknownFunction {
                    name: function_name.to_string(),
                }));
            }
        };

        // Parse the argument (handle optional quotes)
        let argument = match Self::parse_argument(raw_args, kind) {
            Some(Ok(arg)) => arg,
            Some(Err(e)) => return Some(Err(e)),
            None => {
                return Some(Err(TranspilerUtilError::InvalidArgument {
                    function: kind.name(),
                    message: "failed to parse argument".to_string(),
                }));
            }
        };

        Some(Ok(Self { kind, argument }))
    }

    /// Parse the argument string, handling optional quotes and validation.
    fn parse_argument(
        raw: &str,
        kind: TranspilerUtilKind,
    ) -> Option<Result<String, TranspilerUtilError>> {
        // Use regex to handle quoted vs unquoted
        let content = if let Some(caps) = ARGUMENT_CONTENT_REGEX.captures(raw) {
            // Group 1 = quoted content, Group 2 = unquoted content
            caps.get(1)
                .or_else(|| caps.get(2))
                .map(|m| m.as_str().to_string())
                .unwrap_or_default()
        } else {
            // Malformed (e.g., unbalanced quotes)
            return Some(Err(TranspilerUtilError::InvalidArgument {
                function: kind.name(),
                message: format!("malformed argument: '{raw}'"),
            }));
        };

        // Validate the argument
        if let Err(e) = Self::validate_argument(&content, kind) {
            return Some(Err(e));
        }

        Some(Ok(content))
    }

    /// Validate the argument for a specific function.
    fn validate_argument(arg: &str, kind: TranspilerUtilKind) -> Result<(), TranspilerUtilError> {
        match kind {
            TranspilerUtilKind::Keccak256 => {
                // keccak256 requires non-empty input
                if arg.is_empty() {
                    return Err(TranspilerUtilError::InvalidArgument {
                        function: "keccak256",
                        message: "input cannot be empty".to_string(),
                    });
                }

                // keccak256 input should be a valid identifier-like string
                // Allow: alphanumeric, dots, underscores, hyphens
                if !arg
                    .chars()
                    .all(|c| c.is_alphanumeric() || c == '.' || c == '_' || c == '-')
                {
                    return Err(TranspilerUtilError::InvalidArgument {
                        function: "keccak256",
                        message: format!(
                            "input contains invalid characters: '{arg}'. \
                             Allowed: alphanumeric, dots, underscores, hyphens"
                        ),
                    });
                }

                // Validate structure: no consecutive dots, no leading/trailing dots
                if arg.starts_with('.') || arg.ends_with('.') {
                    return Err(TranspilerUtilError::InvalidArgument {
                        function: "keccak256",
                        message: format!("input cannot start or end with a dot: '{arg}'"),
                    });
                }

                if arg.contains("..") {
                    return Err(TranspilerUtilError::InvalidArgument {
                        function: "keccak256",
                        message: format!("input cannot contain consecutive dots: '{arg}'"),
                    });
                }

                Ok(())
            }
        }
    }

    /// Evaluate the utility function and return its raw value.
    ///
    /// For most use cases, prefer [`evaluate_to_dyn_sol_value`](Self::evaluate_to_dyn_sol_value)
    /// which also handles type checking and conversion.
    pub fn evaluate(&self) -> Result<TranspilerUtilValue, TranspilerUtilError> {
        match self.kind {
            TranspilerUtilKind::Keccak256 => {
                let hash = keccak256(self.argument.as_bytes());
                Ok(TranspilerUtilValue::Bytes32(hash))
            }
        }
    }

    /// Evaluate the utility function and convert to a [`DynSolValue`].
    ///
    /// This is the primary method for integrating transpiler utils with the parser.
    /// It evaluates the function, checks type compatibility, and returns the
    /// appropriate [`DynSolValue`] variant.
    ///
    /// # Errors
    ///
    /// Returns an error if:
    /// - The function evaluation fails
    /// - The output type is incompatible with the expected Solidity type
    pub fn evaluate_to_dyn_sol_value(
        &self,
        expected_type: &DynSolType,
    ) -> Result<DynSolValue, TranspilerUtilError> {
        let value = self.evaluate()?;
        value.into_dyn_sol_value(expected_type, self.kind.name())
    }

    /// Check if the output type is compatible with an expected Solidity type.
    pub fn check_type_compatibility_sol(
        &self,
        expected_type: &DynSolType,
    ) -> Result<(), TranspilerUtilError> {
        let output_type = self.kind.output_type();

        if !output_type.is_compatible_with(expected_type) {
            return Err(TranspilerUtilError::TypeMismatch {
                function: self.kind.name(),
                returns: output_type.name(),
                expected: expected_type.to_string(),
            });
        }

        Ok(())
    }

    /// Check if the output type is compatible with an expected type string.
    ///
    /// This is a convenience method for string-based type checking.
    /// For [`DynSolType`] checking, use [`check_type_compatibility_sol`](Self::check_type_compatibility_sol).
    pub fn check_type_compatibility(&self, expected_type: &str) -> Result<(), TranspilerUtilError> {
        let output_type = self.kind.output_type();

        // Normalize the expected type for comparison
        let normalized_expected = expected_type.trim().to_lowercase();

        if output_type.name() != normalized_expected {
            return Err(TranspilerUtilError::TypeMismatch {
                function: self.kind.name(),
                returns: output_type.name(),
                expected: expected_type.to_string(),
            });
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // ========== Parsing Tests ==========

    #[test]
    fn test_parse_keccak256_unquoted() {
        let result = TranspilerUtil::try_parse("${keccak256(test.identifier)}");
        assert!(result.is_some());
        let util = result.unwrap().unwrap();
        assert_eq!(util.kind, TranspilerUtilKind::Keccak256);
        assert_eq!(util.argument, "test.identifier");
    }

    #[test]
    fn test_parse_keccak256_quoted() {
        let result = TranspilerUtil::try_parse("${keccak256(\"test.identifier\")}");
        assert!(result.is_some());
        let util = result.unwrap().unwrap();
        assert_eq!(util.kind, TranspilerUtilKind::Keccak256);
        assert_eq!(util.argument, "test.identifier");
    }

    #[test]
    fn test_parse_keccak256_complex_identifier() {
        let result =
            TranspilerUtil::try_parse("${keccak256(makina.mainnet.convex-fx.position-32)}");
        assert!(result.is_some());
        let util = result.unwrap().unwrap();
        assert_eq!(util.argument, "makina.mainnet.convex-fx.position-32");
    }

    #[test]
    fn test_parse_not_a_util_call_regular_hex() {
        let result = TranspilerUtil::try_parse("0x1234abcd");
        assert!(result.is_none());
    }

    #[test]
    fn test_parse_not_a_util_call_template() {
        // Template variables should return None, not an error
        let result = TranspilerUtil::try_parse("${config.my_value}");
        assert!(result.is_none());
    }

    #[test]
    fn test_parse_not_a_util_call_nested_template() {
        let result = TranspilerUtil::try_parse("${inputs.token.address}");
        assert!(result.is_none());
    }

    #[test]
    fn test_parse_unknown_function() {
        let result = TranspilerUtil::try_parse("${sha256(test)}");
        assert!(result.is_some());
        let err = result.unwrap().unwrap_err();
        assert!(matches!(err, TranspilerUtilError::UnknownFunction { name } if name == "sha256"));
    }

    // ========== Validation Tests ==========

    #[test]
    fn test_validate_keccak256_empty_input() {
        let result = TranspilerUtil::try_parse("${keccak256()}");
        assert!(result.is_some());
        let err = result.unwrap().unwrap_err();
        assert!(matches!(
            err,
            TranspilerUtilError::InvalidArgument {
                function: "keccak256",
                ..
            }
        ));
    }

    #[test]
    fn test_validate_keccak256_empty_quoted() {
        let result = TranspilerUtil::try_parse("${keccak256(\"\")}");
        assert!(result.is_some());
        let err = result.unwrap().unwrap_err();
        assert!(matches!(
            err,
            TranspilerUtilError::InvalidArgument {
                function: "keccak256",
                ..
            }
        ));
    }

    #[test]
    fn test_validate_keccak256_invalid_chars() {
        let result = TranspilerUtil::try_parse("${keccak256(test/path)}");
        assert!(result.is_some());
        let err = result.unwrap().unwrap_err();
        match err {
            TranspilerUtilError::InvalidArgument { function, message } => {
                assert_eq!(function, "keccak256");
                assert!(message.contains("invalid characters"));
            }
            _ => panic!("Expected InvalidArgument error"),
        }
    }

    #[test]
    fn test_validate_keccak256_spaces() {
        let result = TranspilerUtil::try_parse("${keccak256(test value)}");
        assert!(result.is_some());
        let err = result.unwrap().unwrap_err();
        assert!(matches!(
            err,
            TranspilerUtilError::InvalidArgument {
                function: "keccak256",
                ..
            }
        ));
    }

    #[test]
    fn test_validate_keccak256_leading_dot() {
        let result = TranspilerUtil::try_parse("${keccak256(.test)}");
        assert!(result.is_some());
        let err = result.unwrap().unwrap_err();
        match err {
            TranspilerUtilError::InvalidArgument { message, .. } => {
                assert!(message.contains("cannot start or end with a dot"));
            }
            _ => panic!("Expected InvalidArgument error"),
        }
    }

    #[test]
    fn test_validate_keccak256_trailing_dot() {
        let result = TranspilerUtil::try_parse("${keccak256(test.)}");
        assert!(result.is_some());
        let err = result.unwrap().unwrap_err();
        match err {
            TranspilerUtilError::InvalidArgument { message, .. } => {
                assert!(message.contains("cannot start or end with a dot"));
            }
            _ => panic!("Expected InvalidArgument error"),
        }
    }

    #[test]
    fn test_validate_keccak256_consecutive_dots() {
        let result = TranspilerUtil::try_parse("${keccak256(test..value)}");
        assert!(result.is_some());
        let err = result.unwrap().unwrap_err();
        match err {
            TranspilerUtilError::InvalidArgument { message, .. } => {
                assert!(message.contains("consecutive dots"));
            }
            _ => panic!("Expected InvalidArgument error"),
        }
    }

    // ========== Evaluation Tests ==========

    #[test]
    fn test_evaluate_keccak256() {
        let util = TranspilerUtil::try_parse("${keccak256(test)}")
            .unwrap()
            .unwrap();
        let value = util.evaluate().unwrap();

        // Known hash for "test"
        let expected = keccak256("test".as_bytes());
        assert_eq!(value, TranspilerUtilValue::Bytes32(expected));
    }

    #[test]
    fn test_evaluate_keccak256_known_hash() {
        let util = TranspilerUtil::try_parse("${keccak256(test)}")
            .unwrap()
            .unwrap();
        let value = util.evaluate().unwrap();

        if let TranspilerUtilValue::Bytes32(hash) = value {
            // keccak256("test") = 0x9c22ff5f21f0b81b113e63f7db6da94fedef11b2119b4088b89664fb9a3cb658
            assert_eq!(
                format!("{}", hash),
                "0x9c22ff5f21f0b81b113e63f7db6da94fedef11b2119b4088b89664fb9a3cb658"
            );
        } else {
            panic!("Expected Bytes32 value");
        }
    }

    #[test]
    fn test_evaluate_keccak256_quoted_unquoted_same_result() {
        let util1 = TranspilerUtil::try_parse("${keccak256(test)}")
            .unwrap()
            .unwrap();
        let util2 = TranspilerUtil::try_parse("${keccak256(\"test\")}")
            .unwrap()
            .unwrap();

        let value1 = util1.evaluate().unwrap();
        let value2 = util2.evaluate().unwrap();

        assert_eq!(value1, value2);
    }

    #[test]
    fn test_evaluate_keccak256_makina_convex_fx() {
        let util = TranspilerUtil::try_parse("${keccak256(makina.mainnet.convex-fx.32)}")
            .unwrap()
            .unwrap();
        let value = util.evaluate().unwrap();

        if let TranspilerUtilValue::Bytes32(hash) = value {
            assert_eq!(
                format!("{}", hash),
                "0x968a5a59fb04b8c0dff68bf59d710c4db850b0e7b4063ae3dfb3b183aaef78f6"
            );
        } else {
            panic!("Expected Bytes32 value");
        }
    }

    // ========== Utility Kind Tests ==========
    // Note: Type compatibility is tested via integration tests in sol_types.rs

    #[test]
    fn test_util_kind_from_name() {
        assert_eq!(
            TranspilerUtilKind::from_name("keccak256"),
            Some(TranspilerUtilKind::Keccak256)
        );
        assert_eq!(TranspilerUtilKind::from_name("unknown"), None);
    }

    // ========== Edge Cases ==========

    #[test]
    fn test_looks_like_util_call() {
        assert!(TranspilerUtil::looks_like_util_call("${keccak256(test)}"));
        assert!(TranspilerUtil::looks_like_util_call("${foo()}"));
        assert!(!TranspilerUtil::looks_like_util_call("${config.value}"));
        assert!(!TranspilerUtil::looks_like_util_call("0x1234"));
        assert!(!TranspilerUtil::looks_like_util_call("keccak256(test)"));
    }

    #[test]
    fn test_value_into_bytes32() {
        let hash = keccak256("test".as_bytes());
        let value = TranspilerUtilValue::Bytes32(hash);
        assert_eq!(value.into_bytes32(), hash);
    }
}
