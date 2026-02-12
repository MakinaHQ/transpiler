use alloy::{
    dyn_abi::{DynSolType, DynSolValue},
    primitives::{FixedBytes, keccak256},
};
use regex::Regex;
use std::sync::LazyLock;
use thiserror::Error;

/// Errors that can occur when parsing a `${function(...)}` expression.
#[derive(Debug, Error, PartialEq)]
pub enum UtilParseError {
    /// The function name is not recognized.
    #[error("Unknown transpiler util function: '{name}'. Available functions: keccak256")]
    UnknownFunction { name: String },

    /// The syntax looks like a utility call but is malformed.
    #[error("Invalid transpiler util syntax: '{0}'")]
    InvalidSyntax(String),

    /// The argument is empty after parsing.
    #[error("{function}(): argument cannot be empty")]
    EmptyArgument { function: &'static str },
}

/// Errors specific to the keccak256 function.
#[derive(Debug, Error, PartialEq)]
pub enum Keccak256Error {
    /// The output type doesn't match the expected type (must be bytes32).
    #[error("keccak256() returns bytes32, but field expects {0}")]
    TypeMismatch(String),
}

/// Regex to match the full `${function(args)}` syntax.
/// Group 1: function name
/// Group 2: arguments (may include quotes)
static UTIL_SYNTAX_REGEX: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r#"^\$\{([a-zA-Z_][a-zA-Z0-9_]*)\(([^)]*)\)\}$"#).expect("valid regex")
});

/// A parsed keccak256 utility function call.
///
/// Represents a fully parsed `${keccak256(args)}` expression ready for evaluation.
#[derive(Debug, Clone, PartialEq)]
pub struct TranspilerUtil {
    /// The parsed argument string (quotes stripped if present).
    argument: String,
}

impl TranspilerUtil {
    /// Returns the argument string.
    pub fn argument(&self) -> &str {
        &self.argument
    }

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
    /// TranspilerUtil::try_parse("${keccak256(test/path value)}");// Ok (any string allowed)
    ///
    /// // Not a util call (returns None)
    /// TranspilerUtil::try_parse("0x1234");                       // None
    /// TranspilerUtil::try_parse("${config.value}");              // None
    ///
    /// // Invalid util call (returns Some(Err))
    /// TranspilerUtil::try_parse("${unknown_func(test)}");        // Some(Err)
    /// ```
    pub fn try_parse(s: &str) -> Option<Result<Self, UtilParseError>> {
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
                    return Some(Err(UtilParseError::InvalidSyntax(s.to_string())));
                }
                return None;
            }
        };

        let function_name = caps.get(1).map(|m| m.as_str()).unwrap_or("");
        let raw_args = caps.get(2).map(|m| m.as_str()).unwrap_or("");

        // Only keccak256 is supported
        if function_name != "keccak256" {
            return Some(Err(UtilParseError::UnknownFunction {
                name: function_name.to_string(),
            }));
        }

        // Parse the argument (handle optional quotes)
        let argument = Self::parse_argument(raw_args);

        // Validate: must be non-empty
        if argument.is_empty() {
            return Some(Err(UtilParseError::EmptyArgument {
                function: "keccak256",
            }));
        }

        Some(Ok(Self { argument }))
    }

    /// Parse the argument string, handling optional quotes.
    /// Accepts any non-empty string.
    fn parse_argument(raw: &str) -> String {
        let trimmed = raw.trim();

        // If wrapped in quotes, strip them
        if trimmed.starts_with('"') && trimmed.ends_with('"') && trimmed.len() >= 2 {
            return trimmed[1..trimmed.len() - 1].to_string();
        }

        trimmed.to_string()
    }

    /// Evaluate the utility function and return the keccak256 hash as bytes32.
    pub fn evaluate(&self) -> FixedBytes<32> {
        keccak256(self.argument.as_bytes())
    }

    /// Evaluate the utility function and convert to a [`DynSolValue`].
    ///
    /// This is the primary method for integrating transpiler utils with the parser.
    /// It evaluates the function, checks type compatibility, and returns the
    /// appropriate [`DynSolValue`] variant.
    ///
    /// # Errors
    ///
    /// Returns an error if the expected type is not `bytes32` (FixedBytes(32)).
    pub fn evaluate_to_dyn_sol_value(
        &self,
        expected_type: &DynSolType,
    ) -> Result<DynSolValue, Keccak256Error> {
        // keccak256 only returns bytes32
        if !matches!(expected_type, DynSolType::FixedBytes(32)) {
            return Err(Keccak256Error::TypeMismatch(expected_type.to_string()));
        }

        let hash = self.evaluate();
        Ok(DynSolValue::FixedBytes(hash, 32))
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
        assert_eq!(util.argument(), "test.identifier");
    }

    #[test]
    fn test_parse_keccak256_quoted() {
        let result = TranspilerUtil::try_parse("${keccak256(\"test.identifier\")}");
        assert!(result.is_some());
        let util = result.unwrap().unwrap();
        assert_eq!(util.argument(), "test.identifier");
    }

    #[test]
    fn test_parse_keccak256_complex_identifier() {
        let result =
            TranspilerUtil::try_parse("${keccak256(makina.mainnet.convex-fx.position-32)}");
        assert!(result.is_some());
        let util = result.unwrap().unwrap();
        assert_eq!(util.argument(), "makina.mainnet.convex-fx.position-32");
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
        assert!(matches!(err, UtilParseError::UnknownFunction { name } if name == "sha256"));
    }

    #[test]
    fn test_parse_unknown_function_error_message() {
        let result = TranspilerUtil::try_parse("${sha256(test)}");
        let err = result.unwrap().unwrap_err();
        let message = err.to_string();
        assert!(message.contains("sha256"));
        assert!(message.contains("keccak256")); // Lists available functions
    }

    #[test]
    fn test_parse_invalid_syntax_bad_function_name() {
        // Invalid function name (starts with digit) - looks like util call but regex doesn't match
        let result = TranspilerUtil::try_parse("${123func(test)}");
        assert!(result.is_some());
        let err = result.unwrap().unwrap_err();
        assert!(matches!(err, UtilParseError::InvalidSyntax(_)));
    }

    #[test]
    fn test_parse_malformed_not_util_call() {
        // Missing ) before } - doesn't end with )} so not recognized as util call
        let result = TranspilerUtil::try_parse("${keccak256(test}");
        assert!(result.is_none()); // Correctly returns None - not a util call
    }

    // ========== "Any String" Input Tests ==========

    #[test]
    fn test_parse_keccak256_with_spaces() {
        let result = TranspilerUtil::try_parse("${keccak256(test value)}");
        assert!(result.is_some());
        let util = result.unwrap().unwrap();
        assert_eq!(util.argument(), "test value");
    }

    #[test]
    fn test_parse_keccak256_with_slashes() {
        let result = TranspilerUtil::try_parse("${keccak256(test/path)}");
        assert!(result.is_some());
        let util = result.unwrap().unwrap();
        assert_eq!(util.argument(), "test/path");
    }

    #[test]
    fn test_parse_keccak256_with_special_chars() {
        let result = TranspilerUtil::try_parse("${keccak256(test@value#123)}");
        assert!(result.is_some());
        let util = result.unwrap().unwrap();
        assert_eq!(util.argument(), "test@value#123");
    }

    #[test]
    fn test_parse_keccak256_quoted_with_spaces() {
        let result = TranspilerUtil::try_parse("${keccak256(\"test/path value\")}");
        assert!(result.is_some());
        let util = result.unwrap().unwrap();
        assert_eq!(util.argument(), "test/path value");
    }

    #[test]
    fn test_parse_keccak256_leading_trailing_whitespace_trimmed() {
        let result = TranspilerUtil::try_parse("${keccak256(  test  )}");
        assert!(result.is_some());
        let util = result.unwrap().unwrap();
        assert_eq!(util.argument(), "test");
    }

    #[test]
    fn test_parse_keccak256_quoted_preserves_inner_whitespace() {
        let result = TranspilerUtil::try_parse("${keccak256(\"  test  \")}");
        assert!(result.is_some());
        let util = result.unwrap().unwrap();
        assert_eq!(util.argument(), "  test  ");
    }

    // ========== Empty Input Rejection Tests ==========

    #[test]
    fn test_parse_keccak256_empty_input() {
        let result = TranspilerUtil::try_parse("${keccak256()}");
        assert!(result.is_some());
        let err = result.unwrap().unwrap_err();
        assert!(matches!(
            err,
            UtilParseError::EmptyArgument {
                function: "keccak256"
            }
        ));
    }

    #[test]
    fn test_parse_keccak256_empty_quoted() {
        let result = TranspilerUtil::try_parse("${keccak256(\"\")}");
        assert!(result.is_some());
        let err = result.unwrap().unwrap_err();
        assert!(matches!(
            err,
            UtilParseError::EmptyArgument {
                function: "keccak256"
            }
        ));
    }

    #[test]
    fn test_parse_keccak256_whitespace_only() {
        let result = TranspilerUtil::try_parse("${keccak256(   )}");
        assert!(result.is_some());
        let err = result.unwrap().unwrap_err();
        assert!(matches!(
            err,
            UtilParseError::EmptyArgument {
                function: "keccak256"
            }
        ));
    }

    // ========== Evaluation Tests ==========

    #[test]
    fn test_evaluate_keccak256() {
        let util = TranspilerUtil::try_parse("${keccak256(test)}")
            .unwrap()
            .unwrap();
        let hash = util.evaluate();

        // Known hash for "test"
        let expected = keccak256("test".as_bytes());
        assert_eq!(hash, expected);
    }

    #[test]
    fn test_evaluate_keccak256_known_hash() {
        let util = TranspilerUtil::try_parse("${keccak256(test)}")
            .unwrap()
            .unwrap();
        let hash = util.evaluate();

        // keccak256("test") = 0x9c22ff5f21f0b81b113e63f7db6da94fedef11b2119b4088b89664fb9a3cb658
        assert_eq!(
            format!("{}", hash),
            "0x9c22ff5f21f0b81b113e63f7db6da94fedef11b2119b4088b89664fb9a3cb658"
        );
    }

    #[test]
    fn test_evaluate_keccak256_quoted_unquoted_same_result() {
        let util1 = TranspilerUtil::try_parse("${keccak256(test)}")
            .unwrap()
            .unwrap();
        let util2 = TranspilerUtil::try_parse("${keccak256(\"test\")}")
            .unwrap()
            .unwrap();

        assert_eq!(util1.evaluate(), util2.evaluate());
    }

    #[test]
    fn test_evaluate_keccak256_makina_convex_fx() {
        let util = TranspilerUtil::try_parse("${keccak256(makina.mainnet.convex-fx.32)}")
            .unwrap()
            .unwrap();
        let hash = util.evaluate();

        assert_eq!(
            format!("{}", hash),
            "0x968a5a59fb04b8c0dff68bf59d710c4db850b0e7b4063ae3dfb3b183aaef78f6"
        );
    }

    // ========== Type Compatibility Tests ==========

    #[test]
    fn test_evaluate_to_dyn_sol_value_bytes32_success() {
        let util = TranspilerUtil::try_parse("${keccak256(test)}")
            .unwrap()
            .unwrap();
        let result = util.evaluate_to_dyn_sol_value(&DynSolType::FixedBytes(32));
        assert!(result.is_ok());

        let value = result.unwrap();
        assert!(matches!(value, DynSolValue::FixedBytes(_, 32)));
    }

    #[test]
    fn test_evaluate_to_dyn_sol_value_address_fails() {
        let util = TranspilerUtil::try_parse("${keccak256(test)}")
            .unwrap()
            .unwrap();
        let result = util.evaluate_to_dyn_sol_value(&DynSolType::Address);
        assert!(matches!(result, Err(Keccak256Error::TypeMismatch(_))));
    }

    #[test]
    fn test_evaluate_to_dyn_sol_value_bytes16_fails() {
        let util = TranspilerUtil::try_parse("${keccak256(test)}")
            .unwrap()
            .unwrap();
        let result = util.evaluate_to_dyn_sol_value(&DynSolType::FixedBytes(16));
        assert!(matches!(result, Err(Keccak256Error::TypeMismatch(_))));
    }

    #[test]
    fn test_evaluate_to_dyn_sol_value_uint256_fails() {
        let util = TranspilerUtil::try_parse("${keccak256(test)}")
            .unwrap()
            .unwrap();
        let result = util.evaluate_to_dyn_sol_value(&DynSolType::Uint(256));
        assert!(matches!(result, Err(Keccak256Error::TypeMismatch(_))));
    }

    #[test]
    fn test_type_mismatch_error_message() {
        let util = TranspilerUtil::try_parse("${keccak256(test)}")
            .unwrap()
            .unwrap();
        let err = util
            .evaluate_to_dyn_sol_value(&DynSolType::Address)
            .unwrap_err();
        let message = err.to_string();
        assert!(message.contains("bytes32"));
        assert!(message.contains("address"));
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
    fn test_single_char_identifier() {
        // Single character identifiers should work
        let result = TranspilerUtil::try_parse("${keccak256(a)}");
        assert!(result.is_some());
        assert!(result.unwrap().is_ok());
    }
}
