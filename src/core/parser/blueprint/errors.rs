use thiserror::Error;

use crate::core::parser::{
    errors::ParserError,
    sol_types::{self, SolTypeError},
};

#[derive(Debug, Error)]
pub enum BlueprintParserError {
    #[error("Invalid template variable: {0}")]
    InvalidTemplateVariable(String),
    #[error("Type mismatch: expected {expected}, got {actual}")]
    TypeMismatch { expected: String, actual: String },
    #[error("Failed to load blueprint: {0}")]
    FailedToLoadBlueprint(#[from] ParserError),
    #[error("Invalid address format in '{field_path}': {value}")]
    InvalidAddressFormat { field_path: String, value: String },
    #[error("Invalid builtin: {0}")]
    InvalidBuiltin(String),
    #[error("Invalid Sol type: {0}")]
    InvalidSolType(#[from] Box<sol_types::SolTypeError>),
    #[error("Template variable '{variable}' not found in '{source_path}'")]
    TemplateVariableNotFound {
        source_path: String,
        variable: String,
    },
    #[error("Template source '{source_path}' is not valid for '{context}'")]
    InvalidTemplateSource {
        source_path: String,
        context: String,
    },
    #[error("Tuple length mismatch in '{field_path}': expected {expected}, got {actual}")]
    TupleLengthMismatch {
        field_path: String,
        expected: usize,
        actual: usize,
    },
    #[error("Fixed array length mismatch in '{field_path}': expected {expected}, got {actual}")]
    FixedArrayLengthMismatch {
        field_path: String,
        expected: usize,
        actual: usize,
    },
    #[error("Duplicate return name: {name} in '{call_path}'")]
    DuplicateReturnName { name: String, call_path: String },
    #[error("Input type must be scalar in '{field_path}', found: {found_type}")]
    InputTypeMustBeScalar {
        field_path: String,
        found_type: String,
    },
    #[error("Reserved slot type must be scalar in '{field_path}', found: {found_type}")]
    ReservedSlotMustBeScalar {
        field_path: String,
        found_type: String,
    },
    #[error(
        "Reserved slot value must be a template variable (${{...}}) in '{field_path}', found: {value}"
    )]
    ReservedSlotValueMustBeTemplate { field_path: String, value: String },
    #[error(
        "Invalid source '{source_path}' for reserved slot template in '{field_path}'. Must be 'returns', 'constants', or 'builtins'."
    )]
    InvalidReservedSlotSource {
        field_path: String,
        source_path: String,
    },
    #[error(
        "Invalid custom struct in '{field_path}': expected {properties_length} properties, got {types_length} types"
    )]
    InvalidCustomStruct {
        field_path: String,
        struct_name: String,
        properties_length: usize,
        types_length: usize,
    },
    #[error("Invalid meta type in '{field_path}': {found_type}")]
    InvalidMetaType {
        field_path: String,
        found_type: String,
    },
    #[error("YAML parsing error: {0}")]
    YamlParse(#[from] saphyr::ScanError),
}

impl From<SolTypeError> for BlueprintParserError {
    fn from(value: SolTypeError) -> Self {
        BlueprintParserError::InvalidSolType(Box::new(value))
    }
}

pub type BlueprintResult<T> = Result<T, BlueprintParserError>;
