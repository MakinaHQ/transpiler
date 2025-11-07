use thiserror::Error;

use crate::core::parser::{
    errors::ParserError,
    sol_types::{self, SolTypeError},
};

pub type PositionParserResult<T> = Result<T, PositionParserError>;

#[derive(Error, Debug)]
pub enum PositionParserError {
    #[error("Failed to load position: {0}")]
    FailedToLoadPosition(#[from] ParserError),
    #[error("Invalid builtin: {0}")]
    InvalidBuiltin(String),
    #[error("Invalid Sol type: {0}")]
    InvalidSolType(#[from] Box<sol_types::SolTypeError>),
    #[error("Invalid instruction type: {0}")]
    InvalidInstructionType(String),
    #[error("Invalid address format in '{field_path}': {value}")]
    InvalidAddressFormat { field_path: String, value: String },
    #[error("Input type must be scalar in '{field_path}', found: {found_type}")]
    InputTypeMustBeScalar {
        field_path: String,
        found_type: String,
    },
    #[error("Invalid template variable: {0}")]
    InvalidTemplateVariable(String),
    #[error("Template source '{source_path}' is not valid for '{context}'")]
    InvalidTemplateSource {
        source_path: String,
        context: String,
    },
    #[error("Template variable '{variable}' not found in '{source_path}'")]
    TemplateVariableNotFound {
        source_path: String,
        variable: String,
    },
    #[error("Type mismatch: expected {expected}, got {actual}")]
    TypeMismatch { expected: String, actual: String },
    #[error("Invalid path: {value} in {field_path}")]
    InvalidPath { field_path: String, value: String },
    #[error("Invalid tag: \"{raw}\" - expected format: <tag>:<instruction_name>")]
    InvalidTag { raw: String },
}

impl From<SolTypeError> for PositionParserError {
    fn from(value: SolTypeError) -> Self {
        PositionParserError::InvalidSolType(Box::new(value))
    }
}
