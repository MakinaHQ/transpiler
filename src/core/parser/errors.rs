use thiserror::Error;

#[derive(Error, Debug)]
pub enum ParserError {
    #[error("Invalid include path: {0}")]
    InvalidIncludePath(String),

    #[error("Circular include detected: {0}")]
    CircularInclude(String),

    #[error("Unsupported file extension: {0}")]
    UnsupportedFileExtension(String),

    #[error("Unsupported tag: {0}")]
    UnsupportedTag(String),

    #[error("YAML parsing error: {0}")]
    YamlParse(#[from] saphyr::ScanError),

    #[error("Invalid Yaml")]
    InvalidYaml,

    #[error("Invalid builtin: {0}")]
    InvalidBuiltin(String),

    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),

    #[error("Invalid YAML format in '{field_path}': expected {expected_type}, found {found_yaml}")]
    InvalidYamlFormat {
        field_path: String,
        expected_type: String,
        found_yaml: String,
    },

    #[error("Missing required field: {field_path}")]
    MissingRequiredField { field_path: String },
}

pub type ParserResult<T> = Result<T, ParserError>;
