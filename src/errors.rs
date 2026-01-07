use eyre::Report;
use thiserror::Error;

use crate::core::parser::errors::ParserError;

#[derive(Error, Debug)]
pub enum Error {
    #[error("Parser error: {0}")]
    Parser(#[from] ParserError),

    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),

    #[error("TOML error: {0}")]
    Toml(#[from] toml::ser::Error),

    #[error(transparent)]
    Other(#[from] Report),
}

// Convenience type alias
pub type Result<T> = std::result::Result<T, Error>;
