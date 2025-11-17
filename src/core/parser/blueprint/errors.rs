use std::ops::Deref;

use miette::{Diagnostic, NamedSource, SourceSpan};
use saphyr::MarkedYaml;
use saphyr_parser::Span;
use thiserror::Error;

/// General error struct returned from BlueprintParser
/// Contains a labled source location and an optional type definition
#[derive(Debug, Error, Diagnostic)]
#[error("Blueprint Error")]
pub struct BlueprintError {
    #[source_code]
    src: NamedSource<String>,
    #[label("{msg}")]
    location: SourceSpan,
    msg: String,
    #[label("{}", second_msg.as_ref().unwrap())]
    second_location: Option<SourceSpan>,
    second_msg: Option<String>,
    #[label("{}", third_msg.as_ref().unwrap())]
    third_location: Option<SourceSpan>,
    third_msg: Option<String>,
}

impl BlueprintError {
    pub fn new<S, Str>(src: NamedSource<String>, span: S, msg: Str) -> Self
    where
        S: Into<ErrorSpan>,
        Str: Into<String>,
    {
        Self {
            src,
            location: span.into().into(),
            msg: msg.into(),
            second_location: None,
            second_msg: None,
            third_location: None,
            third_msg: None,
        }
    }

    pub fn with_second_location<S, Str>(mut self, span: S, msg: Str) -> Self
    where
        S: Into<ErrorSpan>,
        Str: Into<String>,
    {
        self.second_location = Some(span.into().into());
        self.second_msg = Some(msg.into());
        self
    }

    #[expect(dead_code)]
    pub fn with_third_location<S, Str>(mut self, span: S, msg: Str) -> Self
    where
        S: Into<ErrorSpan>,
        Str: Into<String>,
    {
        self.third_location = Some(span.into().into());
        self.third_msg = Some(msg.into());
        self
    }
}

/// Helper type to keep track of a parsed items location in the source.
/// Source and location need to be tracked externally.
pub struct Marked<T> {
    pub inner: T,
    pub span: Span,
}

impl<T> Marked<T> {
    pub fn new(inner: T, span: Span) -> Self {
        Self { inner, span }
    }
}

impl<T> Deref for Marked<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

/// Helper type for conversion from saphyre's [Span] to miette's [SourceSpan]
pub struct ErrorSpan {
    inner: Span,
}

impl From<Span> for ErrorSpan {
    fn from(inner: Span) -> Self {
        Self { inner }
    }
}

impl<T> From<Marked<T>> for ErrorSpan {
    fn from(marked: Marked<T>) -> Self {
        marked.span.into()
    }
}

impl<'a> From<&'a MarkedYaml<'a>> for ErrorSpan {
    fn from(yaml: &'a MarkedYaml<'a>) -> Self {
        yaml.span.into()
    }
}

impl From<ErrorSpan> for SourceSpan {
    fn from(span: ErrorSpan) -> Self {
        let start = span.inner.start.index();
        let offset = span.inner.end.index() - start;
        (start, offset).into()
    }
}
