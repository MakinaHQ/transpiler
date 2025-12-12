use std::ops::Deref;

use alloy::dyn_abi::DynSolType;
use miette::{Diagnostic, NamedSource, SourceSpan};
use regex::{Match, Regex};
use saphyr::{MarkedYaml, Marker};
use saphyr_parser::Span;
use thiserror::Error;

use crate::core::parser::sol_types::{YamlSolValue, parse_sol_type_str, parse_sol_value_marked};

/// General error struct returned from Parser
/// Contains a labeled source location and an optional type definition
#[derive(Debug, Error, Diagnostic)]
#[error("Parser Error")]
pub struct ParserError {
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

impl ParserError {
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

/// A parsed template string
pub struct TemplateRef<'a> {
    pub source: Marked<&'a str>,
    pub name: Marked<&'a str>,
    pub child: Option<Marked<&'a str>>,
}

pub trait Parser {
    /// Return [NamedSource] for this parser
    fn named_source(&self) -> NamedSource<String>;

    /// Return the [Regex] used for template parsing
    fn template_regex(&self) -> &Regex;

    /// Helper method to generate a [ParserError] referencing this parsers source
    fn error<S, Str>(&self, span: S, msg: Str) -> ParserError
    where
        S: Into<ErrorSpan>,
        Str: Into<String>,
    {
        ParserError::new(self.named_source(), span, msg)
    }

    fn sol_value<'a>(&self, field: &'a MarkedYaml<'a>) -> miette::Result<YamlSolValue> {
        let r#type = self.get_type(field, "type")?;

        let value_field = field
            .data
            .as_mapping_get("value")
            .ok_or(self.error(field.span, "'value' is missing"))?;

        let value = parse_sol_value_marked(&r#type, value_field).ok_or(
            self.error(value_field, "could not be parsed")
                .with_second_location(r#type.span, "into specified type"),
        )?;

        Ok(YamlSolValue {
            r#type: r#type.inner,
            value,
        })
    }

    /// Gets a string at key from a mapping
    fn get_str<'a>(&self, node: &'a MarkedYaml<'a>, key: &str) -> miette::Result<Marked<&'a str>> {
        let field = node
            .data
            .as_mapping_get(key)
            .ok_or_else(|| self.error(node.span, format!("'{key}' is missing")))?;

        let str = field
            .data
            .as_str()
            .ok_or_else(|| self.error(field.span, "must be a string".to_string()))?;

        Ok(Marked::new(str, field.span))
    }

    /// Gets an optional string from a mapping
    fn try_get_str<'a>(
        &self,
        node: &'a MarkedYaml<'a>,
        key: &str,
    ) -> miette::Result<Option<Marked<&'a str>>> {
        let Some(field) = node.data.as_mapping_get(key) else {
            return Ok(None);
        };

        let str = field
            .data
            .as_str()
            .ok_or_else(|| self.error(field.span, "must be a string".to_string()))?;

        Ok(Some(Marked::new(str, field.span)))
    }

    /// Gets a `DynSolType` from a mapping
    fn get_type<'a>(
        &self,
        node: &'a MarkedYaml<'a>,
        key: &str,
    ) -> miette::Result<Marked<DynSolType>> {
        let str = self.get_str(node, key)?;
        let r#type = parse_sol_type_str(*str).or(Err(self.error(str.span, "invalid type")))?;

        Ok(Marked::new(r#type, str.span))
    }

    /// Build a [Marked] str from a regex [Match]
    /// Assumes the match has no line breaks which should always be true given
    /// the `template_regex`
    fn mark_match<'a>(&self, r#match: Match<'a>, field: &'a MarkedYaml<'a>) -> Marked<&'a str> {
        let start = Marker::new(
            field.span.start.index() + r#match.start(),
            field.span.start.line(),
            field.span.start.col() + r#match.start(),
        );

        let end = Marker::new(
            field.span.start.index() + r#match.end(),
            field.span.start.line(),
            field.span.start.col() + r#match.end(),
        );

        let span = Span::new(start, end);

        Marked::new(r#match.as_str(), span)
    }

    /// Parses a template string
    fn parse_template_str<'a>(&self, field: &'a MarkedYaml<'a>) -> miette::Result<TemplateRef<'a>> {
        let str = field
            .data
            .as_str()
            .ok_or_else(|| self.error(field.span, "must be string"))?;

        let caps = self
            .template_regex()
            .captures(str)
            .ok_or_else(|| self.error(field.span, "expected template string"))?;

        let source = caps
            .get(1)
            .ok_or_else(|| self.error(field.span, "must have a source"))?;

        let name = caps
            .get(2)
            .ok_or_else(|| self.error(field.span, "must have a name"))?;

        let child = caps.get(3);

        Ok(TemplateRef {
            source: self.mark_match(source, field),
            name: self.mark_match(name, field),
            child: child.map(|c| self.mark_match(c, field)),
        })
    }

    /// Returns `true` if the field contains a string matching template field syntax
    fn is_template<'a>(&self, field: &'a MarkedYaml<'a>) -> bool {
        let Some(str) = field.data.as_str() else {
            return false;
        };

        self.template_regex().is_match(str)
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
