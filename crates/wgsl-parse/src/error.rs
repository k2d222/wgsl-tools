//! A spanned error is the error type returned by `Parser::parse*` functions.

use std::fmt::Display;

use annotate_snippets::*;
use itertools::Itertools;
use thiserror::Error;

use crate::lexer::Token;

#[derive(Error, Clone, Debug, Default, PartialEq)]
pub enum Error {
    #[default]
    #[error("syntax error")]
    SyntaxError,
    #[error("invalid diagnostic severity")]
    ParseDiagnosticSeverity,
}

type ParseError = lalrpop_util::ParseError<usize, Token, (usize, Error, usize)>;

#[derive(Clone, Debug, PartialEq)]
pub struct SpannedError<'s> {
    inner: ParseError,
    source: &'s str,
}

impl<'s> SpannedError<'s> {
    pub(crate) fn new(inner: ParseError, source: &'s str) -> Self {
        Self { inner, source }
    }
}

impl<'s> std::error::Error for SpannedError<'s> {}

impl<'s> Display for SpannedError<'s> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let err = &self.inner;
        let source = &self.source;
        let renderer = Renderer::styled();
        match err {
            ParseError::InvalidToken { location } => {
                let end = (*location + 1..)
                    .find(|e| source.is_char_boundary(*e))
                    .unwrap_or(*location);
                let message = Level::Error.title("invalid token").snippet(
                    Snippet::source(source).fold(true).annotation(
                        Level::Error
                            .span(*location..end)
                            .label("this token is unknown"),
                    ),
                );
                write!(f, "{}", renderer.render(message))
            }
            ParseError::UnrecognizedEof { location, expected } => {
                let annot = format!("expected {}", expected.iter().format(", "));
                let message = Level::Error.title("unexpected end of file").snippet(
                    Snippet::source(source)
                        .fold(true)
                        .annotation(Level::Error.span(*location..*location).label(&annot)),
                );
                let rendered = renderer.render(message);
                write!(f, "{}", rendered)
            }
            ParseError::UnrecognizedToken { token, expected } => {
                let title = format!("unexpected token `{}`", &source[token.0..token.2]);
                let annot = format!(
                    "expected {}, found {}",
                    expected.iter().format(", "),
                    token.1
                );
                let message = Level::Error.title(&title).snippet(
                    Snippet::source(source)
                        .fold(true)
                        .annotation(Level::Error.span(token.0..token.2).label(&annot)),
                );
                let rendered = renderer.render(message);
                write!(f, "{}", rendered)
            }
            ParseError::ExtraToken { token } => {
                let title = format!("extra token `{}`", &source[token.0..token.2]);
                let annot = format!("extra {} here", token.1);
                let message = Level::Error.title(&title).snippet(
                    Snippet::source(source)
                        .fold(true)
                        .annotation(Level::Error.span(token.0..token.2).label(&annot)),
                );
                let rendered = renderer.render(message);
                write!(f, "{}", rendered)
            }
            ParseError::User {
                error: (start, error, end),
            } => {
                let title = error.to_string();
                let message = Level::Error.title(&title).snippet(
                    Snippet::source(source).fold(true).annotation(
                        Level::Error
                            .span(*start..*end)
                            .label("while parsing this token"),
                    ),
                );
                let rendered = renderer.render(message);
                write!(f, "{}", rendered)
            }
        }
    }
}
