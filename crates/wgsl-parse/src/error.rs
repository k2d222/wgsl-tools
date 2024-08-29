//! A [`SpannedError`] is the error type returned by `Parser::parse*` functions.

use std::fmt::Display;

use annotate_snippets::*;
use itertools::Itertools;
use thiserror::Error;

use crate::lexer::Token;

#[derive(Error, Clone, Debug, Default, PartialEq)]
pub enum ParseError {
    #[default]
    #[error("syntax error")]
    SyntaxError,
    #[error("invalid diagnostic severity")]
    ParseDiagnosticSeverity,
}

type LalrError = lalrpop_util::ParseError<usize, Token, (usize, ParseError, usize)>;

#[derive(Clone, Debug, PartialEq)]
pub struct SpannedError<'s> {
    inner: LalrError,
    source: &'s str,
}

impl<'s> SpannedError<'s> {
    pub(crate) fn new(inner: LalrError, source: &'s str) -> Self {
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
            LalrError::InvalidToken { location } => {
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
            LalrError::UnrecognizedEof { location, expected } => {
                let annot = format!("expected {}", expected.iter().format(", "));
                let message = Level::Error.title("unexpected end of file").snippet(
                    Snippet::source(source)
                        .fold(true)
                        .annotation(Level::Error.span(*location..*location).label(&annot)),
                );
                let rendered = renderer.render(message);
                write!(f, "{}", rendered)
            }
            LalrError::UnrecognizedToken { token, expected } => {
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
            LalrError::ExtraToken { token } => {
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
            LalrError::User {
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
