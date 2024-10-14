//! A [`SpannedError`] is the error type returned by `Parser::parse*` functions.

use std::{
    borrow::Cow,
    fmt::{Debug, Display},
};

use itertools::Itertools;
use thiserror::Error;

use crate::{lexer::Token, span::Span};

#[derive(Error, Clone, Debug, PartialEq)]
pub enum ParseError {
    #[error("invalid token")]
    InvalidToken,
    #[error("unexpected token `{token}`, expected `{}`", .expected.iter().format(", "))]
    UnexpectedToken { token: Token, expected: Vec<String> },
    #[error("unexpected end of file, expected `{}`", .expected.iter().format(", "))]
    UnexpectedEof { expected: Vec<String> },
    #[error("extra token `{0}` at the end of the file")]
    ExtraToken(Token),
    #[error("invalid diagnostic severity")]
    DiagnosticSeverity,
    #[error("invalid `{0}` attribute, {1}")]
    Attribute(&'static str, &'static str),
    #[error("invalid `var` template arguments, {0}")]
    VarTemplate(&'static str),
}

#[derive(Default, Clone, Debug, PartialEq)]
pub enum CustomLalrError {
    #[default]
    LexerError,
    DiagnosticSeverity,
    Attribute(&'static str, &'static str),
    VarTemplate(&'static str),
}

type LalrError = lalrpop_util::ParseError<usize, Token, (usize, CustomLalrError, usize)>;

#[derive(Error, Clone, Debug, PartialEq)]
pub struct Error {
    pub error: ParseError,
    pub span: Span,
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "chars {:?}: {}", self.span.range(), self.error)
    }
}

impl From<LalrError> for Error {
    fn from(err: LalrError) -> Self {
        match err {
            LalrError::InvalidToken { location } => {
                let span = Span::new(location..location + 1);
                let error = ParseError::InvalidToken;
                Self { span, error }
            }
            LalrError::UnrecognizedEof { location, expected } => {
                let span = Span::new(location..location + 1);
                let error = ParseError::UnexpectedEof { expected };
                Self { span, error }
            }
            LalrError::UnrecognizedToken {
                token: (l, token, r),
                expected,
            } => {
                let span = Span::new(l..r);
                let error = ParseError::UnexpectedToken { token, expected };
                Self { span, error }
            }
            LalrError::ExtraToken {
                token: (l, token, r),
            } => {
                let span = Span::new(l..r);
                let error = ParseError::ExtraToken(token);
                Self { span, error }
            }
            LalrError::User {
                error: (l, error, r),
            } => {
                let span = Span::new(l..r);
                let error = match error {
                    CustomLalrError::DiagnosticSeverity => ParseError::DiagnosticSeverity,
                    CustomLalrError::LexerError => ParseError::InvalidToken,
                    CustomLalrError::Attribute(attr, expected) => {
                        ParseError::Attribute(attr, expected)
                    }
                    CustomLalrError::VarTemplate(reason) => ParseError::VarTemplate(reason),
                };
                Self { span, error }
            }
        }
    }
}

pub trait FormatError: Sized + Debug {
    fn fmt_err(&self, f: &mut std::fmt::Formatter<'_>, source: &str) -> std::fmt::Result;

    fn with_source(self, source: Cow<'_, str>) -> ErrorWithSource<'_, Self> {
        ErrorWithSource::new(self, source)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ErrorWithSource<'s, T: FormatError> {
    pub error: T,
    pub source: Cow<'s, str>,
}

impl<'s, T: FormatError> std::error::Error for ErrorWithSource<'s, T> {}

impl<'s, T: FormatError> ErrorWithSource<'s, T> {
    pub fn new(message: T, source: Cow<'s, str>) -> Self {
        Self {
            error: message,
            source,
        }
    }
}

impl<'s, F: FormatError> Display for ErrorWithSource<'s, F> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.error.fmt_err(f, &self.source)
    }
}

impl FormatError for Error {
    fn fmt_err(&self, f: &mut std::fmt::Formatter<'_>, source: &str) -> std::fmt::Result {
        use annotate_snippets::*;
        let text = format!("{}", self.error);

        let annot = Level::Info.span(self.span.range());
        let snip = Snippet::source(source).fold(true).annotation(annot);
        let msg = Level::Error.title(&text).snippet(snip);

        let renderer = Renderer::styled();
        let rendered = renderer.render(msg);
        write!(f, "{rendered}")
    }
}
