use std::fmt::Display;

use wgsl_parse::{error::ParseError, span::Span};

use crate::{CondCompError, EvalError, ImportError, ResolveError, Resource};

#[derive(Clone, Debug, thiserror::Error)]
pub enum Error {
    #[error("{0}")]
    ParseError(#[from] ParseError),
    #[error("{0}")]
    ResolveError(#[from] ResolveError),
    #[cfg(feature = "imports")]
    #[error("{0}")]
    ImportError(#[from] ImportError),
    #[cfg(feature = "condcomp")]
    #[error("{0}")]
    CondCompError(#[from] CondCompError),
    #[cfg(feature = "eval")]
    #[error("{0}")]
    EvalError(#[from] EvalError),
}

#[derive(Clone, Debug)]
pub struct Diagnostic {
    pub error: Error,
    pub source: Option<String>,
    pub file: Option<Resource>,
    pub declaration: Option<String>,
    pub span: Option<Span>,
}

impl From<Error> for Diagnostic {
    fn from(error: Error) -> Self {
        Self {
            error,
            source: None,
            file: None,
            declaration: None,
            span: None,
        }
    }
}

impl From<wgsl_parse::Error> for Diagnostic {
    fn from(error: wgsl_parse::Error) -> Self {
        Self {
            error: error.error.into(),
            source: None,
            file: None,
            declaration: None,
            span: Some(error.span),
        }
    }
}

impl Diagnostic {
    pub fn new(error: Error) -> Diagnostic {
        error.into()
    }
    pub fn source(mut self, source: String) -> Self {
        self.source = Some(source);
        self
    }
    pub fn file(mut self, file: Resource) -> Self {
        self.file = Some(file);
        self
    }
}

impl std::error::Error for Diagnostic {}

impl Display for Diagnostic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use annotate_snippets::*;
        let title = format!("{}", self.error);
        let mut msg = Level::Error.title(&title);

        let orig = self
            .file
            .as_ref()
            .map(|file| file.path().display().to_string());

        if let Some(span) = &self.span {
            let source = self.source.as_ref().map(|source| source.as_str());

            if let Some(source) = source {
                if span.range().end <= source.len() {
                    let annot = Level::Error.span(span.range()).label(&title);
                    let mut snip = Snippet::source(source).fold(true).annotation(annot);

                    if let Some(file) = orig.as_ref() {
                        snip = snip.origin(file);
                    }

                    msg = msg.snippet(snip);
                } else {
                    msg = msg.footer(
                        Level::Note.title("cannot display snippet: invalid source location"),
                    )
                }
            } else {
                msg = msg.footer(Level::Note.title("cannot display snippet: missing source file"))
            }
        }

        let renderer = Renderer::styled();
        let rendered = renderer.render(msg);
        write!(f, "{rendered}")
    }
}
