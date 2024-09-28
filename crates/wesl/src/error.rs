use std::{collections::HashMap, fmt::Display, rc::Rc};

use wgsl_parse::{span::Span, syntax::*};

use crate::{CondCompError, EvalError, ImportError, ResolveError};

#[derive(Clone, Debug, thiserror::Error)]
pub enum Error {
    #[error("{0}")]
    ParseError(#[from] wgsl_parse::Error),
    #[error("import resolution failure: {0}")]
    ResolveError(#[from] ResolveError),
    #[cfg(feature = "imports")]
    #[error("import error: {0}")]
    ImportError(#[from] ImportError),
    #[cfg(feature = "condcomp")]
    #[error("conditional compilation error: {0}")]
    CondCompError(#[from] CondCompError),
    #[cfg(feature = "consteval")]
    #[error("constant evaluation error: {0}")]
    ConstEvalError(#[from] EvalError),
}

#[derive(Clone, Debug)]
pub struct Diagnostic {
    pub error: Error,
    pub source: Option<String>,
    pub file: Option<String>,
    pub declaration: Option<String>,
    pub span: Option<Span>,
}

impl Diagnostic {
    pub fn new(error: Error) -> Diagnostic {
        Diagnostic {
            error,
            source: Default::default(),
            file: Default::default(),
            declaration: Default::default(),
            span: Default::default(),
        }
    }
}

impl std::error::Error for Diagnostic {}

impl Display for Diagnostic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use annotate_snippets::*;
        let title = format!("{}", self.error);
        let mut msg = Level::Error.title(&title);

        if let Some(span) = &self.span {
            let source = self.source.as_ref().map(|source| source.as_str());

            if let Some(source) = source {
                if span.range().end <= source.len() {
                    let annot = Level::Error.span(span.range()).label(&title);
                    let mut snip = Snippet::source(source).fold(true).annotation(annot);

                    if let Some(file) = &self.file {
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
