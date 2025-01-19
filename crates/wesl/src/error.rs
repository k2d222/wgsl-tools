use std::fmt::Display;

use wgsl_parse::{
    error::ParseError,
    span::Span,
    syntax::{Expression, Ident},
};

use crate::{Mangler, ResolveError, Resource, SourceMap, ValidateError};

#[cfg(feature = "condcomp")]
use crate::CondCompError;
#[cfg(feature = "generics")]
use crate::GenericsError;
#[cfg(feature = "imports")]
use crate::ImportError;

#[cfg(feature = "eval")]
use crate::eval::{Context, EvalError};

#[derive(Clone, Debug, thiserror::Error)]
pub enum Error {
    #[error("{0}")]
    ParseError(#[from] ParseError),
    #[error("{0}")]
    ValidateError(#[from] ValidateError),
    #[error("{0}")]
    ResolveError(#[from] ResolveError),
    #[cfg(feature = "imports")]
    #[error("{0}")]
    ImportError(#[from] ImportError),
    #[cfg(feature = "condcomp")]
    #[error("{0}")]
    CondCompError(#[from] CondCompError),
    #[cfg(feature = "generics")]
    #[error("{0}")]
    GenericsError(#[from] GenericsError),
    #[cfg(feature = "eval")]
    #[error("{0}")]
    EvalError(#[from] EvalError),
    #[error("{0}")]
    Error(#[from] Diagnostic<Error>),
}

#[derive(Clone, Debug)]
pub struct Diagnostic<E: std::error::Error> {
    pub error: Box<E>,
    pub source: Option<String>,
    pub file: Option<Resource>,
    pub declaration: Option<String>,
    pub span: Option<Span>,
}

impl From<wgsl_parse::Error> for Diagnostic<Error> {
    fn from(error: wgsl_parse::Error) -> Self {
        let mut res = Self::new(Error::ParseError(error.error));
        res.span = Some(error.span);
        res
    }
}

impl From<ParseError> for Diagnostic<Error> {
    fn from(error: ParseError) -> Self {
        Self::new(error.into())
    }
}

impl From<ValidateError> for Diagnostic<Error> {
    fn from(error: ValidateError) -> Self {
        Self::new(error.into())
    }
}

impl From<ResolveError> for Diagnostic<Error> {
    fn from(error: ResolveError) -> Self {
        match error {
            ResolveError::InvalidResource(_, _) | ResolveError::FileNotFound(_, _) => {
                Self::new(error.into())
            }
            ResolveError::Error(e) => e,
        }
    }
}

#[cfg(feature = "imports")]
impl From<ImportError> for Diagnostic<Error> {
    fn from(error: ImportError) -> Self {
        match error {
            ImportError::ResolveError(e) => Self::from(e),
            _ => Self::new(error.into()),
        }
    }
}

#[cfg(feature = "condcomp")]
impl From<CondCompError> for Diagnostic<Error> {
    fn from(error: CondCompError) -> Self {
        Self::new(error.into())
    }
}

#[cfg(feature = "generics")]
impl From<GenericsError> for Diagnostic<Error> {
    fn from(error: GenericsError) -> Self {
        Self::new(error.into())
    }
}

#[cfg(feature = "eval")]
impl From<EvalError> for Diagnostic<Error> {
    fn from(error: EvalError) -> Self {
        Self::new(error.into())
    }
}

impl From<Error> for Diagnostic<Error> {
    fn from(error: Error) -> Self {
        match error {
            Error::ParseError(e) => e.into(),
            Error::ValidateError(e) => e.into(),
            Error::ResolveError(e) => e.into(),
            #[cfg(feature = "imports")]
            Error::ImportError(e) => e.into(),
            #[cfg(feature = "condcomp")]
            Error::CondCompError(e) => e.into(),
            #[cfg(feature = "generics")]
            Error::GenericsError(e) => e.into(),
            #[cfg(feature = "eval")]
            Error::EvalError(e) => e.into(),
            Error::Error(e) => return e,
        }
    }
}

impl<E: std::error::Error> Diagnostic<E> {
    fn new(error: E) -> Diagnostic<E> {
        Self {
            error: Box::new(error),
            source: None,
            file: None,
            declaration: None,
            span: None,
        }
    }
    pub fn with_source(mut self, source: String) -> Self {
        self.source = Some(source);
        self
    }
    pub fn with_file(mut self, file: Resource) -> Self {
        self.file = Some(file);
        self
    }

    #[cfg(feature = "eval")]
    pub fn with_ctx(mut self, ctx: &Context) -> Self {
        let (decl, span) = ctx.err_ctx();
        self.declaration = decl.map(|d| d.name().to_string());
        self.span = span;
        self
    }

    pub fn with_sourcemap(mut self, sourcemap: &(impl SourceMap + std::fmt::Debug)) -> Self {
        if let Some(decl) = &self.declaration {
            if let Some((resource, decl)) = sourcemap.get_decl(&decl) {
                self.file = Some(resource.clone());
                self.declaration = Some(decl.to_string());
                self.source = sourcemap
                    .get_source(resource)
                    .map(|s| s.to_string())
                    .or(self.source);
            } else {
                self.source = sourcemap
                    .get_default_source()
                    .map(|s| s.to_string())
                    .or(self.source);
            }
        } else {
            self.source = sourcemap
                .get_default_source()
                .map(|s| s.to_string())
                .or(self.source);
        }
        self
    }
}

impl Diagnostic<Error> {
    pub fn unmangle(mut self, mangler: &impl Mangler) -> Self {
        fn unmangle_id(id: &mut Ident, mangler: &impl Mangler) {
            let unmangled = mangler.unmangle(id.name().as_str());
            if let Some((resource, name)) = unmangled {
                *id = Ident::new(format!("{resource}::{name}"));
            }
        }
        fn unmangle_expr(expr: &mut Expression, mangler: &impl Mangler) {
            match expr {
                Expression::Literal(_) => {}
                Expression::Parenthesized(e) => unmangle_expr(&mut e.expression, mangler),
                Expression::NamedComponent(e) => unmangle_expr(&mut e.base, mangler),
                Expression::Indexing(e) => unmangle_expr(&mut e.base, mangler),
                Expression::Unary(e) => unmangle_expr(&mut e.operand, mangler),
                Expression::Binary(e) => {
                    unmangle_expr(&mut e.left, mangler);
                    unmangle_expr(&mut e.right, mangler);
                }
                Expression::FunctionCall(e) => {
                    unmangle_id(&mut e.ty.ident, mangler);
                    for arg in &mut e.arguments {
                        unmangle_expr(arg, mangler);
                    }
                }
                Expression::TypeOrIdentifier(ty) => unmangle_id(&mut ty.ident, mangler),
            }
        }
        match &mut *self.error {
            Error::ParseError(_) => {}
            Error::ValidateError(e) => match e {
                ValidateError::UndefinedSymbol(id)
                | ValidateError::ParamCount(id, _, _)
                | ValidateError::UnknownFunction(id) => unmangle_id(id, mangler),
            },
            Error::ResolveError(_) => {}
            #[cfg(feature = "imports")]
            Error::ImportError(_) => todo!(),
            #[cfg(feature = "condcomp")]
            Error::CondCompError(e) => match e {
                CondCompError::InvalidExpression(expr) => unmangle_expr(expr, mangler),
                CondCompError::InvalidFeatureFlag(_) | CondCompError::MissingFeatureFlag(_) => {}
            },
            #[cfg(feature = "generics")]
            Error::GenericsError(_) => {}
            #[cfg(feature = "eval")]
            Error::EvalError(e) => match e {
                EvalError::UnknownFunction(id) => unmangle_id(id, mangler),
                EvalError::NoDecl(id) => unmangle_id(id, mangler),
                EvalError::Component(_, id) => unmangle_id(id, mangler),
                EvalError::Signature(ty, _) => unmangle_id(&mut ty.ident, mangler),
                EvalError::UnexpectedTemplate(id) => unmangle_id(id, mangler),
                EvalError::ParamCount(id, _, _) => unmangle_id(id, mangler),
                EvalError::NotConst(id) => unmangle_id(id, mangler),
                EvalError::UninitConst(id) => unmangle_id(id, mangler),
                EvalError::UninitLet(id) => unmangle_id(id, mangler),
                EvalError::UninitOverride(id) => unmangle_id(id, mangler),
                EvalError::DuplicateDecl(id) => unmangle_id(id, mangler),
                EvalError::ConstAssertFailure(expr) => unmangle_expr(expr, mangler),
                _ => {}
            },
            Error::Error(_) => {}
        };

        self
    }
}

impl<E: std::error::Error> std::error::Error for Diagnostic<E> {}

impl<E: std::error::Error> Display for Diagnostic<E> {
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

        let note;
        if let Some(decl) = &self.declaration {
            if let Some(file) = orig.as_ref() {
                note = format!("in declaration of `{decl}` in `{file}`");
            } else {
                note = format!("in declaration of `{decl}`");
            }
            msg = msg.footer(Level::Note.title(&note));
        }

        let renderer = Renderer::styled();
        let rendered = renderer.render(msg);
        write!(f, "{rendered}")
    }
}
