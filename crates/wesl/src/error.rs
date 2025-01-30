use std::fmt::Display;

use wgsl_parse::{
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
    ParseError(#[from] wgsl_parse::Error),
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
    pub output: Option<String>,
    pub resource: Option<Resource>,
    pub display_name: Option<String>,
    pub declaration: Option<String>,
    pub span: Option<Span>,
}

impl From<wgsl_parse::Error> for Diagnostic<Error> {
    fn from(error: wgsl_parse::Error) -> Self {
        let span = error.span.clone();
        let mut res = Self::new(Error::ParseError(error));
        res.span = Some(span);
        res
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
            Error::Error(e) => e,
        }
    }
}

impl<E: std::error::Error> Diagnostic<E> {
    fn new(error: E) -> Diagnostic<E> {
        Self {
            error: Box::new(error),
            source: None,
            output: None,
            resource: None,
            display_name: None,
            declaration: None,
            span: None,
        }
    }
    pub fn with_source(mut self, source: String) -> Self {
        self.source = Some(source);
        self
    }
    pub fn with_span(mut self, span: Span) -> Self {
        self.span = Some(span);
        self
    }
    pub fn with_declaration(mut self, decl: String) -> Self {
        self.declaration = Some(decl);
        self
    }
    pub fn with_output(mut self, out: String) -> Self {
        self.output = Some(out);
        self
    }
    pub fn with_resource(mut self, resource: Resource, disp_name: Option<String>) -> Self {
        self.resource = Some(resource);
        self.display_name = disp_name;
        self
    }
    #[cfg(feature = "eval")]
    pub fn with_ctx(mut self, ctx: &Context) -> Self {
        let (decl, span) = ctx.err_ctx();
        self.declaration = decl.map(|id| id.to_string());
        self.span = span;
        self
    }

    pub fn with_sourcemap(mut self, sourcemap: &impl SourceMap) -> Self {
        if let Some(decl) = &self.declaration {
            if let Some((resource, decl)) = sourcemap.get_decl(decl) {
                self.resource = Some(resource.clone());
                self.declaration = Some(decl.to_string());
                self.display_name = sourcemap
                    .get_display_name(resource)
                    .map(|name| name.to_string());
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

    pub fn display_origin(&self) -> String {
        match (&self.resource, &self.display_name) {
            (Some(res), Some(name)) => {
                format!("{res} ({name})")
            }
            (Some(res), None) => res.to_string(),
            (None, Some(name)) => name.to_string(),
            (None, None) => "unknown module".to_string(),
        }
    }

    pub fn display_short_origin(&self) -> Option<String> {
        self.display_name
            .clone()
            .or_else(|| self.resource.as_ref().map(|res| res.to_string()))
    }

    pub fn error_message(&self) -> String {
        self.error.to_string()
    }
}

impl Diagnostic<Error> {
    // XXX: this function has issues when the root module identifiers are not mangled.
    pub fn unmangle(
        mut self,
        sourcemap: Option<&impl SourceMap>,
        mangler: Option<&impl Mangler>,
    ) -> Self {
        fn unmangle_id(
            id: &mut Ident,
            sourcemap: Option<&impl SourceMap>,
            mangler: Option<&impl Mangler>,
        ) {
            let res_name = if let Some(sourcemap) = sourcemap {
                sourcemap
                    .get_decl(&*id.name())
                    .map(|(res, name)| (res.clone(), name.to_string()))
            } else if let Some(mangler) = mangler {
                mangler.unmangle(&*id.name())
            } else {
                None
            };
            if let Some((res, name)) = res_name {
                *id = Ident::new(format!("{res}::{name}"));
            }
        }
        fn unmangle_name(
            mangled: &mut String,
            sourcemap: Option<&impl SourceMap>,
            mangler: Option<&impl Mangler>,
        ) {
            let res_name = if let Some(sourcemap) = sourcemap {
                sourcemap
                    .get_decl(mangled)
                    .map(|(res, name)| (res.clone(), name.to_string()))
            } else if let Some(mangler) = mangler {
                mangler.unmangle(mangled)
            } else {
                None
            };
            if let Some((res, name)) = res_name {
                *mangled = format!("{res}::{name}");
            }
        }
        fn unmangle_expr(
            expr: &mut Expression,
            sourcemap: Option<&impl SourceMap>,
            mangler: Option<&impl Mangler>,
        ) {
            match expr {
                Expression::Literal(_) => {}
                Expression::Parenthesized(e) => {
                    unmangle_expr(&mut e.expression, sourcemap, mangler)
                }
                Expression::NamedComponent(e) => unmangle_expr(&mut e.base, sourcemap, mangler),
                Expression::Indexing(e) => unmangle_expr(&mut e.base, sourcemap, mangler),
                Expression::Unary(e) => unmangle_expr(&mut e.operand, sourcemap, mangler),
                Expression::Binary(e) => {
                    unmangle_expr(&mut e.left, sourcemap, mangler);
                    unmangle_expr(&mut e.right, sourcemap, mangler);
                }
                Expression::FunctionCall(e) => {
                    unmangle_id(&mut e.ty.ident, sourcemap, mangler);
                    for arg in &mut e.arguments {
                        unmangle_expr(arg, sourcemap, mangler);
                    }
                }
                Expression::TypeOrIdentifier(ty) => unmangle_id(&mut ty.ident, sourcemap, mangler),
            }
        }
        match &mut *self.error {
            Error::ParseError(_) => {}
            Error::ValidateError(e) => match e {
                ValidateError::UndefinedSymbol(name)
                | ValidateError::ParamCount(name, _, _)
                | ValidateError::NotCallable(name) => unmangle_name(name, sourcemap, mangler),
            },
            Error::ResolveError(_) => {}
            #[cfg(feature = "imports")]
            Error::ImportError(_) => {}
            #[cfg(feature = "condcomp")]
            Error::CondCompError(e) => match e {
                CondCompError::InvalidExpression(expr) => unmangle_expr(expr, sourcemap, mangler),
                CondCompError::InvalidFeatureFlag(_) | CondCompError::MissingFeatureFlag(_) => {}
            },
            #[cfg(feature = "generics")]
            Error::GenericsError(_) => {}
            #[cfg(feature = "eval")]
            Error::EvalError(e) => match e {
                EvalError::UnknownFunction(name) => unmangle_name(name, sourcemap, mangler),
                EvalError::NoDecl(name) => unmangle_name(name, sourcemap, mangler),
                EvalError::Component(_, name) => unmangle_name(name, sourcemap, mangler),
                EvalError::Signature(ty, _) => unmangle_id(&mut ty.ident, sourcemap, mangler),
                EvalError::UnexpectedTemplate(name) => unmangle_name(name, sourcemap, mangler),
                EvalError::ParamCount(name, _, _) => unmangle_name(name, sourcemap, mangler),
                EvalError::NotConst(name) => unmangle_name(name, sourcemap, mangler),
                EvalError::UninitConst(name) => unmangle_name(name, sourcemap, mangler),
                EvalError::UninitLet(name) => unmangle_name(name, sourcemap, mangler),
                EvalError::UninitOverride(name) => unmangle_name(name, sourcemap, mangler),
                EvalError::DuplicateDecl(name) => unmangle_name(name, sourcemap, mangler),
                EvalError::ConstAssertFailure(expr) => unmangle_expr(expr, sourcemap, mangler),
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

        let orig = self.display_origin();
        let short_orig = self.display_short_origin();

        if let Some(span) = &self.span {
            let source = self.source.as_deref();

            if let Some(source) = source {
                if span.range().end <= source.len() {
                    let annot = Level::Error.span(span.range()).label(&title);
                    let mut snip = Snippet::source(source).fold(true).annotation(annot);

                    if let Some(orig) = &short_orig {
                        snip = snip.origin(orig);
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
            note = format!("in declaration of `{decl}` in {orig}");
            msg = msg.footer(Level::Note.title(&note));
        }

        let renderer = Renderer::styled();
        let rendered = renderer.render(msg);
        write!(f, "{rendered}")
    }
}
