mod attrs;
mod builtin;
mod constant;
mod conv;
mod display;
mod error;
mod eval;
mod exec;
mod instance;
mod lower;
mod mem;
mod ops;
mod to_expr;
mod ty;

pub use attrs::*;
pub use builtin::*;
pub(crate) use constant::*;
pub use conv::*;
pub use error::*;
pub use eval::*;
pub use exec::*;
pub use instance::*;
pub use lower::*;
pub use mem::*;
pub use to_expr::*;
pub use ty::*;

use derive_more::Display;
use std::{collections::HashMap, rc::Rc};
use wgsl_parse::{span::Span, syntax::*};

#[derive(Clone, Debug)]
pub struct ScopeInner<T> {
    local: HashMap<String, T>,
    parent: Option<Rc<ScopeInner<T>>>,
}

#[derive(Clone, Debug)]
pub struct Scope<T> {
    inner: Rc<ScopeInner<T>>,
}

impl<T> Default for ScopeInner<T> {
    fn default() -> Self {
        Self {
            local: Default::default(),
            parent: Default::default(),
        }
    }
}

impl<T> Default for Scope<T> {
    fn default() -> Self {
        Self {
            inner: Default::default(),
        }
    }
}

impl<T> ScopeInner<T> {
    pub fn get(&self, name: &str) -> Option<&T> {
        self.local
            .get(name)
            .or_else(|| self.parent.as_ref().and_then(|parent| parent.get(name)))
    }
    pub fn contains(&self, name: &str) -> bool {
        self.local.contains_key(name)
            || self
                .parent
                .as_ref()
                .is_some_and(|parent| parent.contains(name))
    }
}

impl<T> Scope<T> {
    pub fn new() -> Self {
        Self {
            // stack: vec![Default::default()],
            inner: Rc::new(ScopeInner {
                local: Default::default(),
                parent: None,
            }),
        }
    }

    pub fn push(&mut self) {
        self.inner = self.inner.clone()
    }
    pub fn pop(&mut self) {
        self.inner = self
            .inner
            .parent
            .as_ref()
            .expect("failed to pop scope")
            .clone();
    }

    pub fn add(&mut self, name: String, value: T) {
        if Rc::get_mut(&mut self.inner)
            .expect("cannot edit a parent scope")
            .local
            .insert(name, value)
            .is_some()
        {
            panic!("duplicate scope value insertion")
        }
    }
    pub fn get(&self, name: &str) -> Option<&T> {
        self.inner.get(name)
    }
    pub fn local_contains(&self, name: &str) -> bool {
        self.inner.local.contains_key(name)
    }
    pub fn contains(&self, name: &str) -> bool {
        self.inner.contains(name)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Display)]
pub enum ScopeKind {
    #[display("module")]
    Module,
    #[display("function")]
    Function,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum EvalStage {
    /// Shader module creation
    Const,
    /// Pipeline creation
    Override,
    /// Shader execution
    Exec,
}

#[derive(Clone, Copy, Debug)]
pub enum ResourceKind {
    UniformBuffer,
    StorageBuffer,
    Texture,
    Sampler,
}

// TODO: should we remove the source from the Context struct?
pub struct Context<'s> {
    pub(crate) source: &'s TranslationUnit,
    pub(crate) scope: Scope<Instance>,
    pub(crate) resources: HashMap<(u32, u32), RefInstance>,
    pub(crate) overrides: HashMap<String, Instance>,
    pub(crate) kind: ScopeKind,
    pub(crate) stage: EvalStage,
    err_decl: Option<String>,
    err_expr: Option<Span>,
}

impl<'s> Context<'s> {
    pub fn new(source: &'s TranslationUnit) -> Self {
        Self {
            source,
            scope: Default::default(),
            resources: Default::default(),
            overrides: Default::default(),
            kind: ScopeKind::Function,
            stage: EvalStage::Const,
            err_expr: None,
            err_decl: None,
        }
    }

    pub fn source(&self) -> &TranslationUnit {
        self.source
    }

    fn set_err_decl_ctx(&mut self, decl: String) {
        if self.err_decl.is_none() {
            self.err_decl = Some(decl)
        }
    }
    fn set_err_expr_ctx(&mut self, expr: &Span) {
        if self.err_expr.is_none() {
            self.err_expr = Some(expr.clone())
        }
    }

    pub fn err_ctx(&self) -> (Option<String>, Option<Span>) {
        (self.err_decl.clone(), self.err_expr.clone())
    }

    pub fn set_stage(&mut self, stage: EvalStage) {
        self.stage = stage;
    }

    pub fn add_bindings(&mut self, resources: impl IntoIterator<Item = ((u32, u32), RefInstance)>) {
        for ((group, binding), inst) in resources.into_iter() {
            self.add_binding(group, binding, inst);
        }
    }
    pub fn add_binding(&mut self, group: u32, binding: u32, inst: RefInstance) {
        self.resources.insert((group, binding), inst);
    }
    pub fn resource(&self, group: u32, binding: u32) -> Option<&RefInstance> {
        self.resources.get(&(group, binding))
    }
    pub fn add_overrides(&mut self, overrides: impl IntoIterator<Item = (String, Instance)>) {
        self.overrides.extend(overrides);
    }
    pub fn add_overridable(&mut self, name: String, inst: Instance) {
        self.overrides.insert(name, inst);
    }
    pub fn overridable(&self, name: &str) -> Option<&Instance> {
        self.overrides.get(name)
    }
}

pub trait SyntaxUtil {
    /// find a global declaration by name.
    fn decl(&self, name: &str) -> Option<&GlobalDeclaration>;

    /// find a variable/value declaration by name.
    fn decl_decl(&self, name: &str) -> Option<&Declaration>;

    /// find a type alias declaration by name.
    fn decl_alias(&self, name: &str) -> Option<&TypeAlias>;

    /// find a struct declaration by name.
    ///
    /// see also: [`Self::resolve_alias`] to resolve the name before calling this function.
    fn decl_struct(&self, name: &str) -> Option<&Struct>;

    /// find a function declaration by name.
    fn decl_function(&self, name: &str) -> Option<&Function>;

    /// resolve an alias name.
    fn resolve_alias(&self, name: &str) -> Option<&TypeExpression>;

    /// resolve an aliases in a type expression.
    fn resolve_ty<'a>(&'a self, ty: &'a TypeExpression) -> &'a TypeExpression;
}

impl SyntaxUtil for TranslationUnit {
    fn decl(&self, name: &str) -> Option<&GlobalDeclaration> {
        self.global_declarations
            .iter()
            .chain(PRELUDE.global_declarations.iter())
            .find(|d| match d {
                GlobalDeclaration::Declaration(d) => &*d.ident.name() == name,
                GlobalDeclaration::TypeAlias(d) => &*d.ident.name() == name,
                GlobalDeclaration::Struct(d) => &*d.ident.name() == name,
                GlobalDeclaration::Function(d) => &*d.ident.name() == name,
                _ => false,
            })
    }
    fn decl_decl(&self, name: &str) -> Option<&Declaration> {
        match self.decl(name) {
            Some(GlobalDeclaration::Declaration(s)) => Some(s),
            _ => None,
        }
    }
    fn decl_alias(&self, name: &str) -> Option<&TypeAlias> {
        match self.decl(name) {
            Some(GlobalDeclaration::TypeAlias(s)) => Some(s),
            _ => None,
        }
    }
    fn decl_struct(&self, name: &str) -> Option<&Struct> {
        match self.decl(name) {
            Some(GlobalDeclaration::Struct(s)) => Some(s),
            _ => None,
        }
    }
    fn decl_function(&self, name: &str) -> Option<&Function> {
        match self.decl(name) {
            Some(GlobalDeclaration::Function(f)) => Some(f),
            _ => None,
        }
    }

    fn resolve_alias(&self, name: &str) -> Option<&TypeExpression> {
        match self.decl(name) {
            Some(GlobalDeclaration::TypeAlias(t)) => {
                if t.ty.template_args.is_none() {
                    self.resolve_alias(&t.ty.ident.name()).or(Some(&t.ty))
                } else {
                    Some(&t.ty)
                }
            }
            _ => None,
        }
    }

    fn resolve_ty<'a>(&'a self, ty: &'a TypeExpression) -> &'a TypeExpression {
        if ty.template_args.is_none() {
            self.resolve_alias(&*ty.ident.name()).unwrap_or(ty)
        } else {
            ty
        }
    }
}
