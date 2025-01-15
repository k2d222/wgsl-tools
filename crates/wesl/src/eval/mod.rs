mod attrs;
mod builtin;
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
use std::collections::HashMap;
use wgsl_parse::{span::Span, syntax::*};

#[derive(Clone, Debug)]
pub struct Scope {
    stack: Vec<HashMap<Ident, Instance>>,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            stack: vec![Default::default()],
        }
    }

    pub fn push(&mut self) {
        self.stack.push(Default::default())
    }

    pub fn pop(&mut self) {
        self.stack.pop().expect("failed to pop scope");
    }

    pub fn add_val(&mut self, ident: Ident, value: Instance) {
        if self
            .stack
            .last_mut()
            .unwrap()
            .insert(ident, value)
            .is_some()
        {
            panic!("duplicate variable insertion")
        }
    }

    pub fn add_var(&mut self, ident: Ident, inst: RefInstance) {
        let value = Instance::from(inst);
        if self
            .stack
            .last_mut()
            .unwrap()
            .insert(ident, value)
            .is_some()
        {
            panic!("duplicate variable insertion")
        }
    }

    pub fn get(&self, ident: &Ident) -> Option<Instance> {
        self.stack
            .iter()
            .rev()
            .find_map(|scope| scope.get(ident).cloned())
    }

    pub fn has(&self, ident: &Ident) -> bool {
        self.stack.last().unwrap().contains_key(ident)
    }
}

impl Default for Scope {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Display)]
pub enum ScopeKind {
    #[display("module")]
    Module,
    #[display("function")]
    Function,
}

pub struct ScopeGuard<'a> {
    scope: &'a mut Scope,
}

impl<'a> Drop for ScopeGuard<'a> {
    fn drop(&mut self) {
        self.scope.pop();
    }
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

pub struct Context<'s> {
    pub(crate) source: &'s TranslationUnit,
    pub(crate) scope: Scope,
    pub(crate) resources: HashMap<(u32, u32), RefInstance>,
    pub(crate) overrides: HashMap<String, Instance>,
    pub(crate) kind: ScopeKind,
    pub(crate) stage: EvalStage,
    err_decl: Option<Ident>,
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
        &self.source
    }

    pub fn scope_guard(&mut self) -> ScopeGuard {
        ScopeGuard {
            scope: &mut self.scope,
        }
    }

    fn set_err_decl_ctx(&mut self, decl: Ident) {
        if self.err_decl.is_none() {
            self.err_decl = Some(decl)
        }
    }
    fn set_err_expr_ctx(&mut self, expr: &Span) {
        if self.err_expr.is_none() {
            self.err_expr = Some(expr.clone())
        }
    }

    pub fn err_ctx(&self) -> (Option<Ident>, Option<Span>) {
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
        self.overrides.extend(overrides.into_iter());
    }
    pub fn add_overridable(&mut self, name: String, inst: Instance) {
        self.overrides.insert(name, inst);
    }
    pub fn overridable(&self, name: &str) -> Option<&Instance> {
        self.overrides.get(name)
    }
}

pub trait SyntaxUtil {
    /// find a global declaration by ident.
    fn decl(&self, ident: &Ident) -> Option<&GlobalDeclaration>;

    /// find a struct declaration by ident.
    ///
    /// see also: [`Self::resolve_alias`] to resolve the name before calling this function.
    fn decl_struct(&self, ident: &Ident) -> Option<&Struct>;

    /// find a function declaration by ident.
    fn decl_function(&self, ident: &Ident) -> Option<&Function>;

    /// resolve an alias ident.
    fn resolve_alias(&self, ident: &Ident) -> Option<TypeExpression>;
}

impl SyntaxUtil for TranslationUnit {
    fn decl(&self, ident: &Ident) -> Option<&GlobalDeclaration> {
        self.global_declarations
            .iter()
            .chain(PRELUDE.global_declarations.iter())
            .find(|d| match d {
                GlobalDeclaration::Declaration(d) => &d.ident == ident,
                GlobalDeclaration::TypeAlias(d) => &d.ident == ident,
                GlobalDeclaration::Struct(d) => &d.ident == ident,
                GlobalDeclaration::Function(d) => &d.ident == ident,
                _ => false,
            })
    }
    fn decl_struct(&self, ident: &Ident) -> Option<&Struct> {
        match self.decl(ident) {
            Some(GlobalDeclaration::Struct(s)) => Some(s),
            _ => None,
        }
    }

    fn decl_function(&self, ident: &Ident) -> Option<&Function> {
        match self.decl(ident) {
            Some(GlobalDeclaration::Function(f)) => Some(f),
            _ => None,
        }
    }

    // TODO return borrowed
    fn resolve_alias(&self, ident: &Ident) -> Option<TypeExpression> {
        match self.decl(ident) {
            Some(GlobalDeclaration::TypeAlias(t)) => {
                if t.ty.template_args.is_none() {
                    self.resolve_alias(&t.ty.ident).or(Some(t.ty.clone()))
                } else {
                    Some(t.ty.clone())
                }
            }
            _ => None,
        }
    }
}
