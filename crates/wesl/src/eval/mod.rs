mod builtin;
mod conv;
mod display;
mod error;
mod eval;
mod exec;
mod instance;
mod ops;
mod ty;

pub use builtin::*;
pub use conv::*;
pub use error::*;
pub use eval::*;
pub use exec::*;
pub use instance::*;
pub use ty::*;

use std::{cell::RefCell, collections::HashMap, rc::Rc};

use wgsl_parse::{span::Span, syntax::*};

#[derive(Clone, Debug)]
pub struct Scope {
    stack: Vec<HashMap<String, RefInstance>>,
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

    pub fn add(&mut self, name: String, value: Instance, access: AccessMode) {
        let r = RefInstance::new(Rc::new(RefCell::new(value)), access);
        if self.stack.last_mut().unwrap().insert(name, r).is_some() {
            panic!("duplicate variable insertion")
        }
    }

    pub fn get(&self, name: &str) -> Option<RefInstance> {
        self.stack
            .iter()
            .rev()
            .find_map(|scope| scope.get(name).cloned())
    }

    pub fn has(&self, name: &str) -> bool {
        self.stack.last().unwrap().contains_key(name)
    }
}

impl Default for Scope {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ScopeKind {
    Module,
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

pub struct Context<'s> {
    source: &'s TranslationUnit,
    scope: Scope,
    kind: ScopeKind,
    stage: EvalStage,
    err_decl: Option<String>,
    err_expr: Option<Span>,
}

impl<'s> Context<'s> {
    pub fn new(source: &'s TranslationUnit) -> Self {
        Self {
            source,
            scope: Default::default(),
            kind: ScopeKind::Function,
            stage: EvalStage::Const,
            err_expr: None,
            err_decl: None,
        }
    }

    pub fn scope_guard(&mut self) -> ScopeGuard {
        ScopeGuard {
            scope: &mut self.scope,
        }
    }

    fn set_err_decl_ctx(&mut self, decl: &str) {
        if self.err_decl.is_none() {
            self.err_decl = Some(decl.to_string())
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
}

pub trait SyntaxUtil {
    /// find a global declaration by name.
    fn decl(&self, name: &str) -> Option<&GlobalDeclaration>;

    /// find a struct declaration by name.
    ///
    /// see also: [`resolve_alias`] to resolve the name before calling this function.
    fn decl_struct(&self, name: &str) -> Option<&Struct>;

    /// find a function declaration by name.
    fn decl_function(&self, name: &str) -> Option<&Function>;

    /// resolve an alias name.
    fn resolve_alias(&self, name: &str) -> Option<TypeExpression>;
}

impl SyntaxUtil for TranslationUnit {
    fn decl(&self, name: &str) -> Option<&GlobalDeclaration> {
        self.global_declarations
            .iter()
            .chain(PRELUDE.global_declarations.iter())
            .find(|d| match d {
                GlobalDeclaration::Declaration(d) => &d.name == name,
                GlobalDeclaration::TypeAlias(d) => &d.name == name,
                GlobalDeclaration::Struct(d) => &d.name == name,
                GlobalDeclaration::Function(d) => &d.name == name,
                _ => false,
            })
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

    // TODO return borrowed
    fn resolve_alias(&self, name: &str) -> Option<TypeExpression> {
        match self.decl(name) {
            Some(GlobalDeclaration::TypeAlias(t)) => {
                if t.ty.template_args.is_none() {
                    self.resolve_alias(&t.name).or(Some(t.ty.clone()))
                } else {
                    Some(t.ty.clone())
                }
            }
            _ => None,
        }
    }
}