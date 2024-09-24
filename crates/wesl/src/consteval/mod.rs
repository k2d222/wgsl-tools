mod builtin;
mod conv;
mod display;
mod eval;
mod exec;
mod instance;
mod ops;
mod ty;

pub use builtin::*;
pub use conv::*;
pub use eval::*;
pub use exec::*;
pub use instance::*;
pub use ty::*;

use std::collections::{HashMap, HashSet};

use itertools::Itertools;
use lazy_static::lazy_static;
use thiserror::Error;
use wgsl_parse::syntax::*;

#[derive(Clone, Debug, Error)]
pub enum ConstEvalError {
    #[error("not implemented: `{0}`")]
    NotImpl(String),
    #[error("invalid reference to memory location `{0}`")]
    Ref(Address),
    #[error("invalid reference to `{0}`, expected reference to `{1}`")]
    RefType(Type, Type),
    #[error("expected type `{0}`, got `{1}`")]
    Type(Type, Type),
    #[error("unknown type `{0}`")]
    UnknownType(TypeExpression),
    #[error("unknown function `{0}`")]
    UnknownFunction(String),
    #[error("no declaration named `{0}` in scope")]
    NoDecl(String),
    #[error("cannot convert from `{0}` to `{1}`")]
    ConversionFailure(Type, Type),

    // indexing
    #[error("`{0}` has no component `{1}`")]
    Component(Type, String),
    #[error("invalid array index type `{0}`")]
    Index(Type),
    #[error("`{0}` cannot be indexed")]
    NotIndexable(Type),
    #[error("invalid vector component or swizzle `{0}`")]
    Swizzle(String),
    #[error("index `{0}` is out-of-bounds for `{1}` of `{2}` components")]
    OutOfBounds(usize, Type, usize),

    // arithmetic
    #[error("cannot use unary operator `{0}` on type `{1}`")]
    Unary(UnaryOperator, Type),
    #[error("cannot use binary operator `{0}` with operands `{1}` and `{2}`")]
    Binary(BinaryOperator, Type, Type),
    #[error("cannot apply component-wise binary operation on operands `{0}` and `{1}`")]
    CompwiseBinary(Type, Type),
    #[error("attempt to negate with overflow")]
    NegOverflow,
    #[error("attempt to add with overflow")]
    AddOverflow,
    #[error("attempt to subtract with overflow")]
    SubOverflow,
    #[error("attempt to multiply with overflow")]
    MulOverflow,
    #[error("attempt to divide by zero")]
    DivByZero,
    #[error("attempt to calculate the remainder with a divisor of zero")]
    RemZeroDiv,
    #[error("attempt to shift left by `{0}`, which would overflow")]
    ShlOverflow(u32),
    #[error("attempt to shift right by `{0}`, which would overflow")]
    ShrOverflow(u32),

    // functions
    #[error("{0}")]
    Builtin(&'static str),
    #[error("invalid template arguments to `{0}`")]
    TemplateArgs(&'static str),
    #[error("type `{0}` does not take any template arguments")]
    UnexpectedTemplate(Type),
    #[error("missing template arguments for type `{0}`")]
    MissingTemplate(&'static str),
    #[error("invalid number of function call parameters, expected `{0}`, got `{1}`")]
    ParamCount(usize, usize),
    #[error("invalid parameter type, expected `{0}`, got `{1}`")]
    ParamType(Type, Type),
    #[error("invalid return type, expected `{0}`, got `{1}`")]
    ReturnType(Type, Type),

    // declarations
    #[error("override-declarations are not permitted in const contexts")]
    OverrideInConst,
    #[error("override-declarations are not permitted in function bodies")]
    OverrideInFn,
    #[error("let-declarations are not permitted at the module scope")]
    LetInMod,
    #[error("uninitialized const-declaration `{0}`")]
    UninitConst(String),
    #[error("uninitialized let-declaration `{0}`")]
    UninitLet(String),
    #[error("duplicate declaration of `{0}` in the current scope")]
    DuplicateDecl(String),
    #[error("a declaration must have an explicit type or an initializer")]
    UntypedDecl,

    // statements
    #[error("expected a reference, got value `{0}`")]
    NotRef(Instance),
    #[error("cannot assign a `{0}` to a `{1}`")]
    AssignType(Type, Type),
    #[error("cannot increment a `{0}`")]
    IncrType(Type),
    #[error("attempt to increment with overflow")]
    IncrOverflow,
    #[error("cannot decrement a `{0}`")]
    DecrType(Type),
    #[error("attempt to decrement with overflow")]
    DecrOverflow,
    #[error("a continuing body cannot contain a `{0}` statement")]
    FlowInContinuing(Flow),
    #[error("discard statements are not permitted in const contexts")]
    DiscardInConst,
    #[error("const assertion failed: `{0}` is `false`")]
    ConstAssertFailure(Expression),
    #[error("a function body cannot contain a `{}` statement")]
    FlowInFunction(Flow),
}

#[derive(Clone, Debug)]
pub struct Scope {
    stack: Vec<HashMap<String, usize>>,
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
        // TODO: garbage collect ctx memory
        self.stack.pop();
    }

    pub fn add(&mut self, name: String, ptr: usize) -> Option<usize> {
        self.stack.last_mut().unwrap().insert(name, ptr)
    }

    pub fn get(&self, name: &str) -> Option<usize> {
        self.stack
            .iter()
            .rev()
            .find_map(|scope| scope.get(name).copied())
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

pub struct Context<'s> {
    source: &'s TranslationUnit,
    scope: Scope,
    kind: ScopeKind,
    memory: Vec<Instance>,
}

impl<'s> Context<'s> {
    pub fn new(source: &'s TranslationUnit) -> Self {
        Self {
            source,
            scope: Default::default(),
            kind: ScopeKind::Function,
            memory: Default::default(),
        }
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
