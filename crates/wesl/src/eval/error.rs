use itertools::Itertools;
use thiserror::Error;
use wgsl_parse::syntax::*;

use super::{Flow, Instance, LiteralInstance, MemView, Ty, Type};

#[derive(Clone, Debug, Error)]
pub enum EvalError {
    #[error("not implemented: `{0}`")]
    NotImpl(String),
    #[error("expected type `{0}`, got `{1}`")]
    Type(Type, Type),
    #[error("unknown type or variable `{0}`")]
    UnknownType(String),
    #[error("unknown function `{0}`")]
    UnknownFunction(String),
    #[error("no declaration named `{0}` in scope")]
    NoDecl(String),
    #[error("`{0}` is not constructible")]
    NotConstructible(Type),
    #[error("expected a scalar type, got `{0}`")]
    NotScalar(Type),

    // references
    #[error("invalid reference to memory view `{0}{1}`")]
    View(Type, MemView),
    #[error("invalid reference to `{0}`, expected reference to `{1}`")]
    RefType(Type, Type),
    #[error("cannot write a `{0}` to a reference to `{1}`")]
    WriteRefType(Type, Type),
    #[error("attempt to write to a read-only reference")]
    NotWrite,
    #[error("attempt to read a write-only reference")]
    NotRead,
    #[error("reference is not read-write")]
    NotReadWrite,

    // conversions
    #[error("cannot convert from `{0}` to `{1}`")]
    ConversionFailure(Type, Type),
    #[error("overflow while converting `{0}` to `{1}`")]
    ConvOverflow(LiteralInstance, Type),

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
    #[error(
        "invalid function call signature: `{0}({})`",
        (.1).iter().map(Ty::ty).format(", "),
    )]
    Signature(TypeExpression, Vec<Instance>),
    #[error("{0}")]
    Builtin(&'static str),
    #[error("invalid template arguments to `{0}`")]
    TemplateArgs(&'static str),
    #[error("type `{0}` does not take any template arguments")]
    UnexpectedTemplate(String),
    #[error("missing template arguments for type `{0}`")]
    MissingTemplate(&'static str),
    #[error("incorrect number of arguments to `{0}`, expected `{1}`, got `{2}`")]
    ParamCount(String, usize, usize),
    #[error("invalid parameter type, expected `{0}`, got `{1}`")]
    ParamType(Type, Type),
    #[error("returned a `{0}` from a function that returns `{1}`")]
    ReturnType(Type, Type),
    #[error("calling non-const function `{0}` in const context")]
    NotConst(String),

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
    ConstAssertFailure(ExpressionNode),
    #[error("a function body cannot contain a `{0}` statement")]
    FlowInFunction(Flow),
    #[error("a global declaration cannot contain a `{0}` statement")]
    FlowInModule(Flow),
}
