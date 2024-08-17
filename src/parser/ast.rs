// follwing the spec at this date: https://www.w3.org/TR/2024/WD-WGSL-20240731/
use super::lexer::Span;

// spanned
type S<T> = (T, Span);

#[derive(Clone, Debug)]
pub struct TranslationUnit {
    pub global_directives: Vec<S<GlobalDirective>>,
    pub global_declarations: Vec<S<GlobalDeclaration>>,
}

#[derive(Clone, Debug)]
pub enum GlobalDirective {
    Diagnostic,
    Enable,
    Requires,
}

#[derive(Clone, Debug)]
pub enum Severity {
    Error,
    Warning,
    Info,
    Off,
}

#[derive(Clone, Debug)]
pub enum GlobalDeclaration {
    Void,
    Variable,
    Value,
    TypeAlias,
    Struct,
    Function,
    ConstAssert,
}

#[derive(Clone, Debug)]
pub struct GlobalVariableDeclaration {
    attributes: Vec<S<Attribute>>,
    declaration: S<VariableDeclaration>,
    expression: Option<S<Expression>>,
}

// TODO incomplete
pub type Attribute = ();

#[derive(Clone, Debug)]
pub enum Expression {
    Relational,
    ShortCircuitOr,
    ShortCircuitAnd,
    Bitwise,
}

#[derive(Clone, Debug)]
pub struct VariableDeclaration {}
