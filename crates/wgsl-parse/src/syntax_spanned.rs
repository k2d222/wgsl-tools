//! A spanned syntax tree for WGSL files. The root of the tree is a [TranslationUnit].
//!
//! Follwing the spec at this date:
//! [2024-07-31](https://www.w3.org/TR/2024/WD-WGSL-20240731/).
//! The syntax tree closely mirrors wgsl structure while allowing language extensions.
//!
//! ## Spanned
//!
//! The following elements are Spanned to allow easy modification of the source code:
//! Directives, Declarations, Statements, Expressions, Struct Members, Attributes, Idents,
//! Template Arguments, Formal Parameters.
//! ... plus all language extensions, and maybe others.
//!
//! Spans, if provided, are outer spans, meaning e.g. Statements include the `;` and
//! Struct Members include the `,`, but do not include the spaces between neighbor items.
//! Use the convenience methods to manipulate the spans as needed (TODO).

use std::ops::Deref;

use crate::span::{Span, S};
pub use crate::syntax::{
    BinaryOperator, DeclarationKind, DiagnosticSeverity, LiteralExpression, UnaryOperator,
};

pub struct WithSource<'s, T> {
    syntax: T,
    source: &'s str,
}

impl<'s, T> WithSource<'s, T> {
    fn new(syntax: T, source: &'s str) -> Self {
        Self { syntax, source }
    }
    pub fn source(&self) -> &str {
        self.source
    }
}

impl<'s, T> Deref for WithSource<'s, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.syntax
    }
}

impl<'s, T> AsRef<T> for WithSource<'s, T> {
    fn as_ref(&self) -> &T {
        &self.syntax
    }
}

pub trait SpannedSyntax {
    fn with_source<'s>(&self, source: &'s str) -> WithSource<'s, &Self> {
        WithSource::new(self, source)
    }
}

impl SpannedSyntax for TranslationUnit {}
impl SpannedSyntax for GlobalDirective {}
impl SpannedSyntax for DiagnosticDirective {}
impl SpannedSyntax for EnableDirective {}
impl SpannedSyntax for RequiresDirective {}
impl SpannedSyntax for GlobalDeclaration {}
impl SpannedSyntax for Declaration {}
impl SpannedSyntax for TypeAlias {}
impl SpannedSyntax for Struct {}
impl SpannedSyntax for StructMember {}
impl SpannedSyntax for Function {}
impl SpannedSyntax for FormalParameter {}
impl SpannedSyntax for ConstAssert {}
impl SpannedSyntax for Attribute {}
impl SpannedSyntax for Expression {}
impl SpannedSyntax for NamedComponentExpression {}
impl SpannedSyntax for IndexingExpression {}
impl SpannedSyntax for UnaryExpression {}
impl SpannedSyntax for BinaryExpression {}
impl SpannedSyntax for FunctionCallExpression {}
impl SpannedSyntax for TypeExpression {}
impl SpannedSyntax for Statement {}
impl SpannedSyntax for CompoundStatement {}
impl SpannedSyntax for AssignmentStatement {}
impl SpannedSyntax for AssignmentOperator {}
impl SpannedSyntax for IfStatement {}
impl SpannedSyntax for SwitchStatement {}
impl SpannedSyntax for SwitchClause {}
impl SpannedSyntax for CaseSelector {}
impl SpannedSyntax for LoopStatement {}
impl SpannedSyntax for ContinuingStatement {}
impl SpannedSyntax for ForStatement {}
impl SpannedSyntax for WhileStatement {}
impl SpannedSyntax for IdentifierExpression {}

#[derive(Clone, Debug, PartialEq)]
#[allow(unused)]
pub struct TranslationUnit {
    pub global_directives: Vec<S<GlobalDirective>>,
    pub global_declarations: Vec<S<GlobalDeclaration>>,
}

#[derive(Clone, Debug, PartialEq)]
#[allow(unused)]
pub enum GlobalDirective {
    Diagnostic(DiagnosticDirective),
    Enable(EnableDirective),
    Requires(RequiresDirective),
}

#[derive(Clone, Debug, PartialEq)]
#[allow(unused)]
pub struct DiagnosticDirective {
    pub severity: DiagnosticSeverity,
    pub rule_name: Span,
}

// #[derive(Clone, Debug, PartialEq)]
// #[allow(unused)]
// pub enum DiagnosticSeverity {
//     Error,
//     Warning,
//     Info,
//     Off,
// }

#[derive(Clone, Debug, PartialEq)]
#[allow(unused)]
pub struct EnableDirective {
    pub extensions: Vec<Span>,
}

#[derive(Clone, Debug, PartialEq)]
#[allow(unused)]
pub struct RequiresDirective {
    pub extensions: Vec<Span>,
}

#[derive(Clone, Debug, PartialEq)]
#[allow(unused)]
pub enum GlobalDeclaration {
    Void,
    Declaration(Declaration),
    TypeAlias(TypeAlias),
    Struct(Struct),
    Function(Function),
    ConstAssert(ConstAssert),
}

#[derive(Clone, Debug, PartialEq)]
#[allow(unused)]
pub struct Declaration {
    pub attributes: Vec<S<Attribute>>,
    pub kind: DeclarationKind,
    pub template_args: Option<Vec<S<TemplateArg>>>,
    pub name: Span,
    pub typ: Option<TypeExpression>,
    pub initializer: Option<S<Expression>>,
}

// #[derive(Clone, Debug, PartialEq)]
// #[allow(unused)]
// pub enum DeclarationKind {
//     Const,
//     Override,
//     Let,
//     Var,
// }

#[derive(Clone, Debug, PartialEq)]
#[allow(unused)]
pub struct TypeAlias {
    pub name: Span,
    pub typ: TypeExpression,
}

#[derive(Clone, Debug, PartialEq)]
#[allow(unused)]
pub struct Struct {
    pub name: Span,
    pub members: Vec<S<StructMember>>,
}

#[derive(Clone, Debug, PartialEq)]
#[allow(unused)]
pub struct StructMember {
    pub attributes: Vec<S<Attribute>>,
    pub name: Span,
    pub typ: TypeExpression,
}

#[derive(Clone, Debug, PartialEq)]
#[allow(unused)]
pub struct Function {
    pub attributes: Vec<S<Attribute>>,
    pub name: Span,
    pub parameters: Vec<S<FormalParameter>>,
    pub return_attributes: Vec<S<Attribute>>,
    pub return_type: Option<TypeExpression>,
    pub body: CompoundStatement,
}

#[derive(Clone, Debug, PartialEq)]
#[allow(unused)]
pub struct FormalParameter {
    pub attributes: Vec<S<Attribute>>,
    pub name: Span,
    pub typ: TypeExpression,
}

#[derive(Clone, Debug, PartialEq)]
#[allow(unused)]
pub struct ConstAssert {
    pub expression: S<Expression>,
}

#[derive(Clone, Debug, PartialEq)]
#[allow(unused)]
pub struct Attribute {
    pub name: Span,
    pub arguments: Option<Vec<S<Expression>>>,
}

#[derive(Clone, Debug, PartialEq)]
#[allow(unused)]
pub enum Expression {
    Literal(LiteralExpression),
    Parenthesized(ParenthesizedExpression),
    NamedComponent(NamedComponentExpression),
    Indexing(IndexingExpression),
    Unary(UnaryExpression),
    Binary(BinaryExpression),
    FunctionCall(FunctionCallExpression),
    Identifier(IdentifierExpression),
    Type(TypeExpression),
}

// #[derive(Clone, Debug, PartialEq)]
// #[allow(unused)]
// pub enum LiteralExpression {
//     True,
//     False,
//     AbstractInt(i64),
//     AbstractFloat(f64),
//     I32(i32),
//     U32(u32),
//     F32(f32),
//     F16(f32),
// }

pub type ParenthesizedExpression = Box<Expression>;

#[derive(Clone, Debug, PartialEq)]
#[allow(unused)]
pub struct NamedComponentExpression {
    pub base: Box<S<Expression>>,
    pub component: Span,
}

#[derive(Clone, Debug, PartialEq)]
#[allow(unused)]
pub struct IndexingExpression {
    pub base: S<Box<Expression>>,
    pub index: S<Box<Expression>>,
}

#[derive(Clone, Debug, PartialEq)]
#[allow(unused)]
pub struct UnaryExpression {
    pub operator: UnaryOperator,
    pub operand: S<Box<Expression>>, // TODO maybe rename rhs
}

// #[derive(Clone, Debug, PartialEq)]
// #[allow(unused)]
// pub enum UnaryOperator {
//     LogicalNegation,
//     Negation,
//     BitwiseComplement,
//     AddressOf,
//     Indirection,
// }

#[derive(Clone, Debug, PartialEq)]
#[allow(unused)]
pub struct BinaryExpression {
    pub operator: BinaryOperator,
    pub left: S<Box<Expression>>, // TODO: rename lhs rhs
    pub right: S<Box<Expression>>,
}

// #[derive(Clone, Debug, PartialEq)]
// #[allow(unused)]
// pub enum BinaryOperator {
//     ShortCircuitOr,
//     ShortCircuitAnd,
//     Addition,
//     Subtraction,
//     Multiplication,
//     Division,
//     Remainder,
//     Equality,
//     Inequality,
//     LessThan,
//     LessThanEqual,
//     GreaterThan,
//     GreaterThanEqual,
//     BitwiseOr,
//     BitwiseAnd,
//     BitwiseXor,
//     ShiftLeft,
//     ShiftRight,
// }

#[derive(Clone, Debug, PartialEq)]
#[allow(unused)]
pub struct FunctionCallExpression {
    pub name: Span,
    pub template_args: Option<Vec<S<TemplateArg>>>,
    pub arguments: Vec<S<Expression>>,
}

pub type IdentifierExpression = Span;

#[derive(Clone, Debug, PartialEq)]
#[allow(unused)]
pub struct TypeExpression {
    pub name: Span,
    pub template_args: Option<Vec<S<TemplateArg>>>,
}

// TODO
pub type TemplateArg = Expression;

#[derive(Clone, Debug, PartialEq)]
#[allow(unused)]
pub enum Statement {
    Void,
    Compound(CompoundStatement),
    Assignment(AssignmentStatement),
    Increment(IncrementStatement),
    Decrement(DecrementStatement),
    If(IfStatement),
    Switch(SwitchStatement),
    Loop(LoopStatement),
    For(ForStatement),
    While(WhileStatement),
    Break,
    Continue,
    Return(ReturnStatement),
    Discard,
    FunctionCall(FunctionCallStatement),
    ConstAssert(ConstAssertStatement),
    Declaration(DeclarationStatement),
}

#[derive(Clone, Debug, PartialEq)]
#[allow(unused)]
pub struct CompoundStatement {
    pub attributes: Vec<S<Attribute>>,
    pub statements: Vec<S<Statement>>,
}

#[derive(Clone, Debug, PartialEq)]
#[allow(unused)]
pub struct AssignmentStatement {
    pub operator: AssignmentOperator,
    pub lhs: S<Expression>,
    pub rhs: S<Expression>,
}

#[derive(Clone, Debug, PartialEq)]
#[allow(unused)]
pub enum AssignmentOperator {
    Equal,
    PlusEqual,
    MinusEqual,
    TimesEqual,
    DivisionEqual,
    ModuloEqual,
    AndEqual,
    OrEqual,
    XorEqual,
    ShiftRightAssign,
    ShiftLeftAssign,
}

pub type IncrementStatement = S<Expression>;

pub type DecrementStatement = S<Expression>;

#[derive(Clone, Debug, PartialEq)]
#[allow(unused)]
pub struct IfStatement {
    pub attributes: Vec<S<Attribute>>,
    pub if_clause: (S<Expression>, CompoundStatement),
    pub else_if_clauses: Vec<(S<Expression>, CompoundStatement)>,
    pub else_clause: Option<CompoundStatement>,
}

#[derive(Clone, Debug, PartialEq)]
#[allow(unused)]
pub struct SwitchStatement {
    pub attributes: Vec<S<Attribute>>,
    pub expression: S<Expression>,
    pub body_attributes: Vec<S<Attribute>>,
    pub clauses: Vec<SwitchClause>,
}

#[derive(Clone, Debug, PartialEq)]
#[allow(unused)]
pub struct SwitchClause {
    pub case_selectors: Vec<CaseSelector>,
    pub body: CompoundStatement,
}

#[derive(Clone, Debug, PartialEq)]
#[allow(unused)]
pub enum CaseSelector {
    Default,
    Expression(S<Expression>),
}

#[derive(Clone, Debug, PartialEq)]
#[allow(unused)]
pub struct LoopStatement {
    pub attributes: Vec<S<Attribute>>,
    pub body: CompoundStatement,
    // a ContinuingStatement can only appear inside a LoopStatement body, therefore it is
    // not part of the Statement enum. it appear shere instead, but consider it part of
    // body as the last statement of the CompoundStatement.
    pub continuing: Option<S<ContinuingStatement>>,
}

#[derive(Clone, Debug, PartialEq)]
#[allow(unused)]
pub struct ContinuingStatement {
    pub body: CompoundStatement,
    // a BreakIfStatement can only appear inside a ContinuingStatement body, therefore it
    // not part of the Statement enum. it appear shere instead, but consider it part of
    // body as the last statement of the CompoundStatement.
    pub break_if: Option<S<BreakIfStatement>>,
}

pub type BreakIfStatement = Expression;

#[derive(Clone, Debug, PartialEq)]
#[allow(unused)]
pub struct ForStatement {
    pub attributes: Vec<S<Attribute>>,
    pub initializer: Option<S<Box<Statement>>>,
    pub condition: Option<S<Expression>>,
    pub update: Option<S<Box<Statement>>>,
    pub body: CompoundStatement,
}

#[derive(Clone, Debug, PartialEq)]
#[allow(unused)]
pub struct WhileStatement {
    pub attributes: Vec<S<Attribute>>,
    pub condition: S<Expression>,
    pub body: CompoundStatement,
}

pub type ReturnStatement = Option<S<Expression>>;

pub type FunctionCallStatement = FunctionCallExpression;

pub type ConstAssertStatement = ConstAssert;

pub type DeclarationStatement = Declaration;
