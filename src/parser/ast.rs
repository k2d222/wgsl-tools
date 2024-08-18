use std::str::FromStr;

// follwing the spec at this date: https://www.w3.org/TR/2024/WD-WGSL-20240731/
use super::lexer::Span;
use super::Error;

// spanned
#[derive(Clone, Debug)]
pub struct Spanned<T>(pub T, pub Span);

impl<T> From<Spanned<T>> for Spanned<Box<T>> {
    fn from(value: Spanned<T>) -> Self {
        Spanned(value.0.into(), value.1)
    }
}

type S<T> = Spanned<T>;

#[derive(Clone, Debug)]
#[allow(unused)]
pub struct TranslationUnit {
    pub global_directives: Vec<S<GlobalDirective>>,
    pub global_declarations: Vec<S<GlobalDeclaration>>,
}

#[derive(Clone, Debug)]
#[allow(unused)]
pub enum GlobalDirective {
    Diagnostic(DiagnosticDirective),
    Enable(EnableDirective),
    Requires(RequiresDirective),
}

#[derive(Clone, Debug)]
#[allow(unused)]
pub struct DiagnosticDirective {
    pub severity: DiagnosticSeverity,
    pub rule_name: Span,
}

#[derive(Clone, Debug)]
#[allow(unused)]
pub enum DiagnosticSeverity {
    Error,
    Warning,
    Info,
    Off,
}

#[derive(Clone, Debug)]
#[allow(unused)]
pub struct EnableDirective {
    pub extensions: Vec<Span>,
}

#[derive(Clone, Debug)]
#[allow(unused)]
pub struct RequiresDirective {
    pub extensions: Vec<Span>,
}

#[derive(Clone, Debug)]
#[allow(unused)]
pub enum GlobalDeclaration {
    Void,
    Variable(S<Declaration>),
    Value(S<Declaration>),
    TypeAlias(S<TypeAlias>),
    Struct(S<Struct>),
    Function(S<Function>),
    ConstAssert(S<Expression>),
}

#[derive(Clone, Debug)]
#[allow(unused)]
pub struct TypeAlias {
    pub name: Span,
    pub typ: S<TypeExpression>,
}

#[derive(Clone, Debug)]
#[allow(unused)]
pub struct Struct {
    pub name: Span,
    pub members: Vec<S<StructMember>>,
}

#[derive(Clone, Debug)]
#[allow(unused)]
pub struct StructMember {
    pub attributes: Vec<S<Attribute>>,
    pub name: Span,
    pub typ: S<TypeExpression>,
}

#[derive(Clone, Debug)]
#[allow(unused)]
pub struct Function {
    pub attributes: Vec<S<Attribute>>,
    pub name: Span,
    pub parameters: Vec<FormalParameter>,
    pub return_attributes: Vec<S<Attribute>>,
    pub return_type: Option<TypeExpression>,
    pub body: CompoundStatement,
}

#[derive(Clone, Debug)]
#[allow(unused)]
pub struct FormalParameter {
    pub attributes: Vec<S<Attribute>>,
    pub name: Span,
    pub typ: S<TypeExpression>,
}

// TODO incomplete
pub type Attribute = ();

#[derive(Clone, Debug)]
#[allow(unused)]
pub enum Expression {
    Literal(Literal),
    Parenthesized(Box<Expression>),
    NamedComponent(NamedComponentExpression),
    Indexing(IndexingExpression),
    Unary(UnaryExpression),
    Binary(BinaryExpression),
    FunctionCall(FunctionCallExpression),
    Identifier(Span),
    Type(TypeExpression),
}

#[derive(Clone, Debug)]
#[allow(unused)]
pub enum Literal {
    True,
    False,
    AbstractInt(i32),
    AbstractFloat(f32),
    I32(i32),
    U32(u32),
    F32(f32),
    F16(f32),
}

#[derive(Clone, Debug)]
#[allow(unused)]
pub struct NamedComponentExpression {
    pub base: S<Box<Expression>>,
    pub component: Span,
}

#[derive(Clone, Debug)]
#[allow(unused)]
pub struct IndexingExpression {
    pub base: S<Box<Expression>>,
    pub index: S<Box<Expression>>,
}

#[derive(Clone, Debug)]
#[allow(unused)]
pub struct UnaryExpression {
    pub operator: UnaryOperator,
    pub operand: S<Box<Expression>>,
}

#[derive(Clone, Debug)]
#[allow(unused)]
pub enum UnaryOperator {
    LogicalNegation,
    Negation,
    BitwiseComplement,
    AddressOf,
    Indirection,
}

#[derive(Clone, Debug)]
#[allow(unused)]
pub struct BinaryExpression {
    pub operator: BinaryOperator,
    pub left: S<Box<Expression>>,
    pub right: S<Box<Expression>>,
}

#[derive(Clone, Debug)]
#[allow(unused)]
pub enum BinaryOperator {
    ShortCircuitOr,
    ShortCircuitAnd,
    LogicalOr,
    LogicalAnd,
    Addition,
    Subtraction,
    Multiplication,
    Division,
    Remainder,
    Equality,
    Inequality,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
    BitwiseOr,
    BitwiseAnd,
    BitwiseXor,
    ShiftLeft,
    ShiftRight,
}

#[derive(Clone, Debug)]
#[allow(unused)]
pub struct FunctionCallExpression {
    pub name: Span,
    pub template_args: Option<Vec<S<TemplateArg>>>,
    pub arguments: Vec<S<Expression>>,
}

#[derive(Clone, Debug)]
#[allow(unused)]
pub struct TypeExpression {
    pub name: Span,
    pub template_args: Option<Vec<S<TemplateArg>>>,
}

// TODO
pub type TemplateArg = Expression;

#[derive(Clone, Debug)]
#[allow(unused)]
pub enum Statement {
    Void,
    Compound(CompoundStatement),
    Assignment(AssignmentStatement),
    Increment(S<Box<Expression>>),
    Decrement(S<Box<Expression>>),
    If(IfStatement),
    Switch(SwitchStatement),
    Loop(LoopStatement),
    For(ForStatement),
    While(WhileStatement),
    Break,
    Continue,
    Return(Option<S<Expression>>),
    Discard,
    FunctionCall(FunctionCallExpression),
    ConstAssert(S<Expression>),
    Declaration(Declaration),
}

#[derive(Clone, Debug)]
#[allow(unused)]
pub struct CompoundStatement {
    pub attributes: Vec<S<Attribute>>,
    pub statements: Vec<S<Statement>>,
}

#[derive(Clone, Debug)]
#[allow(unused)]
pub struct AssignmentStatement {
    pub operator: AssignmentOperator,
    pub lhs: S<Box<Expression>>,
    pub rhs: S<Box<Expression>>,
}

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
#[allow(unused)]
pub struct IfStatement {
    pub attributes: Vec<S<Attribute>>,
    pub if_clause: (S<Expression>, S<CompoundStatement>),
    pub else_if_clauses: Vec<(S<Expression>, S<CompoundStatement>)>,
    pub else_clause: Option<S<CompoundStatement>>,
}

#[derive(Clone, Debug)]
#[allow(unused)]
pub struct SwitchStatement {
    pub attributes: Vec<S<Attribute>>,
    pub body_attributes: Vec<S<Attribute>>,
    pub expression: S<Box<Expression>>,
    pub clauses: Vec<S<SwitchClause>>,
}

#[derive(Clone, Debug)]
#[allow(unused)]
pub struct SwitchClause {
    pub case_selectors: Vec<S<CaseSelector>>,
    pub statement: S<CompoundStatement>,
}

#[derive(Clone, Debug)]
#[allow(unused)]
pub enum CaseSelector {
    Default,
    Expression(S<Expression>),
}

#[derive(Clone, Debug)]
#[allow(unused)]
pub struct LoopStatement {
    pub attributes: Vec<S<Attribute>>,
    pub body_attributes: Vec<S<Attribute>>,
    pub body: Vec<S<Statement>>,
    pub continuing: Option<S<ContinuingStatement>>,
}

#[derive(Clone, Debug)]
#[allow(unused)]
pub struct ContinuingStatement {
    pub attributes: Vec<S<Attribute>>,
    pub statements: Vec<S<Statement>>,
    pub break_if: Option<S<Expression>>,
}

#[derive(Clone, Debug)]
#[allow(unused)]
pub struct ForStatement {
    pub attributes: Vec<S<Attribute>>,
    pub initializer: Option<S<Box<Statement>>>,
    pub condition: Option<S<Expression>>,
    pub update: Option<S<Box<Statement>>>,
    pub body: CompoundStatement,
}

#[derive(Clone, Debug)]
#[allow(unused)]
pub struct WhileStatement {
    pub attributes: Vec<S<Attribute>>,
    pub condition: S<Expression>,
    pub body: S<CompoundStatement>,
}

#[derive(Clone, Debug)]
#[allow(unused)]
pub struct Declaration {
    pub attributes: Vec<S<Attribute>>,
    pub kind: DeclarationKind,
    pub template_args: Option<Vec<S<TemplateArg>>>,
    pub name: Span,
    pub typ: Option<TypeExpression>,
    pub initializer: Option<S<Box<Expression>>>,
}

#[derive(Clone, Debug)]
#[allow(unused)]
pub enum DeclarationKind {
    Const,
    Override,
    Let,
    Var,
}
