//! A syntax tree for WGSL files. The root of the tree is a [`TranslationUnit`].
//!
//! Follwing the spec at this date:
//! [2024-07-31](https://www.w3.org/TR/2024/WD-WGSL-20240731/).
//! The syntax tree closely mirrors wgsl structure while allowing language extensions.
//!
//! ## Strictness
//!
//! This syntax tree is rather strict, meaning it cannot represent most syntaxically
//! incorrect programs. But it is only syntactic, meaning it doesn't perform many
//! contextual checks: for example, certain attributes can only appear in certain places,
//! or declarations have different constraints depending on where they appear.
//! stricter checking is TODO and will be optional.
//!
//! ## Extensions
//!
//! TODO, the syntax tree can be mutated to allow well-defined language extensions with
//! feature flags (wgsl-tooling-imports, wgsl-tooling-generics, ...).
//!
//! ## Design considerations
//!
//! The parsing is not designed to be primarily efficient, but flexible and correct.
//! It is made with the ultimate goal to implement spec-compliant language extensions.
//! This is why this parser doesn't borrow strings.

pub use crate::visit::*;

#[derive(Default, Clone, Debug, PartialEq, Visit)]
pub struct TranslationUnit {
    #[cfg(feature = "imports")]
    pub imports: Vec<Import>,
    pub global_directives: Vec<GlobalDirective>,
    pub global_declarations: Vec<GlobalDeclaration>,
}

#[cfg(feature = "imports")]
#[derive(Clone, Debug, PartialEq, Visit)]
pub struct Import {
    pub path: Vec<String>,
    pub content: ImportContent,
}

#[cfg(feature = "imports")]
#[derive(Clone, Debug, PartialEq, Visit)]
pub enum ImportContent {
    Star(ImportItem),
    Item(ImportItem),
    Collection(Vec<Import>),
}

#[cfg(feature = "imports")]
#[derive(Clone, Debug, PartialEq, Visit)]
pub struct ImportItem {
    pub name: String,
    pub rename: Option<String>,
}

#[derive(Clone, Debug, PartialEq, Visit)]
pub enum GlobalDirective {
    Diagnostic(DiagnosticDirective),
    Enable(EnableDirective),
    Requires(RequiresDirective),
}

#[derive(Clone, Debug, PartialEq, Visit)]
pub struct DiagnosticDirective {
    pub severity: DiagnosticSeverity,
    pub rule_name: String,
}

#[derive(Clone, Debug, PartialEq)]
pub enum DiagnosticSeverity {
    Error,
    Warning,
    Info,
    Off,
}

#[derive(Clone, Debug, PartialEq, Visit)]
pub struct EnableDirective {
    pub extensions: Vec<String>,
}

#[derive(Clone, Debug, PartialEq, Visit)]
pub struct RequiresDirective {
    pub extensions: Vec<String>,
}

#[derive(Clone, Debug, PartialEq, Visit)]
pub enum GlobalDeclaration {
    Void,
    Declaration(Declaration),
    TypeAlias(TypeAlias),
    Struct(Struct),
    Function(Function),
    ConstAssert(ConstAssert),
}

#[derive(Clone, Debug, PartialEq, Visit)]
pub struct Declaration {
    pub attributes: Vec<Attribute>,
    pub kind: DeclarationKind,
    pub template_args: Option<Vec<TemplateArg>>,
    pub name: String,
    pub typ: Option<TypeExpression>,
    pub initializer: Option<Expression>,
}

#[derive(Clone, Debug, PartialEq, Visit)]
pub enum DeclarationKind {
    Const,
    Override,
    Let,
    Var,
}

#[derive(Clone, Debug, PartialEq, Visit)]
pub struct TypeAlias {
    pub name: String,
    pub typ: TypeExpression,
}

#[derive(Clone, Debug, PartialEq, Visit)]
pub struct Struct {
    pub name: String,
    pub members: Vec<StructMember>,
}

#[derive(Clone, Debug, PartialEq, Visit)]
pub struct StructMember {
    pub attributes: Vec<Attribute>,
    pub name: String,
    pub typ: TypeExpression,
}

#[derive(Clone, Debug, PartialEq, Visit)]
pub struct Function {
    pub attributes: Vec<Attribute>,
    pub name: String,
    pub parameters: Vec<FormalParameter>,
    pub return_attributes: Vec<Attribute>,
    pub return_type: Option<TypeExpression>,
    pub body: CompoundStatement,
}

#[derive(Clone, Debug, PartialEq, Visit)]
pub struct FormalParameter {
    pub attributes: Vec<Attribute>,
    pub name: String,
    pub typ: TypeExpression,
}

#[derive(Clone, Debug, PartialEq, Visit)]
pub struct ConstAssert {
    pub expression: Expression,
}

#[derive(Clone, Debug, PartialEq, Visit)]
pub struct Attribute {
    pub name: String,
    pub arguments: Option<Vec<Expression>>,
}

#[derive(Clone, Debug, PartialEq, Visit)]
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

#[derive(Clone, Debug, PartialEq, Visit)]
pub enum LiteralExpression {
    True,
    False,
    AbstractInt(i64),
    AbstractFloat(f64),
    I32(i32),
    U32(u32),
    F32(f32),
    F16(f32),
}

pub type ParenthesizedExpression = Box<Expression>;

#[derive(Clone, Debug, PartialEq, Visit)]
pub struct NamedComponentExpression {
    pub base: Box<Expression>,
    pub component: String,
}

#[derive(Clone, Debug, PartialEq, Visit)]
pub struct IndexingExpression {
    pub base: Box<Expression>,
    pub index: Box<Expression>,
}

#[derive(Clone, Debug, PartialEq, Visit)]
pub struct UnaryExpression {
    pub operator: UnaryOperator,
    pub operand: Box<Expression>, // TODO maybe rename rhs
}

#[derive(Clone, Debug, PartialEq, Visit)]
pub enum UnaryOperator {
    LogicalNegation,
    Negation,
    BitwiseComplement,
    AddressOf,
    Indirection,
}

#[derive(Clone, Debug, PartialEq, Visit)]
pub struct BinaryExpression {
    pub operator: BinaryOperator,
    pub left: Box<Expression>, // TODO: rename lhs rhs
    pub right: Box<Expression>,
}

#[derive(Clone, Debug, PartialEq, Visit)]
pub enum BinaryOperator {
    ShortCircuitOr,
    ShortCircuitAnd,
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

#[derive(Clone, Debug, PartialEq, Visit)]
pub struct FunctionCallExpression {
    pub name: String,
    pub template_args: Option<Vec<TemplateArg>>,
    pub arguments: Vec<Expression>,
}

pub type IdentifierExpression = String;

#[derive(Clone, Debug, PartialEq, Visit)]
pub struct TypeExpression {
    pub name: String,
    pub template_args: Option<Vec<TemplateArg>>,
}

// TODO
pub type TemplateArg = Expression;

#[derive(Clone, Debug, PartialEq, Visit)]
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

#[derive(Clone, Debug, PartialEq, Visit)]
pub struct CompoundStatement {
    pub attributes: Vec<Attribute>,
    pub statements: Vec<Statement>,
}

#[derive(Clone, Debug, PartialEq, Visit)]
pub struct AssignmentStatement {
    pub operator: AssignmentOperator,
    pub lhs: Expression,
    pub rhs: Expression,
}

#[derive(Clone, Debug, PartialEq, Visit)]
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

pub type IncrementStatement = Expression;

pub type DecrementStatement = Expression;

#[derive(Clone, Debug, PartialEq, Visit)]
pub struct IfStatement {
    pub attributes: Vec<Attribute>,
    pub if_clause: (Expression, CompoundStatement),
    pub else_if_clauses: Vec<(Expression, CompoundStatement)>,
    pub else_clause: Option<CompoundStatement>,
}

#[derive(Clone, Debug, PartialEq, Visit)]
pub struct SwitchStatement {
    pub attributes: Vec<Attribute>,
    pub expression: Expression,
    pub body_attributes: Vec<Attribute>,
    pub clauses: Vec<SwitchClause>,
}

#[derive(Clone, Debug, PartialEq, Visit)]
pub struct SwitchClause {
    pub case_selectors: Vec<CaseSelector>,
    pub body: CompoundStatement,
}

#[derive(Clone, Debug, PartialEq, Visit)]
pub enum CaseSelector {
    Default,
    Expression(Expression),
}

#[derive(Clone, Debug, PartialEq, Visit)]
pub struct LoopStatement {
    pub attributes: Vec<Attribute>,
    pub body: CompoundStatement,
    // a ContinuingStatement can only appear inside a LoopStatement body, therefore it is
    // not part of the Statement enum. it appear shere instead, but consider it part of
    // body as the last statement of the CompoundStatement.
    pub continuing: Option<ContinuingStatement>,
}

#[derive(Clone, Debug, PartialEq, Visit)]
pub struct ContinuingStatement {
    pub body: CompoundStatement,
    // a BreakIfStatement can only appear inside a ContinuingStatement body, therefore it
    // not part of the Statement enum. it appear shere instead, but consider it part of
    // body as the last statement of the CompoundStatement.
    pub break_if: Option<BreakIfStatement>,
}

pub type BreakIfStatement = Expression;

#[derive(Clone, Debug, PartialEq, Visit)]
pub struct ForStatement {
    pub attributes: Vec<Attribute>,
    pub initializer: Option<Box<Statement>>,
    pub condition: Option<Expression>,
    pub update: Option<Box<Statement>>,
    pub body: CompoundStatement,
}

#[derive(Clone, Debug, PartialEq, Visit)]
pub struct WhileStatement {
    pub attributes: Vec<Attribute>,
    pub condition: Expression,
    pub body: CompoundStatement,
}

pub type ReturnStatement = Option<Expression>;

pub type FunctionCallStatement = FunctionCallExpression;

pub type ConstAssertStatement = ConstAssert;

pub type DeclarationStatement = Declaration;
