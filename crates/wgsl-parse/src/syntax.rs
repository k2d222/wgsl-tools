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

use derive_more::From;

#[derive(Default, Clone, Debug, PartialEq)]
pub struct TranslationUnit {
    #[cfg(feature = "imports")]
    pub imports: Vec<Import>,
    pub global_directives: Vec<GlobalDirective>,
    pub global_declarations: Vec<GlobalDeclaration>,
}

#[cfg(feature = "imports")]
#[derive(Clone, Debug, PartialEq)]
pub struct Import {
    #[cfg(feature = "condcomp")]
    pub attributes: Vec<Attribute>,
    pub path: std::path::PathBuf,
    pub content: ImportContent,
}

#[cfg(feature = "imports")]
#[derive(Clone, Debug, PartialEq)]
pub enum ImportContent {
    Star(ImportItem),
    Item(ImportItem),
    Collection(Vec<Import>),
}

#[cfg(feature = "imports")]
#[derive(Clone, Debug, PartialEq)]
pub struct ImportItem {
    pub name: String,
    pub rename: Option<String>,
}

#[derive(Clone, Debug, PartialEq, From)]
pub enum GlobalDirective {
    Diagnostic(DiagnosticDirective),
    Enable(EnableDirective),
    Requires(RequiresDirective),
}

#[derive(Clone, Debug, PartialEq)]
pub struct DiagnosticDirective {
    #[cfg(feature = "condcomp")]
    pub attributes: Vec<Attribute>,
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

#[derive(Clone, Debug, PartialEq)]
pub struct EnableDirective {
    #[cfg(feature = "condcomp")]
    pub attributes: Vec<Attribute>,
    pub extensions: Vec<String>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct RequiresDirective {
    #[cfg(feature = "condcomp")]
    pub attributes: Vec<Attribute>,
    pub extensions: Vec<String>,
}

#[derive(Clone, Debug, PartialEq, From)]
pub enum GlobalDeclaration {
    Void,
    Declaration(Declaration),
    TypeAlias(TypeAlias),
    Struct(Struct),
    Function(Function),
    ConstAssert(ConstAssert),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Declaration {
    pub attributes: Vec<Attribute>,
    pub kind: DeclarationKind,
    pub template_args: TemplateArgs,
    pub name: String,
    pub ty: Option<TypeExpression>,
    pub initializer: Option<Expression>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum DeclarationKind {
    Const,
    Override,
    Let,
    Var,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypeAlias {
    #[cfg(feature = "condcomp")]
    pub attributes: Vec<Attribute>,
    pub name: String,
    pub ty: TypeExpression,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Struct {
    #[cfg(feature = "condcomp")]
    pub attributes: Vec<Attribute>,
    pub name: String,
    pub members: Vec<StructMember>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct StructMember {
    pub attributes: Vec<Attribute>,
    pub name: String,
    pub ty: TypeExpression,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    pub attributes: Vec<Attribute>,
    pub name: String,
    pub parameters: Vec<FormalParameter>,
    pub return_attributes: Vec<Attribute>,
    pub return_type: Option<TypeExpression>,
    pub body: CompoundStatement,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FormalParameter {
    pub attributes: Vec<Attribute>,
    pub name: String,
    pub ty: TypeExpression,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ConstAssert {
    #[cfg(feature = "condcomp")]
    pub attributes: Vec<Attribute>,
    pub expression: Expression,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Attribute {
    pub name: String,
    pub arguments: Option<Vec<Expression>>,
}

#[derive(Clone, Debug, PartialEq, From)]
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

#[derive(Clone, Copy, Debug, PartialEq, From)]
pub enum LiteralExpression {
    Bool(bool),
    AbstractInt(i64),
    AbstractFloat(f64),
    I32(i32),
    U32(u32),
    F32(f32),
    #[from(skip)]
    F16(f32),
}

#[derive(Clone, Debug, PartialEq)]
pub struct ParenthesizedExpression {
    pub expression: Box<Expression>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct NamedComponentExpression {
    pub base: Box<Expression>,
    pub component: String,
}

#[derive(Clone, Debug, PartialEq)]
pub struct IndexingExpression {
    pub base: Box<Expression>,
    pub index: Box<Expression>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct UnaryExpression {
    pub operator: UnaryOperator,
    pub operand: Box<Expression>, // TODO maybe rename rhs
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum UnaryOperator {
    LogicalNegation,
    Negation,
    BitwiseComplement,
    AddressOf,
    Indirection,
}

#[derive(Clone, Debug, PartialEq)]
pub struct BinaryExpression {
    pub operator: BinaryOperator,
    pub left: Box<Expression>, // TODO: rename lhs rhs
    pub right: Box<Expression>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
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

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionCall {
    pub name: String,
    pub template_args: TemplateArgs,
    pub arguments: Vec<Expression>,
}

pub type FunctionCallExpression = FunctionCall;

#[derive(Clone, Debug, PartialEq)]
pub struct IdentifierExpression {
    pub name: String,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypeExpression {
    pub name: String,
    pub template_args: TemplateArgs,
}

// TODO
pub type TemplateArg = Expression;
pub type TemplateArgs = Option<Vec<TemplateArg>>;

#[derive(Clone, Debug, PartialEq, From)]
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
    Break(BreakStatement),
    Continue(ContinueStatement),
    Return(ReturnStatement),
    Discard(DiscardStatement),
    FunctionCall(FunctionCallStatement),
    ConstAssert(ConstAssertStatement),
    Declaration(DeclarationStatement),
}

#[derive(Clone, Debug, PartialEq)]
pub struct CompoundStatement {
    pub attributes: Vec<Attribute>,
    pub statements: Vec<Statement>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct AssignmentStatement {
    #[cfg(feature = "condcomp")]
    pub attributes: Vec<Attribute>,
    pub operator: AssignmentOperator,
    pub lhs: Expression,
    pub rhs: Expression,
}

#[derive(Clone, Debug, PartialEq)]
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

#[derive(Clone, Debug, PartialEq)]
pub struct IncrementStatement {
    #[cfg(feature = "condcomp")]
    pub attributes: Vec<Attribute>,
    pub expression: Expression,
}

#[derive(Clone, Debug, PartialEq)]
pub struct DecrementStatement {
    #[cfg(feature = "condcomp")]
    pub attributes: Vec<Attribute>,
    pub expression: Expression,
}

#[derive(Clone, Debug, PartialEq)]
pub struct IfStatement {
    pub attributes: Vec<Attribute>,
    pub if_clause: IfClause,
    pub else_if_clauses: Vec<ElseIfClause>,
    pub else_clause: Option<ElseClause>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct IfClause {
    pub expression: Expression,
    pub body: CompoundStatement,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ElseIfClause {
    #[cfg(feature = "condcomp")]
    pub attributes: Vec<Attribute>,
    pub expression: Expression,
    pub body: CompoundStatement,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ElseClause {
    #[cfg(feature = "condcomp")]
    pub attributes: Vec<Attribute>,
    pub body: CompoundStatement,
}

#[derive(Clone, Debug, PartialEq)]
pub struct SwitchStatement {
    pub attributes: Vec<Attribute>,
    pub expression: Expression,
    pub body_attributes: Vec<Attribute>,
    pub clauses: Vec<SwitchClause>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct SwitchClause {
    #[cfg(feature = "condcomp")]
    pub attributes: Vec<Attribute>,
    pub case_selectors: Vec<CaseSelector>,
    pub body: CompoundStatement,
}

#[derive(Clone, Debug, PartialEq, From)]
pub enum CaseSelector {
    Default,
    Expression(Expression),
}

#[derive(Clone, Debug, PartialEq)]
pub struct LoopStatement {
    pub attributes: Vec<Attribute>,
    pub body: CompoundStatement,
    // a ContinuingStatement can only appear inside a LoopStatement body, therefore it is
    // not part of the Statement enum. it appears here instead, but consider it part of
    // body as the last statement of the CompoundStatement.
    pub continuing: Option<ContinuingStatement>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ContinuingStatement {
    #[cfg(feature = "condcomp")]
    pub attributes: Vec<Attribute>,
    pub body: CompoundStatement,
    // a BreakIfStatement can only appear inside a ContinuingStatement body, therefore it
    // not part of the Statement enum. it appears here instead, but consider it part of
    // body as the last statement of the CompoundStatement.
    pub break_if: Option<BreakIfStatement>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct BreakIfStatement {
    #[cfg(feature = "condcomp")]
    pub attributes: Vec<Attribute>,
    pub expression: Expression,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ForStatement {
    pub attributes: Vec<Attribute>,
    pub initializer: Option<Box<Statement>>,
    pub condition: Option<Expression>,
    pub update: Option<Box<Statement>>,
    pub body: CompoundStatement,
}

#[derive(Clone, Debug, PartialEq)]
pub struct WhileStatement {
    pub attributes: Vec<Attribute>,
    pub condition: Expression,
    pub body: CompoundStatement,
}

#[derive(Clone, Debug, PartialEq)]
pub struct BreakStatement {
    #[cfg(feature = "condcomp")]
    pub attributes: Vec<Attribute>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ContinueStatement {
    #[cfg(feature = "condcomp")]
    pub attributes: Vec<Attribute>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ReturnStatement {
    #[cfg(feature = "condcomp")]
    pub attributes: Vec<Attribute>,
    pub expression: Option<Expression>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct DiscardStatement {
    #[cfg(feature = "condcomp")]
    pub attributes: Vec<Attribute>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionCallStatement {
    #[cfg(feature = "condcomp")]
    pub attributes: Vec<Attribute>,
    pub call: FunctionCall,
}

pub type ConstAssertStatement = ConstAssert;

pub type DeclarationStatement = Declaration;
