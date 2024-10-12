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

use crate::span::Spanned;

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
    #[cfg(feature = "attributes")]
    pub attributes: Attributes,
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
    #[cfg(feature = "attributes")]
    pub attributes: Attributes,
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
    #[cfg(feature = "attributes")]
    pub attributes: Attributes,
    pub extensions: Vec<String>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct RequiresDirective {
    #[cfg(feature = "attributes")]
    pub attributes: Attributes,
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
    pub attributes: Attributes,
    pub kind: DeclarationKind,
    pub template_args: TemplateArgs,
    pub name: String,
    pub ty: Option<TypeExpression>,
    pub initializer: Option<ExpressionNode>,
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
    #[cfg(feature = "attributes")]
    pub attributes: Attributes,
    pub name: String,
    pub ty: TypeExpression,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Struct {
    #[cfg(feature = "attributes")]
    pub attributes: Attributes,
    pub name: String,
    pub members: Vec<StructMember>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct StructMember {
    pub attributes: Attributes,
    pub name: String,
    pub ty: TypeExpression,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    pub attributes: Attributes,
    pub name: String,
    pub parameters: Vec<FormalParameter>,
    pub return_attributes: Attributes,
    pub return_type: Option<TypeExpression>,
    pub body: CompoundStatement,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FormalParameter {
    pub attributes: Attributes,
    pub name: String,
    pub ty: TypeExpression,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ConstAssert {
    #[cfg(feature = "attributes")]
    pub attributes: Attributes,
    pub expression: ExpressionNode,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum BuiltinValue {
    VertexIndex,
    InstanceIndex,
    Position,
    FrontFacing,
    FragDepth,
    SampleIndex,
    SampleMask,
    LocalInvocationId,
    LocalInvocationIndex,
    GlobalInvocationId,
    WorkgroupId,
    NumWorkgroups,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum InterpolationType {
    Perspective,
    Linear,
    Flat,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum InterpolationSampling {
    Center,
    Centroid,
    Sample,
    First,
    Either,
}

#[derive(Clone, Debug, PartialEq)]
pub struct DiagnosticAttribute {
    pub severity: DiagnosticSeverity,
    pub rule: String,
}

#[derive(Clone, Debug, PartialEq)]
pub struct InterpolateAttribute {
    pub ty: InterpolationType,
    pub sampling: InterpolationSampling,
}

#[derive(Clone, Debug, PartialEq)]
pub struct WorkgroupSizeAttribute {
    pub x: ExpressionNode,
    pub y: Option<ExpressionNode>,
    pub z: Option<ExpressionNode>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct CustomAttribute {
    pub name: String,
    pub arguments: Option<Vec<ExpressionNode>>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Attribute {
    Align(ExpressionNode),
    Binding(ExpressionNode),
    BlendSrc(ExpressionNode),
    Builtin(BuiltinValue),
    Const,
    Diagnostic(DiagnosticAttribute),
    Group(ExpressionNode),
    Id(ExpressionNode),
    Interpolate(InterpolateAttribute),
    Invariant,
    Location(ExpressionNode),
    MustUse,
    Size(ExpressionNode),
    WorkgroupSize(WorkgroupSizeAttribute),
    Vertex,
    Fragment,
    Compute,
    #[cfg(feature = "condcomp")]
    If(ExpressionNode),
    Custom(CustomAttribute),
}

pub type Attributes = Vec<Attribute>;

#[derive(Clone, Debug, PartialEq, From)]
pub enum Expression {
    Literal(LiteralExpression),
    Parenthesized(ParenthesizedExpression),
    NamedComponent(NamedComponentExpression),
    Indexing(IndexingExpression),
    Unary(UnaryExpression),
    Binary(BinaryExpression),
    FunctionCall(FunctionCallExpression),
    TypeOrIdentifier(TypeExpression),
}

pub type ExpressionNode = Spanned<Expression>;

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
    pub expression: ExpressionNode,
}

#[derive(Clone, Debug, PartialEq)]
pub struct NamedComponentExpression {
    pub base: ExpressionNode,
    pub component: String,
}

#[derive(Clone, Debug, PartialEq)]
pub struct IndexingExpression {
    pub base: ExpressionNode,
    pub index: ExpressionNode,
}

#[derive(Clone, Debug, PartialEq)]
pub struct UnaryExpression {
    pub operator: UnaryOperator,
    pub operand: ExpressionNode,
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
    pub left: ExpressionNode,
    pub right: ExpressionNode,
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
    pub ty: TypeExpression,
    pub arguments: Vec<ExpressionNode>,
}

pub type FunctionCallExpression = FunctionCall;

#[derive(Clone, Debug, PartialEq)]
pub struct TypeExpression {
    pub name: String,
    pub template_args: TemplateArgs,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TemplateArg {
    pub expression: ExpressionNode,
}
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

pub type StatementNode = Spanned<Statement>;

#[derive(Clone, Debug, PartialEq)]
pub struct CompoundStatement {
    pub attributes: Attributes,
    pub statements: Vec<StatementNode>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct AssignmentStatement {
    #[cfg(feature = "attributes")]
    pub attributes: Attributes,
    pub operator: AssignmentOperator,
    pub lhs: ExpressionNode,
    pub rhs: ExpressionNode,
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
    #[cfg(feature = "attributes")]
    pub attributes: Attributes,
    pub expression: ExpressionNode,
}

#[derive(Clone, Debug, PartialEq)]
pub struct DecrementStatement {
    #[cfg(feature = "attributes")]
    pub attributes: Attributes,
    pub expression: ExpressionNode,
}

#[derive(Clone, Debug, PartialEq)]
pub struct IfStatement {
    pub attributes: Attributes,
    pub if_clause: IfClause,
    pub else_if_clauses: Vec<ElseIfClause>,
    pub else_clause: Option<ElseClause>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct IfClause {
    pub expression: ExpressionNode,
    pub body: CompoundStatement,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ElseIfClause {
    #[cfg(feature = "attributes")]
    pub attributes: Attributes,
    pub expression: ExpressionNode,
    pub body: CompoundStatement,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ElseClause {
    #[cfg(feature = "attributes")]
    pub attributes: Attributes,
    pub body: CompoundStatement,
}

#[derive(Clone, Debug, PartialEq)]
pub struct SwitchStatement {
    pub attributes: Attributes,
    pub expression: ExpressionNode,
    pub body_attributes: Attributes,
    pub clauses: Vec<SwitchClause>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct SwitchClause {
    #[cfg(feature = "attributes")]
    pub attributes: Attributes,
    pub case_selectors: Vec<CaseSelector>,
    pub body: CompoundStatement,
}

#[derive(Clone, Debug, PartialEq, From)]
pub enum CaseSelector {
    Default,
    Expression(ExpressionNode),
}

#[derive(Clone, Debug, PartialEq)]
pub struct LoopStatement {
    pub attributes: Attributes,
    pub body: CompoundStatement,
    // a ContinuingStatement can only appear inside a LoopStatement body, therefore it is
    // not part of the StatementNode enum. it appears here instead, but consider it part of
    // body as the last statement of the CompoundStatement.
    pub continuing: Option<ContinuingStatement>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ContinuingStatement {
    #[cfg(feature = "attributes")]
    pub attributes: Attributes,
    pub body: CompoundStatement,
    // a BreakIfStatement can only appear inside a ContinuingStatement body, therefore it
    // not part of the StatementNode enum. it appears here instead, but consider it part of
    // body as the last statement of the CompoundStatement.
    pub break_if: Option<BreakIfStatement>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct BreakIfStatement {
    #[cfg(feature = "attributes")]
    pub attributes: Attributes,
    pub expression: ExpressionNode,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ForStatement {
    pub attributes: Attributes,
    pub initializer: Option<StatementNode>,
    pub condition: Option<ExpressionNode>,
    pub update: Option<StatementNode>,
    pub body: CompoundStatement,
}

#[derive(Clone, Debug, PartialEq)]
pub struct WhileStatement {
    pub attributes: Attributes,
    pub condition: ExpressionNode,
    pub body: CompoundStatement,
}

#[derive(Clone, Debug, PartialEq)]
pub struct BreakStatement {
    #[cfg(feature = "attributes")]
    pub attributes: Attributes,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ContinueStatement {
    #[cfg(feature = "attributes")]
    pub attributes: Attributes,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ReturnStatement {
    #[cfg(feature = "attributes")]
    pub attributes: Attributes,
    pub expression: Option<ExpressionNode>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct DiscardStatement {
    #[cfg(feature = "attributes")]
    pub attributes: Attributes,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionCallStatement {
    #[cfg(feature = "attributes")]
    pub attributes: Attributes,
    pub call: FunctionCall,
}

pub type ConstAssertStatement = ConstAssert;

pub type DeclarationStatement = Declaration;
