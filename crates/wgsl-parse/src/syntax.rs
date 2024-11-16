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

use derive_more::{From, IsVariant};

use crate::span::Spanned;

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Default, Clone, Debug, PartialEq)]
pub struct TranslationUnit {
    #[cfg(feature = "imports")]
    pub imports: Vec<Import>,
    pub global_directives: Vec<GlobalDirective>,
    pub global_declarations: Vec<GlobalDeclaration>,
}

#[cfg_attr(
    all(feature = "serde", feature = "imports"),
    derive(serde::Serialize, serde::Deserialize)
)]
#[cfg(feature = "imports")]
#[derive(Clone, Debug, PartialEq)]
pub struct Import {
    #[cfg(feature = "attributes")]
    pub attributes: Attributes,
    pub path: std::path::PathBuf,
    pub content: ImportContent,
}

#[cfg_attr(
    all(feature = "serde", feature = "imports"),
    derive(serde::Serialize, serde::Deserialize)
)]
#[cfg(feature = "imports")]
#[derive(Clone, Debug, PartialEq, IsVariant)]
pub enum ImportContent {
    Star(ImportItem),
    Item(ImportItem),
    Collection(Vec<Import>),
}

#[cfg_attr(
    all(feature = "serde", feature = "imports"),
    derive(serde::Serialize, serde::Deserialize)
)]
#[cfg(feature = "imports")]
#[derive(Clone, Debug, PartialEq)]
pub struct ImportItem {
    pub name: String,
    pub rename: Option<String>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone, Debug, PartialEq, From, IsVariant)]
pub enum GlobalDirective {
    Diagnostic(DiagnosticDirective),
    Enable(EnableDirective),
    Requires(RequiresDirective),
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone, Debug, PartialEq)]
pub struct DiagnosticDirective {
    #[cfg(feature = "attributes")]
    pub attributes: Attributes,
    pub severity: DiagnosticSeverity,
    pub rule_name: String,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone, Debug, PartialEq, IsVariant)]
pub enum DiagnosticSeverity {
    Error,
    Warning,
    Info,
    Off,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone, Debug, PartialEq)]
pub struct EnableDirective {
    #[cfg(feature = "attributes")]
    pub attributes: Attributes,
    pub extensions: Vec<String>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone, Debug, PartialEq)]
pub struct RequiresDirective {
    #[cfg(feature = "attributes")]
    pub attributes: Attributes,
    pub extensions: Vec<String>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone, Debug, PartialEq, From, IsVariant)]
pub enum GlobalDeclaration {
    Void,
    Declaration(Declaration),
    TypeAlias(TypeAlias),
    Struct(Struct),
    Function(Function),
    ConstAssert(ConstAssert),
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone, Debug, PartialEq)]
pub struct Declaration {
    pub attributes: Attributes,
    pub kind: DeclarationKind,
    pub name: String,
    pub ty: Option<TypeExpression>,
    pub initializer: Option<ExpressionNode>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone, Copy, Debug, PartialEq, IsVariant)]
pub enum DeclarationKind {
    Const,
    Override,
    Let,
    Var(Option<AddressSpace>),
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone, Copy, Debug, PartialEq, Eq, IsVariant)]
pub enum AddressSpace {
    Function,
    Private,
    Workgroup,
    Uniform,
    // TODO: move access mode out of here
    Storage(Option<AccessMode>),
    Handle,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AccessMode {
    Read,
    Write,
    ReadWrite,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone, Debug, PartialEq)]
pub struct TypeAlias {
    #[cfg(feature = "attributes")]
    pub attributes: Attributes,
    pub name: String,
    pub ty: TypeExpression,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone, Debug, PartialEq)]
pub struct Struct {
    #[cfg(feature = "attributes")]
    pub attributes: Attributes,
    pub name: String,
    pub members: Vec<StructMember>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone, Debug, PartialEq)]
pub struct StructMember {
    pub attributes: Attributes,
    pub name: String,
    pub ty: TypeExpression,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    pub attributes: Attributes,
    pub name: String,
    pub parameters: Vec<FormalParameter>,
    pub return_attributes: Attributes,
    pub return_type: Option<TypeExpression>,
    pub body: CompoundStatement,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone, Debug, PartialEq)]
pub struct FormalParameter {
    pub attributes: Attributes,
    pub name: String,
    pub ty: TypeExpression,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone, Debug, PartialEq)]
pub struct ConstAssert {
    #[cfg(feature = "attributes")]
    pub attributes: Attributes,
    pub expression: ExpressionNode,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone, Copy, Debug, PartialEq, IsVariant)]
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

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone, Copy, Debug, PartialEq, IsVariant)]
pub enum InterpolationType {
    Perspective,
    Linear,
    Flat,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone, Copy, Debug, PartialEq, IsVariant)]
pub enum InterpolationSampling {
    Center,
    Centroid,
    Sample,
    First,
    Either,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone, Debug, PartialEq)]
pub struct DiagnosticAttribute {
    pub severity: DiagnosticSeverity,
    pub rule: String,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone, Debug, PartialEq)]
pub struct InterpolateAttribute {
    pub ty: InterpolationType,
    pub sampling: InterpolationSampling,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone, Debug, PartialEq)]
pub struct WorkgroupSizeAttribute {
    pub x: ExpressionNode,
    pub y: Option<ExpressionNode>,
    pub z: Option<ExpressionNode>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone, Debug, PartialEq)]
pub struct CustomAttribute {
    pub name: String,
    pub arguments: Option<Vec<ExpressionNode>>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone, Debug, PartialEq, IsVariant)]
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
    #[cfg(feature = "generics")]
    Type(TypeConstraint),
    Custom(CustomAttribute),
}

#[cfg_attr(
    all(feature = "serde", feature = "generics"),
    derive(serde::Serialize, serde::Deserialize)
)]
#[cfg(feature = "generics")]
#[derive(Clone, Debug, PartialEq, From)]
pub struct TypeConstraint {
    pub name: String,
    pub variants: Vec<TypeExpression>,
}

pub type Attributes = Vec<Attribute>;

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone, Debug, PartialEq, From, IsVariant)]
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

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone, Copy, Debug, PartialEq, From, IsVariant)]
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

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone, Debug, PartialEq)]
pub struct ParenthesizedExpression {
    pub expression: ExpressionNode,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone, Debug, PartialEq)]
pub struct NamedComponentExpression {
    pub base: ExpressionNode,
    pub component: String,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone, Debug, PartialEq)]
pub struct IndexingExpression {
    pub base: ExpressionNode,
    pub index: ExpressionNode,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone, Debug, PartialEq)]
pub struct UnaryExpression {
    pub operator: UnaryOperator,
    pub operand: ExpressionNode,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone, Copy, Debug, PartialEq, IsVariant)]
pub enum UnaryOperator {
    LogicalNegation,
    Negation,
    BitwiseComplement,
    AddressOf,
    Indirection,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone, Debug, PartialEq)]
pub struct BinaryExpression {
    pub operator: BinaryOperator,
    pub left: ExpressionNode,
    pub right: ExpressionNode,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone, Copy, Debug, PartialEq, IsVariant)]
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

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone, Debug, PartialEq)]
pub struct FunctionCall {
    pub ty: TypeExpression,
    pub arguments: Vec<ExpressionNode>,
}

pub type FunctionCallExpression = FunctionCall;

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone, Debug, PartialEq)]
pub struct TypeExpression {
    pub name: String,
    pub template_args: TemplateArgs,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone, Debug, PartialEq)]
pub struct TemplateArg {
    pub expression: ExpressionNode,
}
pub type TemplateArgs = Option<Vec<TemplateArg>>;

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone, Debug, PartialEq, From, IsVariant)]
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

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone, Debug, PartialEq)]
pub struct CompoundStatement {
    pub attributes: Attributes,
    pub statements: Vec<StatementNode>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone, Debug, PartialEq)]
pub struct AssignmentStatement {
    #[cfg(feature = "attributes")]
    pub attributes: Attributes,
    pub operator: AssignmentOperator,
    pub lhs: ExpressionNode,
    pub rhs: ExpressionNode,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone, Debug, PartialEq, IsVariant)]
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

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone, Debug, PartialEq)]
pub struct IncrementStatement {
    #[cfg(feature = "attributes")]
    pub attributes: Attributes,
    pub expression: ExpressionNode,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone, Debug, PartialEq)]
pub struct DecrementStatement {
    #[cfg(feature = "attributes")]
    pub attributes: Attributes,
    pub expression: ExpressionNode,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone, Debug, PartialEq)]
pub struct IfStatement {
    pub attributes: Attributes,
    pub if_clause: IfClause,
    pub else_if_clauses: Vec<ElseIfClause>,
    pub else_clause: Option<ElseClause>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone, Debug, PartialEq)]
pub struct IfClause {
    pub expression: ExpressionNode,
    pub body: CompoundStatement,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone, Debug, PartialEq)]
pub struct ElseIfClause {
    #[cfg(feature = "attributes")]
    pub attributes: Attributes,
    pub expression: ExpressionNode,
    pub body: CompoundStatement,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone, Debug, PartialEq)]
pub struct ElseClause {
    #[cfg(feature = "attributes")]
    pub attributes: Attributes,
    pub body: CompoundStatement,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone, Debug, PartialEq)]
pub struct SwitchStatement {
    pub attributes: Attributes,
    pub expression: ExpressionNode,
    pub body_attributes: Attributes,
    pub clauses: Vec<SwitchClause>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone, Debug, PartialEq)]
pub struct SwitchClause {
    #[cfg(feature = "attributes")]
    pub attributes: Attributes,
    pub case_selectors: Vec<CaseSelector>,
    pub body: CompoundStatement,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone, Debug, PartialEq, From)]
pub enum CaseSelector {
    Default,
    Expression(ExpressionNode),
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone, Debug, PartialEq)]
pub struct LoopStatement {
    pub attributes: Attributes,
    pub body: CompoundStatement,
    // a ContinuingStatement can only appear inside a LoopStatement body, therefore it is
    // not part of the StatementNode enum. it appears here instead, but consider it part of
    // body as the last statement of the CompoundStatement.
    pub continuing: Option<ContinuingStatement>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
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

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone, Debug, PartialEq)]
pub struct BreakIfStatement {
    #[cfg(feature = "attributes")]
    pub attributes: Attributes,
    pub expression: ExpressionNode,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone, Debug, PartialEq)]
pub struct ForStatement {
    pub attributes: Attributes,
    pub initializer: Option<StatementNode>,
    pub condition: Option<ExpressionNode>,
    pub update: Option<StatementNode>,
    pub body: CompoundStatement,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone, Debug, PartialEq)]
pub struct WhileStatement {
    pub attributes: Attributes,
    pub condition: ExpressionNode,
    pub body: CompoundStatement,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone, Debug, PartialEq)]
pub struct BreakStatement {
    #[cfg(feature = "attributes")]
    pub attributes: Attributes,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone, Debug, PartialEq)]
pub struct ContinueStatement {
    #[cfg(feature = "attributes")]
    pub attributes: Attributes,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone, Debug, PartialEq)]
pub struct ReturnStatement {
    #[cfg(feature = "attributes")]
    pub attributes: Attributes,
    pub expression: Option<ExpressionNode>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone, Debug, PartialEq)]
pub struct DiscardStatement {
    #[cfg(feature = "attributes")]
    pub attributes: Attributes,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone, Debug, PartialEq)]
pub struct FunctionCallStatement {
    #[cfg(feature = "attributes")]
    pub attributes: Attributes,
    pub call: FunctionCall,
}

pub type ConstAssertStatement = ConstAssert;

pub type DeclarationStatement = Declaration;
