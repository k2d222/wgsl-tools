use crate::span::Spanned;

use super::syntax::*;

impl TranslationUnit {
    /// New empty [`TranslationUnit`]
    pub fn new() -> Self {
        Self::default()
    }

    /// Remove all [`GlobalDeclaration::Void`] and [`Statement::Void`]
    pub fn remove_voids(&mut self) {
        self.global_declarations.retain_mut(|decl| match decl {
            GlobalDeclaration::Void => false,
            _ => {
                decl.remove_voids();
                true
            }
        })
    }
}

impl GlobalDeclaration {
    /// Remove all [`Statement::Void`]
    pub fn remove_voids(&mut self) {
        if let GlobalDeclaration::Function(decl) = self {
            decl.body.remove_voids();
        }
    }
}

impl TypeExpression {
    /// New [`TypeExpression`] with no template.
    pub fn new(ident: Ident) -> Self {
        Self {
            #[cfg(feature = "imports")]
            path: None,
            ident,
            template_args: None,
        }
    }
}

impl CompoundStatement {
    /// Remove all [`Statement::Void`]
    pub fn remove_voids(&mut self) {
        self.statements.retain_mut(|stat| match stat.node_mut() {
            Statement::Void => false,
            _ => {
                stat.remove_voids();
                true
            }
        })
    }
}

impl Statement {
    /// Remove all [`Statement::Void`]
    pub fn remove_voids(&mut self) {
        match self {
            Statement::Compound(stat) => {
                stat.remove_voids();
            }
            Statement::If(stat) => {
                stat.if_clause.body.remove_voids();
                for clause in &mut stat.else_if_clauses {
                    clause.body.remove_voids();
                }
                if let Some(clause) = &mut stat.else_clause {
                    clause.body.remove_voids();
                }
            }
            Statement::Switch(stat) => stat
                .clauses
                .iter_mut()
                .for_each(|clause| clause.body.remove_voids()),
            Statement::Loop(stat) => stat.body.remove_voids(),
            Statement::For(stat) => stat.body.remove_voids(),
            Statement::While(stat) => stat.body.remove_voids(),
            _ => (),
        }
    }
}

impl AccessMode {
    /// Is [`Self::Read`] or [`Self::ReadWrite`]
    pub fn is_read(&self) -> bool {
        matches!(self, Self::Read | Self::ReadWrite)
    }
    /// Is [`Self::Write`] or [`Self::ReadWrite`]
    pub fn is_write(&self) -> bool {
        matches!(self, Self::Write | Self::ReadWrite)
    }
}

impl From<Ident> for TypeExpression {
    fn from(name: Ident) -> Self {
        Self {
            #[cfg(feature = "imports")]
            path: None,
            ident: name,
            template_args: None,
        }
    }
}

impl From<ExpressionNode> for ReturnStatement {
    fn from(expression: ExpressionNode) -> Self {
        Self {
            #[cfg(feature = "attributes")]
            attributes: Default::default(),
            expression: Some(expression),
        }
    }
}
impl From<Expression> for ReturnStatement {
    fn from(expression: Expression) -> Self {
        Self::from(ExpressionNode::from(expression))
    }
}

impl GlobalDeclaration {
    /// Get the name of the declaration, if it has one.
    pub fn ident(&self) -> Option<&Ident> {
        match self {
            GlobalDeclaration::Void => None,
            GlobalDeclaration::Declaration(decl) => Some(&decl.ident),
            GlobalDeclaration::TypeAlias(decl) => Some(&decl.ident),
            GlobalDeclaration::Struct(decl) => Some(&decl.ident),
            GlobalDeclaration::Function(decl) => Some(&decl.ident),
            GlobalDeclaration::ConstAssert(_) => None,
        }
    }
    /// Get the name of the declaration, if it has one.
    pub fn ident_mut(&mut self) -> Option<&mut Ident> {
        match self {
            GlobalDeclaration::Void => None,
            GlobalDeclaration::Declaration(decl) => Some(&mut decl.ident),
            GlobalDeclaration::TypeAlias(decl) => Some(&mut decl.ident),
            GlobalDeclaration::Struct(decl) => Some(&mut decl.ident),
            GlobalDeclaration::Function(decl) => Some(&mut decl.ident),
            GlobalDeclaration::ConstAssert(_) => None,
        }
    }
}

/// A trait implemented on all types that can be prefixed by attributes.
pub trait Decorated {
    /// List all attributes (`@name`) of a syntax node.
    fn attributes(&self) -> &[Attribute];
    /// List all attributes (`@name`) of a syntax node.
    fn attributes_mut(&mut self) -> &mut [Attribute];
}

impl<T: Decorated> Decorated for Spanned<T> {
    fn attributes(&self) -> &[Attribute] {
        self.node().attributes()
    }

    fn attributes_mut(&mut self) -> &mut [Attribute] {
        self.node_mut().attributes_mut()
    }
}

macro_rules! impl_decorated_struct {
    ($ty:ty) => {
        impl Decorated for $ty {
            fn attributes(&self) -> &[Attribute] {
                &self.attributes
            }
            fn attributes_mut(&mut self) -> &mut [Attribute] {
                &mut self.attributes
            }
        }
    };
}

#[cfg(all(feature = "imports", feature = "attributes"))]
impl_decorated_struct!(Import);

#[cfg(feature = "attributes")]
impl Decorated for GlobalDirective {
    fn attributes(&self) -> &[Attribute] {
        match self {
            GlobalDirective::Diagnostic(directive) => &directive.attributes,
            GlobalDirective::Enable(directive) => &directive.attributes,
            GlobalDirective::Requires(directive) => &directive.attributes,
        }
    }

    fn attributes_mut(&mut self) -> &mut [Attribute] {
        match self {
            GlobalDirective::Diagnostic(directive) => &mut directive.attributes,
            GlobalDirective::Enable(directive) => &mut directive.attributes,
            GlobalDirective::Requires(directive) => &mut directive.attributes,
        }
    }
}

#[cfg(feature = "attributes")]
impl_decorated_struct!(DiagnosticDirective);

#[cfg(feature = "attributes")]
impl_decorated_struct!(EnableDirective);

#[cfg(feature = "attributes")]
impl_decorated_struct!(RequiresDirective);

#[cfg(feature = "attributes")]
impl Decorated for GlobalDeclaration {
    fn attributes(&self) -> &[Attribute] {
        match self {
            GlobalDeclaration::Void => &[],
            GlobalDeclaration::Declaration(decl) => &decl.attributes,
            GlobalDeclaration::TypeAlias(decl) => &decl.attributes,
            GlobalDeclaration::Struct(decl) => &decl.attributes,
            GlobalDeclaration::Function(decl) => &decl.attributes,
            GlobalDeclaration::ConstAssert(decl) => &decl.attributes,
        }
    }

    fn attributes_mut(&mut self) -> &mut [Attribute] {
        match self {
            GlobalDeclaration::Void => &mut [],
            GlobalDeclaration::Declaration(decl) => &mut decl.attributes,
            GlobalDeclaration::TypeAlias(decl) => &mut decl.attributes,
            GlobalDeclaration::Struct(decl) => &mut decl.attributes,
            GlobalDeclaration::Function(decl) => &mut decl.attributes,
            GlobalDeclaration::ConstAssert(decl) => &mut decl.attributes,
        }
    }
}

impl_decorated_struct!(Declaration);

#[cfg(feature = "attributes")]
impl_decorated_struct!(TypeAlias);

#[cfg(feature = "attributes")]
impl_decorated_struct!(Struct);

impl_decorated_struct!(StructMember);

impl_decorated_struct!(Function);

impl_decorated_struct!(FormalParameter);

#[cfg(feature = "attributes")]
impl_decorated_struct!(ConstAssert);

#[cfg(feature = "attributes")]
impl Decorated for Statement {
    fn attributes(&self) -> &[Attribute] {
        match self {
            Statement::Void => &[],
            Statement::Compound(stat) => &stat.attributes,
            Statement::Assignment(stat) => &stat.attributes,
            Statement::Increment(stat) => &stat.attributes,
            Statement::Decrement(stat) => &stat.attributes,
            Statement::If(stat) => &stat.attributes,
            Statement::Switch(stat) => &stat.attributes,
            Statement::Loop(stat) => &stat.attributes,
            Statement::For(stat) => &stat.attributes,
            Statement::While(stat) => &stat.attributes,
            Statement::Break(stat) => &stat.attributes,
            Statement::Continue(stat) => &stat.attributes,
            Statement::Return(stat) => &stat.attributes,
            Statement::Discard(stat) => &stat.attributes,
            Statement::FunctionCall(stat) => &stat.attributes,
            Statement::ConstAssert(stat) => &stat.attributes,
            Statement::Declaration(stat) => &stat.attributes,
        }
    }

    fn attributes_mut(&mut self) -> &mut [Attribute] {
        match self {
            Statement::Void => &mut [],
            Statement::Compound(stat) => &mut stat.attributes,
            Statement::Assignment(stat) => &mut stat.attributes,
            Statement::Increment(stat) => &mut stat.attributes,
            Statement::Decrement(stat) => &mut stat.attributes,
            Statement::If(stat) => &mut stat.attributes,
            Statement::Switch(stat) => &mut stat.attributes,
            Statement::Loop(stat) => &mut stat.attributes,
            Statement::For(stat) => &mut stat.attributes,
            Statement::While(stat) => &mut stat.attributes,
            Statement::Break(stat) => &mut stat.attributes,
            Statement::Continue(stat) => &mut stat.attributes,
            Statement::Return(stat) => &mut stat.attributes,
            Statement::Discard(stat) => &mut stat.attributes,
            Statement::FunctionCall(stat) => &mut stat.attributes,
            Statement::ConstAssert(stat) => &mut stat.attributes,
            Statement::Declaration(stat) => &mut stat.attributes,
        }
    }
}

impl_decorated_struct!(CompoundStatement);

#[cfg(feature = "attributes")]
impl_decorated_struct!(AssignmentStatement);

#[cfg(feature = "attributes")]
impl_decorated_struct!(IncrementStatement);

#[cfg(feature = "attributes")]
impl_decorated_struct!(DecrementStatement);

impl_decorated_struct!(IfStatement);

#[cfg(feature = "attributes")]
impl_decorated_struct!(ElseIfClause);

#[cfg(feature = "attributes")]
impl_decorated_struct!(ElseClause);

impl_decorated_struct!(SwitchStatement);

#[cfg(feature = "attributes")]
impl_decorated_struct!(SwitchClause);

impl_decorated_struct!(LoopStatement);

#[cfg(feature = "attributes")]
impl_decorated_struct!(ContinuingStatement);

#[cfg(feature = "attributes")]
impl_decorated_struct!(BreakIfStatement);

impl_decorated_struct!(ForStatement);

impl_decorated_struct!(WhileStatement);

#[cfg(feature = "attributes")]
impl_decorated_struct!(BreakStatement);

#[cfg(feature = "attributes")]
impl_decorated_struct!(ContinueStatement);

#[cfg(feature = "attributes")]
impl_decorated_struct!(ReturnStatement);

#[cfg(feature = "attributes")]
impl_decorated_struct!(DiscardStatement);

#[cfg(feature = "attributes")]
impl_decorated_struct!(FunctionCallStatement);
