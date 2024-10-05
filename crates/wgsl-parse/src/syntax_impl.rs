use crate::span::Spanned;

use super::syntax::*;

impl From<String> for TypeExpression {
    fn from(name: String) -> Self {
        Self {
            name,
            template_args: None,
        }
    }
}

impl GlobalDeclaration {
    pub fn name(&self) -> &str {
        match self {
            GlobalDeclaration::Void => "",
            GlobalDeclaration::Declaration(decl) => decl.name.as_str(),
            GlobalDeclaration::TypeAlias(decl) => decl.name.as_str(),
            GlobalDeclaration::Struct(decl) => decl.name.as_str(),
            GlobalDeclaration::Function(decl) => decl.name.as_str(),
            GlobalDeclaration::ConstAssert(_) => "",
        }
    }
}

/// A trait implemented on all types that can be prefixed by attributes.
pub trait Decorated {
    fn attributes(&self) -> &[Attribute];
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
