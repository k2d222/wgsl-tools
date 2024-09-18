use std::str::FromStr;

use super::{error::ParseError, syntax::*};

impl FromStr for DiagnosticSeverity {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "error" => Ok(Self::Error),
            "warning" => Ok(Self::Warning),
            "info" => Ok(Self::Info),
            "off" => Ok(Self::Off),
            _ => Err(ParseError::ParseDiagnosticSeverity),
        }
    }
}

impl From<String> for IdentifierExpression {
    fn from(identifier: String) -> Self {
        Self { name: identifier }
    }
}

/// A trait implemented on all types that can be prefixed by attributes.
pub trait Decorated {
    fn attributes(&self) -> &[Attribute];
    fn attributes_mut(&mut self) -> &mut [Attribute];
}

macro_rules! impl_struct_decorated {
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

#[cfg(all(feature = "imports", feature = "condcomp"))]
impl_struct_decorated!(Import);

#[cfg(feature = "condcomp")]
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

#[cfg(feature = "condcomp")]
impl_struct_decorated!(DiagnosticDirective);

#[cfg(feature = "condcomp")]
impl_struct_decorated!(EnableDirective);

#[cfg(feature = "condcomp")]
impl_struct_decorated!(RequiresDirective);

#[cfg(feature = "condcomp")]
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

impl_struct_decorated!(Declaration);

#[cfg(feature = "condcomp")]
impl_struct_decorated!(TypeAlias);

#[cfg(feature = "condcomp")]
impl_struct_decorated!(Struct);

impl_struct_decorated!(StructMember);

impl_struct_decorated!(Function);

impl_struct_decorated!(FormalParameter);

#[cfg(feature = "condcomp")]
impl_struct_decorated!(ConstAssert);

#[cfg(feature = "condcomp")]
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

impl_struct_decorated!(CompoundStatement);

#[cfg(feature = "condcomp")]
impl_struct_decorated!(AssignmentStatement);

#[cfg(feature = "condcomp")]
impl_struct_decorated!(IncrementStatement);

#[cfg(feature = "condcomp")]
impl_struct_decorated!(DecrementStatement);

impl_struct_decorated!(IfStatement);

#[cfg(feature = "condcomp")]
impl_struct_decorated!(ElseIfClause);

#[cfg(feature = "condcomp")]
impl_struct_decorated!(ElseClause);

impl_struct_decorated!(SwitchStatement);

#[cfg(feature = "condcomp")]
impl_struct_decorated!(SwitchClause);

impl_struct_decorated!(LoopStatement);

#[cfg(feature = "condcomp")]
impl_struct_decorated!(ContinuingStatement);

#[cfg(feature = "condcomp")]
impl_struct_decorated!(BreakIfStatement);

impl_struct_decorated!(ForStatement);

impl_struct_decorated!(WhileStatement);

#[cfg(feature = "condcomp")]
impl_struct_decorated!(BreakStatement);

#[cfg(feature = "condcomp")]
impl_struct_decorated!(ContinueStatement);

#[cfg(feature = "condcomp")]
impl_struct_decorated!(ReturnStatement);

#[cfg(feature = "condcomp")]
impl_struct_decorated!(DiscardStatement);

#[cfg(feature = "condcomp")]
impl_struct_decorated!(FunctionCallStatement);
