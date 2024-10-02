//! support functions to be injected in the lalrpop parser.

use std::str::FromStr;

use crate::{
    span::{Span, Spanned},
    syntax::*,
};

pub(crate) enum Component {
    Named(String),
    Index(ExpressionNode),
}

pub(crate) fn apply_components(
    expr: Expression,
    span: Span,
    components: Vec<Spanned<Component>>,
) -> Expression {
    components.into_iter().fold(expr, |base, comp| {
        let span = span.extend(comp.span());
        let base = Spanned::new(base, span);
        match comp.into_inner() {
            Component::Named(component) => {
                Expression::NamedComponent(NamedComponentExpression { base, component })
            }
            Component::Index(index) => Expression::Indexing(IndexingExpression { base, index }),
        }
    })
}

impl FromStr for DiagnosticSeverity {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "error" => Ok(Self::Error),
            "warning" => Ok(Self::Warning),
            "info" => Ok(Self::Info),
            "off" => Ok(Self::Off),
            _ => Err(()),
        }
    }
}
