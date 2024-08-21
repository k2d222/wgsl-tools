//! support functions to be injected in the lalrpop parser.

use crate::syntax::*;

pub(crate) enum Component {
    Named(String),
    Index(Box<Expression>),
}

pub(crate) fn apply_components(components: Vec<Component>, expr: Expression) -> Expression {
    components.into_iter().fold(expr, |base, comp| match comp {
        Component::Named(component) => Expression::NamedComponent(NamedComponentExpression {
            base: base.into(),
            component,
        }),
        Component::Index(index) => Expression::Indexing(IndexingExpression {
            base: base.into(),
            index,
        }),
    })
}
