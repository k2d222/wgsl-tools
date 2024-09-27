//! support functions to be injected in the lalrpop parser.

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
