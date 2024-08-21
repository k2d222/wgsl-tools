//! support functions to be injected in the lalrpop parser.

use crate::{span::*, syntax_spanned::*};

pub(crate) enum Component {
    Named(Span),
    Index(Spanned<Box<Expression>>),
}

pub(crate) fn apply_components(
    components: Vec<Component>,
    expr: Spanned<Expression>,
) -> Spanned<Expression> {
    components.into_iter().fold(expr, |base, comp| match comp {
        Component::Named(component) => {
            let span = base.span().start..component.end;
            Spanned::new(
                Expression::NamedComponent(NamedComponentExpression {
                    base: base.into(),
                    component,
                }),
                span,
            )
        }
        Component::Index(index) => {
            let span = base.span().start..index.span().end;
            Spanned::new(
                Expression::Indexing(IndexingExpression {
                    base: base.into(),
                    index,
                }),
                span,
            )
        }
    })
}
