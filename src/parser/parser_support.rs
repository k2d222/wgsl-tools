// support functions to be injected in the lalrpop parser.

use super::{ast::*, Span};

pub(crate) type S<T> = Spanned<T>;

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
            let span = base.1.start..component.end;
            Spanned(
                Expression::NamedComponent(NamedComponentExpression {
                    base: base.into(),
                    component,
                }),
                span,
            )
        }
        Component::Index(index) => {
            let span = base.1.start..index.1.end;
            Spanned(
                Expression::Indexing(IndexingExpression {
                    base: base.into(),
                    index,
                }),
                span,
            )
        }
    })
}
