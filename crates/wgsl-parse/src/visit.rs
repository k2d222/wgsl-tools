use wgsl_parse_macros::{query, query_mut};

use crate::syntax::*;

pub trait Visit<T> {
    fn visit<'a>(&'a self) -> impl Iterator<Item = &'a T>
    where
        T: 'a;
    fn visit_mut<'a>(&'a mut self) -> impl Iterator<Item = &'a mut T>
    where
        T: 'a;
}

// TODO: find a way to avoid duplication

macro_rules! impl_visit {
    ($type:ty => $visited:ty, $expr:tt) => {
        impl Visit<$visited> for $type {
            fn visit<'a>(&'a self) -> impl Iterator<Item = &'a $visited>
            where
                $visited: 'a,
            {
                #[allow(unused)]
                fn visit<'a, T: Visit<U>, U: 'a>(expr: &'a T) -> impl Iterator<Item = &'a U> {
                    Visit::<U>::visit(expr)
                }

                #[allow(unused)]
                fn recurse(expr: &$type) -> impl Iterator<Item = &$visited> {
                    Visit::<$visited>::visit(expr)
                }

                let root: &$type = self;
                query!(root.$expr)
            }
            fn visit_mut<'a>(&'a mut self) -> impl Iterator<Item = &'a mut $visited>
            where
                $visited: 'a,
            {
                #[allow(unused)]
                fn visit<'a, T: Visit<U>, U: 'a>(
                    expr: &'a mut T,
                ) -> impl Iterator<Item = &'a mut U> {
                    Visit::<U>::visit_mut(expr)
                }

                #[allow(unused)]
                fn recurse(expr: &mut $type) -> impl Iterator<Item = &mut $visited> {
                    Visit::<$visited>::visit_mut(expr)
                }

                let root: &mut $type = self;
                query_mut!(root.$expr)
            }
        }
    };
}

impl_visit! { Expression => ExpressionNode,
    {
        Expression::Parenthesized.expression.(x => recurse(x)),
        Expression::NamedComponent.base.(x => recurse(x)),
        Expression::Indexing.{
            base.(x => recurse(x)),
            index.(x => recurse(x)),
        },
        Expression::Unary.operand.(x => recurse(x)),
        Expression::Binary.{
            left.(x => recurse(x)),
            right.(x => recurse(x)),
        },
        Expression::FunctionCall.arguments.[].(x => recurse(x)),
    }
}

impl_visit! { Expression => TypeExpression,
    {
        Expression::Parenthesized.expression.(x => recurse(x)),
        Expression::NamedComponent.base.(x => recurse(x)),
        Expression::Indexing.{ base.(x => recurse(x)), index.(x => recurse(x)) },
        Expression::Unary.operand.(x => recurse(x)),
        Expression::Binary.{ left.(x => recurse(x)), right.(x => recurse(x)) },
        Expression::FunctionCall.{
            ty,
            arguments.[].(x => recurse(x))
        },
        Expression::TypeOrIdentifier,
    }
}

impl_visit! { Statement => Attributes,
    {
        Statement::Compound.{ attributes, statements.[].(x => recurse(x)) },
        Statement::If.{
            attributes,
            else_if_clauses.[].body.statements.[].(x => recurse(x)),
            else_clause.[].body.statements.[].(x => recurse(x)),
        },
        Statement::Switch.{
            attributes,
            clauses.[].{ attributes, body.statements.[].(x => recurse(x)) },
        },
        Statement::Loop.{
            attributes,
            body.statements.[].(x => recurse(x)),
            continuing.[].{
                attributes,
                body.statements.[].(x => recurse(x)),
                break_if.[].{ attributes }
            },
        },
        Statement::For.{
            attributes,
            body.statements.[].(x => recurse(x)),
        },
        Statement::While.{
            attributes,
            body.statements.[].(x => recurse(x)),
        },
        Statement::Break.attributes,
        Statement::Continue.attributes,
        Statement::Return.attributes,
        Statement::Discard.attributes,
        Statement::FunctionCall.attributes,
        Statement::ConstAssert.attributes,
        Statement::Declaration.attributes,
    }
}

impl_visit! { Statement => TypeExpression,
    {
        Statement::Compound.{
            attributes.[].(x => visit::<Attribute, TypeExpression>(x)),
            statements.[].(x => recurse(x)),
        },
        Statement::Assignment.{
            #[cfg(feature = "attributes")]
            attributes.[].(x => visit::<Attribute, TypeExpression>(x)),
            lhs.(x => visit::<Expression, TypeExpression>(x)),
            rhs.(x => visit::<Expression, TypeExpression>(x)),
        },
        Statement::Increment.{
            #[cfg(feature = "attributes")]
            attributes.[].(x => visit::<Attribute, TypeExpression>(x)),
            expression.(x => visit::<Expression, TypeExpression>(x)),
        },
        Statement::Decrement.{
            #[cfg(feature = "attributes")]
            attributes.[].(x => visit::<Attribute, TypeExpression>(x)),
            expression.(x => visit::<Expression, TypeExpression>(x)),
        },
        Statement::If.{
            attributes.[].(x => visit::<Attribute, TypeExpression>(x)),
            if_clause.{
                expression.(x => visit::<Expression, TypeExpression>(x)),
                body.{
                    attributes.[].(x => visit::<Attribute, TypeExpression>(x)),
                    statements.[].(x => recurse(x)),
                }
            },
            else_if_clauses.[].{
                #[cfg(feature = "attributes")]
                attributes.[].(x => visit::<Attribute, TypeExpression>(x)),
                expression.(x => visit::<Expression, TypeExpression>(x)),
                body.{
                    attributes.[].(x => visit::<Attribute, TypeExpression>(x)),
                    statements.[].(x => recurse(x)),
                }
            },
            else_clause.[].{
                #[cfg(feature = "attributes")]
                attributes.[].(x => visit::<Attribute, TypeExpression>(x)),
                body.{
                    attributes.[].(x => visit::<Attribute, TypeExpression>(x)),
                    statements.[].(x => recurse(x)),
                }
            },
        },
        Statement::Switch.{
            attributes.[].(x => visit::<Attribute, TypeExpression>(x)),
            expression.(x => visit::<Expression, TypeExpression>(x)),
            body_attributes.[].(x => visit::<Attribute, TypeExpression>(x)),
            clauses.[].{
                attributes.[].(x => visit::<Attribute, TypeExpression>(x)),
                case_selectors.[].CaseSelector::Expression.(x => visit::<Expression, TypeExpression>(x)),
                body.{
                    attributes.[].(x => visit::<Attribute, TypeExpression>(x)),
                    statements.[].(x => recurse(x)),
                }
            }
        },
        Statement::Loop.{
            attributes.[].(x => visit::<Attribute, TypeExpression>(x)),
            body.{
                attributes.[].(x => visit::<Attribute, TypeExpression>(x)),
                statements.[].(x => recurse(x)),
            },
            continuing.[].{
                #[cfg(feature = "attributes")]
                attributes.[].(x => visit::<Attribute, TypeExpression>(x)),
                body.{
                    attributes.[].(x => visit::<Attribute, TypeExpression>(x)),
                    statements.[].(x => recurse(x)),
                },
                break_if.[].{
                    #[cfg(feature = "attributes")]
                    attributes.[].(x => visit::<Attribute, TypeExpression>(x)),
                    expression.(x => visit::<Expression, TypeExpression>(x)),
                }
            }
        },
        Statement::For.{
            attributes.[].(x => visit::<Attribute, TypeExpression>(x)),
            initializer.[].(x => recurse(x)),
            condition.[].(x => visit::<Expression, TypeExpression>(x)),
            update.[].(x => recurse(x)),
            body.{
                attributes.[].(x => visit::<Attribute, TypeExpression>(x)),
                statements.[].(x => recurse(x)),
            },
        },
        Statement::While.{
            attributes.[].(x => visit::<Attribute, TypeExpression>(x)),
            condition.(x => visit::<Expression, TypeExpression>(x)),
            body.{
                attributes.[].(x => visit::<Attribute, TypeExpression>(x)),
                statements.[].(x => recurse(x)),
            },
        },
        #[cfg(feature = "attributes")]
        Statement::Break.{
            attributes.[].(x => visit::<Attribute, TypeExpression>(x)),
        },
        #[cfg(feature = "attributes")]
        Statement::Continue.{
            attributes.[].(x => visit::<Attribute, TypeExpression>(x)),
        },
        Statement::Return.{
            #[cfg(feature = "attributes")]
            attributes.[].(x => visit::<Attribute, TypeExpression>(x)),
            expression.[].(x => visit::<Expression, TypeExpression>(x)),
        },
        #[cfg(feature = "attributes")]
        Statement::Discard.{
            attributes.[].(x => visit::<Attribute, TypeExpression>(x)),
        },
        Statement::FunctionCall.{
            #[cfg(feature = "attributes")]
            attributes.[].(x => visit::<Attribute, TypeExpression>(x)),
            call.{
                ty,
                arguments.[].(x => visit::<Expression, TypeExpression>(x)),
            }
        },
        Statement::ConstAssert.{
            #[cfg(feature = "attributes")]
            attributes.[].(x => visit::<Attribute, TypeExpression>(x)),
            expression.(x => visit::<Expression, TypeExpression>(x)),
        },
        Statement::Declaration.{
            attributes.[].(x => visit::<Attribute, TypeExpression>(x)),
            ty.[],
            initializer.[].(x => visit::<Expression, TypeExpression>(x)),
        },
    }
}

impl_visit! { Statement => ExpressionNode,
    {
        Statement::Compound.statements.[].(x => recurse(x)),
        Statement::Assignment.{ lhs, rhs },
        Statement::Increment.expression,
        Statement::Decrement.expression,
        Statement::If.{
            if_clause.{
                expression,
                body.statements.[].(x => recurse(x)),
            },
            else_if_clauses.[].{
                expression,
                body.statements.[].(x => recurse(x)),
            }
        },
        Statement::Switch.{
            expression,
            clauses.[].{
                case_selectors.[].CaseSelector::Expression,
                body.statements.[].(x => recurse(x)),
            }
        },
        Statement::Loop.{
            body.statements.[].(x => recurse(x)),
            continuing.[].{
                body.statements.[].(x => recurse(x)),
                break_if.[].expression,
            }
        },
        Statement::For.{
            initializer.[].(x => recurse(x)),
            condition.[],
            update.[].(x => recurse(x)),
            body.statements.[].(x => recurse(x)),
        },
        Statement::While.{
            condition,
            body.statements.[].(x => recurse(x)),
        },
        Statement::Return.expression.[],
        Statement::FunctionCall.call.arguments.[],
        Statement::ConstAssert.expression,
        Statement::Declaration.initializer.[],
    }
}

impl_visit! { Statement => StatementNode,
    {
        Statement::Compound.statements.[],
        Statement::If.{
            if_clause.body.statements.[],
            else_if_clauses.[].body.statements.[],
        },
        Statement::Switch.clauses.[].body.statements.[],
        Statement::Loop.{
            body.statements.[],
            continuing.[].body.statements.[],
        },
        Statement::For.{
            initializer.[],
            update.[],
            body.statements.[],
        },
        Statement::While.body.statements.[],
    }
}

impl_visit! { Attribute => TypeExpression,
    {
        Attribute::Align.(x => visit::<Expression, TypeExpression>(x)),
        Attribute::Binding.(x => visit::<Expression, TypeExpression>(x)),
        Attribute::BlendSrc.(x => visit::<Expression, TypeExpression>(x)),
        Attribute::Group.(x => visit::<Expression, TypeExpression>(x)),
        Attribute::Id.(x => visit::<Expression, TypeExpression>(x)),
        Attribute::Location.(x => visit::<Expression, TypeExpression>(x)),
        Attribute::Size.(x => visit::<Expression, TypeExpression>(x)),
        Attribute::WorkgroupSize.{
            x.(x => visit::<Expression, TypeExpression>(x)),
            y.[].(x => visit::<Expression, TypeExpression>(x)),
            z.[].(x => visit::<Expression, TypeExpression>(x)),
        },
        #[cfg(feature = "generics")]
        Attribute::Type.variants.[],
        Attribute::Custom.arguments.[].[].(x => visit::<Expression, TypeExpression>(x))
    }
}

impl_visit! { TranslationUnit => ExpressionNode,
    {
        global_declarations.[].{
            GlobalDeclaration::Declaration.{
                initializer.[],
            },
            GlobalDeclaration::Function.{
                body.statements.[].(x => visit::<Statement, ExpressionNode>(x)),
            }
        }
    }
}

impl_visit! { TranslationUnit => StatementNode,
    {
        global_declarations.[].GlobalDeclaration::Function.body.statements.[]
    }
}

impl_visit! { TranslationUnit => Attributes,
    {
        imports.[].attributes,
        global_directives.[].{
            GlobalDirective::Diagnostic.attributes,
            GlobalDirective::Enable.attributes,
            GlobalDirective::Requires.attributes,
        },
        global_declarations.[].{
            GlobalDeclaration::Declaration.attributes,
            GlobalDeclaration::TypeAlias.attributes,
            GlobalDeclaration::Struct.{
                attributes,
                members.[].attributes,
            },
            GlobalDeclaration::Function.{
                attributes,
                parameters.[].attributes,
                body.{ attributes, statements.[].(x => visit::<Statement, Attributes>(x)) }
            },
            GlobalDeclaration::ConstAssert.attributes,
        }
    }
}

impl_visit! { TranslationUnit => TypeExpression,
    {
        global_declarations.[].(x => visit::<GlobalDeclaration, TypeExpression>(x))
    }
}

impl_visit! { GlobalDeclaration => TypeExpression,
    {
        GlobalDeclaration::Declaration.(x => visit::<Declaration, TypeExpression>(x)),
        GlobalDeclaration::TypeAlias.(x => visit::<TypeAlias, TypeExpression>(x)),
        GlobalDeclaration::Struct.(x => visit::<Struct, TypeExpression>(x)),
        GlobalDeclaration::Function.(x => visit::<Function, TypeExpression>(x)),
        GlobalDeclaration::ConstAssert.(x => visit::<ConstAssert, TypeExpression>(x))
    }
}

impl_visit! { Declaration => TypeExpression,
    {
        attributes.[].(x => visit::<Attribute, TypeExpression>(x)),
        ty.[],
        initializer.[].(x => visit::<Expression, TypeExpression>(x)),
    }
}
impl_visit! { TypeAlias => TypeExpression,
    {
        #[cfg(feature = "attributes")]
        attributes.[].(x => visit::<Attribute, TypeExpression>(x)),
        ty,
    }
}
impl_visit! { Struct => TypeExpression,
    {
        #[cfg(feature = "attributes")]
        attributes.[].(x => visit::<Attribute, TypeExpression>(x)),
        members.[].{
            attributes.[].(x => visit::<Attribute, TypeExpression>(x)),
            ty,
        },
    }
}
impl_visit! { Function => TypeExpression,
    {
        attributes.[].(x => visit::<Attribute, TypeExpression>(x)),
        parameters.[].{
            attributes.[].(x => visit::<Attribute, TypeExpression>(x)),
            ty,
        },
        return_attributes.[].(x => visit::<Attribute, TypeExpression>(x)),
        return_type.[],
        body.{
            attributes.[].(x => visit::<Attribute, TypeExpression>(x)),
            statements.[].(x => visit::<Statement, ExpressionNode>(x)).(x => visit::<Expression, TypeExpression>(x)),
        }
    }
}
impl_visit! { ConstAssert => TypeExpression,
    {
        #[cfg(feature = "attributes")]
        attributes.[].(x => visit::<Attribute, TypeExpression>(x)),
        expression.(x => visit::<Expression, TypeExpression>(x)),
    }
}
