use wgsl_parse_macros::{query, query_mut};

use crate::syntax::*;

pub trait Visit<T> {
    fn visit<'a>(&'a self) -> impl Iterator<Item = &'a T>
    where
        T: 'a;
}

pub trait VisitMut<T> {
    fn visit_mut<'a>(&'a mut self) -> impl Iterator<Item = &'a mut T>
    where
        T: 'a;
}

// TODO: find a way to avoid duplication

macro_rules! impl_visit {
    ($type:ident => $visited:ident, $expr:tt) => {
        impl Visit<$visited> for $type {
            fn visit<'a>(&'a self) -> impl Iterator<Item = &'a $visited>
            where
                $visited: 'a,
            {
                let root: &$type = self;
                query!(root.$expr)
            }
        }
    };
}

macro_rules! impl_visit_mut {
    ($type:ident => $visited:ident, $expr:tt) => {
        impl VisitMut<$visited> for $type {
            fn visit_mut<'a>(&'a mut self) -> impl Iterator<Item = &'a mut $visited>
            where
                $visited: 'a,
            {
                let root: &mut $type = self;
                query_mut!(root.$expr)
            }
        }
    };
}

impl_visit! { Expression => Expression,
    {
        Expression::Parenthesized.expression.(x => Visit::<Expression>::visit(&**x)),
        Expression::NamedComponent.base.(x => Visit::<Expression>::visit(&**x)),
        Expression::Indexing.{
            base.(x => Visit::<Expression>::visit(&**x)),
            index.(x => Visit::<Expression>::visit(&**x)),
        },
        Expression::Unary.operand.(x => Visit::<Expression>::visit(&**x)),
        Expression::Binary.{
            left.(x => Visit::<Expression>::visit(&**x)),
            right.(x => Visit::<Expression>::visit(&**x)),
        },
        Expression::FunctionCall.arguments.[].(x => Visit::<Expression>::visit(x)),
    }
}

impl_visit_mut! { Expression => Expression,
    {
        Expression::Parenthesized.expression.(x => VisitMut::<Expression>::visit_mut(&mut **x)),
        Expression::NamedComponent.base.(x => VisitMut::<Expression>::visit_mut(&mut **x)),
        Expression::Indexing.{
            base.(x => VisitMut::<Expression>::visit_mut(&mut **x)),
            index.(x => VisitMut::<Expression>::visit_mut(&mut **x)),
        },
        Expression::Unary.operand.(x => VisitMut::<Expression>::visit_mut(&mut **x)),
        Expression::Binary.{
            left.(x => VisitMut::<Expression>::visit_mut(&mut **x)),
            right.(x => VisitMut::<Expression>::visit_mut(&mut **x)),
        },
        Expression::FunctionCall.arguments.[].(x => VisitMut::<Expression>::visit_mut(x)),
    }
}

impl_visit! { Expression => TypeExpression,
    {
        Expression::Parenthesized.expression.(x => Visit::<TypeExpression>::visit(&**x)),
        Expression::NamedComponent.base.(x => Visit::<TypeExpression>::visit(&**x)),
        Expression::Indexing.{ base.(x => Visit::<TypeExpression>::visit(&**x)), index.(x => Visit::<TypeExpression>::visit(&**x)) },
        Expression::Unary.operand.(x => Visit::<TypeExpression>::visit(&**x)),
        Expression::Binary.{ left.(x => Visit::<TypeExpression>::visit(&**x)), right.(x => Visit::<TypeExpression>::visit(&**x)) },
        Expression::FunctionCall.arguments.[].(x => Visit::<TypeExpression>::visit(x)),
        Expression::Type,
    }
}

impl_visit_mut! { Expression => TypeExpression,
    {
        Expression::Parenthesized.expression.(x => VisitMut::<TypeExpression>::visit_mut(&mut **x)),
        Expression::NamedComponent.base.(x => VisitMut::<TypeExpression>::visit_mut(&mut **x)),
        Expression::Indexing.{ base.(x => VisitMut::<TypeExpression>::visit_mut(&mut **x)), index.(x => VisitMut::<TypeExpression>::visit_mut(&mut **x)) },
        Expression::Unary.operand.(x => VisitMut::<TypeExpression>::visit_mut(&mut **x)),
        Expression::Binary.{ left.(x => VisitMut::<TypeExpression>::visit_mut(&mut **x)), right.(x => VisitMut::<TypeExpression>::visit_mut(&mut **x)) },
        Expression::FunctionCall.arguments.[].(x => VisitMut::<TypeExpression>::visit_mut(x)),
        Expression::Type,
    }
}

impl_visit! { Statement => Expression,
    {
        Statement::Compound.statements.[].(x => Visit::<Expression>::visit(x)),
        Statement::Assignment.{ lhs, rhs },
        Statement::Increment.expression,
        Statement::Decrement.expression,
        Statement::If.{
            if_clause.{
                0,
                1.statements.[].(x => Visit::<Expression>::visit(x)),
            },
            else_if_clauses.[].{
                0,
                1.statements.[].(x => Visit::<Expression>::visit(x)),
            }
        },
        Statement::Switch.{
            expression,
            clauses.[].{
                case_selectors.[].CaseSelector::Expression,
                body.statements.[].(x => Visit::<Expression>::visit(x)),
            }
        },
        Statement::Loop.{
            body.statements.[].(x => Visit::<Expression>::visit(x)),
            continuing.[].{
                body.statements.[].(x => Visit::<Expression>::visit(x)),
                break_if.[].expression,
            }
        },
        Statement::For.{
            initializer.[].(x => Visit::<Expression>::visit(&**x)),
            condition.[],
            update.[].(x => Visit::<Expression>::visit(&**x)),
            body.statements.[].(x => Visit::<Expression>::visit(x)),
        },
        Statement::While.{
            condition,
            body.statements.[].(x => Visit::<Expression>::visit(x)),
        },
        Statement::Return.expression.[],
        Statement::FunctionCall.arguments.[],
        Statement::ConstAssert.expression,
        Statement::Declaration.initializer.[],
    }
}

impl_visit_mut! { Statement => Expression,
    {
        Statement::Compound.statements.[].(x => VisitMut::<Expression>::visit_mut(x)),
        Statement::Assignment.{ lhs, rhs },
        Statement::Increment.expression,
        Statement::Decrement.expression,
        Statement::If.{
            if_clause.{
                0,
                1.statements.[].(x => VisitMut::<Expression>::visit_mut(x)),
            },
            else_if_clauses.[].{
                0,
                1.statements.[].(x => VisitMut::<Expression>::visit_mut(x)),
            }
        },
        Statement::Switch.{
            expression,
            clauses.[].{
                case_selectors.[].CaseSelector::Expression,
                body.statements.[].(x => VisitMut::<Expression>::visit_mut(x)),
            }
        },
        Statement::Loop.{
            body.statements.[].(x => VisitMut::<Expression>::visit_mut(x)),
            continuing.[].{
                body.statements.[].(x => VisitMut::<Expression>::visit_mut(x)),
                break_if.[].expression,
            }
        },
        Statement::For.{
            initializer.[].(x => VisitMut::<Expression>::visit_mut(&mut **x)),
            condition.[],
            update.[].(x => VisitMut::<Expression>::visit_mut(&mut **x)),
            body.statements.[].(x => VisitMut::<Expression>::visit_mut(x)),
        },
        Statement::While.{
            condition,
            body.statements.[].(x => VisitMut::<Expression>::visit_mut(x)),
        },
        Statement::Return.expression.[],
        Statement::FunctionCall.arguments.[],
        Statement::ConstAssert.expression,
        Statement::Declaration.initializer.[],
    }
}

impl_visit! { TranslationUnit => TypeExpression,
    {
        global_declarations.[].{
            GlobalDeclaration::Declaration.{
                ty.[],
                initializer.[].(x => Visit::<TypeExpression>::visit(x)),
            },
            GlobalDeclaration::TypeAlias.ty,
            GlobalDeclaration::Struct.members.[].ty,
            GlobalDeclaration::Function.{
                parameters.[].ty,
                return_type.[],
                body.statements.[].(x => Visit::<Expression>::visit(x)).(x => Visit::<TypeExpression>::visit(x)),
            }
        }
    }
}

impl_visit_mut! { TranslationUnit => TypeExpression,
    {
        global_declarations.[].{
            GlobalDeclaration::Declaration.{
                ty.[],
                initializer.[].(x => VisitMut::<TypeExpression>::visit_mut(x)),
            },
            GlobalDeclaration::TypeAlias.ty,
            GlobalDeclaration::Struct.members.[].ty,
            GlobalDeclaration::Function.{
                parameters.[].ty,
                return_type.[],
                body.statements.[].(x => VisitMut::<Expression>::visit_mut(x)).(x => VisitMut::<TypeExpression>::visit_mut(x)),
            }
        }
    }
}
