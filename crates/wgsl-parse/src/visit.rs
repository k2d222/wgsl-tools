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
    ($type:ty => $visited:ty, $expr:tt) => {
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
    ($type:ty => $visited:ty, $expr:tt) => {
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

impl_visit! { Expression => ExpressionNode,
    {
        Expression::Parenthesized.expression.(x => Visit::<ExpressionNode>::visit(&**x)),
        Expression::NamedComponent.base.(x => Visit::<ExpressionNode>::visit(&**x)),
        Expression::Indexing.{
            base.(x => Visit::<ExpressionNode>::visit(&**x)),
            index.(x => Visit::<ExpressionNode>::visit(&**x)),
        },
        Expression::Unary.operand.(x => Visit::<ExpressionNode>::visit(&**x)),
        Expression::Binary.{
            left.(x => Visit::<ExpressionNode>::visit(&**x)),
            right.(x => Visit::<ExpressionNode>::visit(&**x)),
        },
        Expression::FunctionCall.arguments.[].(x => Visit::<ExpressionNode>::visit(&**x)),
    }
}

impl_visit_mut! { Expression => ExpressionNode,
    {
        Expression::Parenthesized.expression.(x => VisitMut::<ExpressionNode>::visit_mut(&mut **x)),
        Expression::NamedComponent.base.(x => VisitMut::<ExpressionNode>::visit_mut(&mut **x)),
        Expression::Indexing.{
            base.(x => VisitMut::<ExpressionNode>::visit_mut(&mut **x)),
            index.(x => VisitMut::<ExpressionNode>::visit_mut(&mut **x)),
        },
        Expression::Unary.operand.(x => VisitMut::<ExpressionNode>::visit_mut(&mut **x)),
        Expression::Binary.{
            left.(x => VisitMut::<ExpressionNode>::visit_mut(&mut **x)),
            right.(x => VisitMut::<ExpressionNode>::visit_mut(&mut **x)),
        },
        Expression::FunctionCall.arguments.[].(x => VisitMut::<ExpressionNode>::visit_mut(&mut **x)),
    }
}

impl_visit! { Expression => TypeExpression,
    {
        Expression::Parenthesized.expression.(x => Visit::<TypeExpression>::visit(&**x)),
        Expression::NamedComponent.base.(x => Visit::<TypeExpression>::visit(&**x)),
        Expression::Indexing.{ base.(x => Visit::<TypeExpression>::visit(&**x)), index.(x => Visit::<TypeExpression>::visit(&**x)) },
        Expression::Unary.operand.(x => Visit::<TypeExpression>::visit(&**x)),
        Expression::Binary.{ left.(x => Visit::<TypeExpression>::visit(&**x)), right.(x => Visit::<TypeExpression>::visit(&**x)) },
        Expression::FunctionCall.arguments.[].(x => Visit::<TypeExpression>::visit(&**x)),
        Expression::TypeOrIdentifier,
    }
}

impl_visit_mut! { Expression => TypeExpression,
    {
        Expression::Parenthesized.expression.(x => VisitMut::<TypeExpression>::visit_mut(&mut **x)),
        Expression::NamedComponent.base.(x => VisitMut::<TypeExpression>::visit_mut(&mut **x)),
        Expression::Indexing.{ base.(x => VisitMut::<TypeExpression>::visit_mut(&mut **x)), index.(x => VisitMut::<TypeExpression>::visit_mut(&mut **x)) },
        Expression::Unary.operand.(x => VisitMut::<TypeExpression>::visit_mut(&mut **x)),
        Expression::Binary.{ left.(x => VisitMut::<TypeExpression>::visit_mut(&mut **x)), right.(x => VisitMut::<TypeExpression>::visit_mut(&mut **x)) },
        Expression::FunctionCall.arguments.[].(x => VisitMut::<TypeExpression>::visit_mut(&mut **x)),
        Expression::TypeOrIdentifier,
    }
}

impl_visit! { Statement => ExpressionNode,
    {
        Statement::Compound.statements.[].(x => Visit::<ExpressionNode>::visit(&**x)),
        Statement::Assignment.{ lhs, rhs },
        Statement::Increment.expression,
        Statement::Decrement.expression,
        Statement::If.{
            if_clause.{
                expression,
                body.statements.[].(x => Visit::<ExpressionNode>::visit(&**x)),
            },
            else_if_clauses.[].{
                expression,
                body.statements.[].(x => Visit::<ExpressionNode>::visit(&**x)),
            }
        },
        Statement::Switch.{
            expression,
            clauses.[].{
                case_selectors.[].CaseSelector::Expression,
                body.statements.[].(x => Visit::<ExpressionNode>::visit(&**x)),
            }
        },
        Statement::Loop.{
            body.statements.[].(x => Visit::<ExpressionNode>::visit(&**x)),
            continuing.[].{
                body.statements.[].(x => Visit::<ExpressionNode>::visit(&**x)),
                break_if.[].expression,
            }
        },
        Statement::For.{
            initializer.[].(x => Visit::<ExpressionNode>::visit(&**x)),
            condition.[],
            update.[].(x => Visit::<ExpressionNode>::visit(&**x)),
            body.statements.[].(x => Visit::<ExpressionNode>::visit(&**x)),
        },
        Statement::While.{
            condition,
            body.statements.[].(x => Visit::<ExpressionNode>::visit(&**x)),
        },
        Statement::Return.expression.[],
        Statement::FunctionCall.call.arguments.[],
        Statement::ConstAssert.expression,
        Statement::Declaration.initializer.[],
    }
}

impl_visit_mut! { Statement => ExpressionNode,
    {
        Statement::Compound.statements.[].(x => VisitMut::<ExpressionNode>::visit_mut(&mut **x)),
        Statement::Assignment.{ lhs, rhs },
        Statement::Increment.expression,
        Statement::Decrement.expression,
        Statement::If.{
            if_clause.{
                expression,
                body.statements.[].(x => VisitMut::<ExpressionNode>::visit_mut(&mut **x)),
            },
            else_if_clauses.[].{
                expression,
                body.statements.[].(x => VisitMut::<ExpressionNode>::visit_mut(&mut **x)),
            }
        },
        Statement::Switch.{
            expression,
            clauses.[].{
                case_selectors.[].CaseSelector::Expression,
                body.statements.[].(x => VisitMut::<ExpressionNode>::visit_mut(&mut **x)),
            }
        },
        Statement::Loop.{
            body.statements.[].(x => VisitMut::<ExpressionNode>::visit_mut(&mut **x)),
            continuing.[].{
                body.statements.[].(x => VisitMut::<ExpressionNode>::visit_mut(&mut **x)),
                break_if.[].expression,
            }
        },
        Statement::For.{
            initializer.[].(x => VisitMut::<ExpressionNode>::visit_mut(&mut **x)),
            condition.[],
            update.[].(x => VisitMut::<ExpressionNode>::visit_mut(&mut **x)),
            body.statements.[].(x => VisitMut::<ExpressionNode>::visit_mut(&mut **x)),
        },
        Statement::While.{
            condition,
            body.statements.[].(x => VisitMut::<ExpressionNode>::visit_mut(&mut **x)),
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

impl_visit_mut! { Statement => StatementNode,
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

impl_visit! { TranslationUnit => ExpressionNode,
    {
        global_declarations.[].{
            GlobalDeclaration::Declaration.{
                initializer.[],
            },
            GlobalDeclaration::Function.{
                body.statements.[].(x => Visit::<ExpressionNode>::visit(&**x)),
            }
        }
    }
}

impl_visit_mut! { TranslationUnit => ExpressionNode,
    {
        global_declarations.[].{
            GlobalDeclaration::Declaration.{
                initializer.[],
            },
            GlobalDeclaration::Function.{
                body.statements.[].(x => VisitMut::<ExpressionNode>::visit_mut(&mut **x)),
            }
        }
    }
}

impl_visit! { TranslationUnit => StatementNode,
    {
        global_declarations.[].GlobalDeclaration::Function.body.statements.[]
    }
}

impl_visit_mut! { TranslationUnit => StatementNode,
    {
        global_declarations.[].GlobalDeclaration::Function.body.statements.[]
    }
}

impl_visit! { TranslationUnit => TypeExpression,
    {
        global_declarations.[].{
            GlobalDeclaration::Declaration.{
                ty.[],
                initializer.[].(x => Visit::<TypeExpression>::visit(&**x)),
            },
            GlobalDeclaration::TypeAlias.ty,
            GlobalDeclaration::Struct.members.[].ty,
            GlobalDeclaration::Function.{
                parameters.[].ty,
                return_type.[],
                body.statements.[].(x => Visit::<ExpressionNode>::visit(&**x)).(x => Visit::<TypeExpression>::visit(&**x)),
            }
        }
    }
}

impl_visit_mut! { TranslationUnit => TypeExpression,
    {
        global_declarations.[].{
            GlobalDeclaration::Declaration.{
                ty.[],
                initializer.[].(x => VisitMut::<TypeExpression>::visit_mut(&mut **x)),
            },
            GlobalDeclaration::TypeAlias.ty,
            GlobalDeclaration::Struct.members.[].ty,
            GlobalDeclaration::Function.{
                parameters.[].ty,
                return_type.[],
                body.statements.[].(x => VisitMut::<ExpressionNode>::visit_mut(&mut **x)).(x => VisitMut::<TypeExpression>::visit_mut(&mut **x)),
            }
        }
    }
}
