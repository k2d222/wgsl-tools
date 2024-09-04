use std::hash::DefaultHasher;
use std::hash::Hash;
use std::hash::Hasher;

use itertools::chain;
use wgsl_parse::syntax::*;
use wgsl_parse::visit_fields;
use wgsl_parse::visit_variants;
use wgsl_parse_macros::query;

use crate::resolve::FileResource;
use crate::resolve::{FileResolver, ImportError, Module, Resolver};

pub trait Mangler<R: Resolver> {
    fn mangle(&self, resource: &R::Resource, item: &str) -> String;
}

#[derive(Default, Clone, Debug)]
pub struct FileManglerHash;

impl Mangler<FileResolver> for FileManglerHash {
    fn mangle(&self, resource: &FileResource, item: &str) -> String {
        let mut hasher = DefaultHasher::new();
        resource.hash(&mut hasher);
        item.hash(&mut hasher);
        let hash = hasher.finish();
        format!("{item}_{hash}")
    }
}

fn mod_visit_exprs(module: &mut TranslationUnit) -> impl Iterator<Item = &mut Expression> {
    module
        .visit_mut()
        .global_declarations()
        .each()
        .flat_map(visit_variants! {
            GlobalDeclaration::Declaration(x) => x.visit_mut().initializer().some(),
            GlobalDeclaration::Function(x) => visit_fields!(x, {
                body => body.visit_mut().statements().each().flat_map(stat_visit_exprs),
            }),
        })
}

fn mod_visit_type_exprs(module: &mut TranslationUnit) -> impl Iterator<Item = &mut TypeExpression> {
    module
        .visit_mut()
        .global_declarations()
        .each()
        .flat_map(visit_variants! {
            GlobalDeclaration::Declaration(x) => x.typ.visit_mut().some(),
            GlobalDeclaration::TypeAlias(x) => x.typ.visit_mut(),
            GlobalDeclaration::Struct(x) => x.members.visit_mut().each().typ(),
            GlobalDeclaration::Function(x) => visit_fields!(x, {
                parameters => parameters.visit_mut().each().typ(),
                return_type => return_type.visit_mut().some(),
            }),
        })
}

fn expr_visit_exprs(expr: &mut Expression) -> impl Iterator<Item = &mut Expression> {
    visit_variants!(expr, {
        Expression::Parenthesized(x) => x.visit_mut(),
        Expression::NamedComponent(x) => expr_visit_exprs(&mut x.base),
        Expression::Indexing(x) => visit_fields!(x, {
            base => expr_visit_exprs(base),
            index => expr_visit_exprs(index),
        }),
        Expression::Unary(x) => expr_visit_exprs(&mut x.operand),
        Expression::Binary(x) => visit_fields!(x, {
            left => expr_visit_exprs(left),
            right => expr_visit_exprs(right),
        }),
        Expression::FunctionCall(x) => x.arguments.visit_mut().each().flat_map(expr_visit_exprs),
    })
}

fn stat_visit_exprs(stat: &mut Statement) -> impl Iterator<Item = &mut Expression> {
    visit_variants!(stat, {
        Statement::Compound(x) => x.statements.visit_mut().each().flat_map(stat_visit_exprs),
        Statement::Assignment(x) => visit_fields!(x, {
            lhs => lhs.visit_mut(),
            rhs => rhs.visit_mut(),
        }),
        Statement::Increment(x) => x.visit_mut(),
        Statement::Decrement(x) => x.visit_mut(),
        Statement::If(x) => visit_fields!(x, {
            if_clause => {
                let (expr, stat) = if_clause;
                chain!(expr.visit_mut(), stat.statements.visit_mut().each().flat_map(stat_visit_exprs))
            },
            else_if_clauses => else_if_clauses.visit_mut().each().flat_map(|(expr, stat)| {
                chain!(expr.visit_mut(), stat.statements.visit_mut().each().flat_map(stat_visit_exprs))
            }),
            else_clause => else_clause.visit_mut().some().statements().each().flat_map(stat_visit_exprs),
        }),
        Statement::Switch(x) => visit_fields!(x , {
            expression => expression.visit_mut(),
            clauses => clauses.visit_mut().each().flat_map(visit_fields! {
                case_selectors => case_selectors.visit_mut().each().match_expression(),
                body => body.statements.visit_mut().each().flat_map(stat_visit_exprs),
            }),
        }),
        Statement::Loop(x) => visit_fields!(x, {
            body => body.statements.visit_mut().each().flat_map(stat_visit_exprs),
            continuing => continuing.visit_mut().some().flat_map(visit_fields! {
                body => body.statements.visit_mut().each().flat_map(stat_visit_exprs),
                break_if => break_if.visit_mut().some(),
            }),
        }),
        Statement::For(x) => visit_fields!(x, {
            initializer => initializer.visit_mut().some().flat_map(|x| stat_visit_exprs(x)),
            condition => condition.visit_mut().some(),
            update => update.visit_mut().some().flat_map(|x| stat_visit_exprs(x)),
            body => body.statements.visit_mut().each().flat_map(stat_visit_exprs),
        }),
        Statement::While(x) => visit_fields!(x, {
            condition => condition.visit_mut(),
            body => body.statements.visit_mut().each().flat_map(stat_visit_exprs),
        }),
        Statement::Return(x) => x.visit_mut().some(),
        Statement::FunctionCall(x) => x.arguments.visit_mut().each(),
        Statement::ConstAssert(x) => x.expression.visit_mut(),
        Statement::Declaration(x) => x.visit_mut().initializer().some(),
    })
}

fn replace_imported_ident(module: &mut TranslationUnit, old_ident: &str, new_ident: &str) {
    fn expr_visit_exprs(expr: &Expression) -> impl Iterator<Item = &Expression> {
        query!(
            expr.{
                Expression::Parenthesized.(x => expr_visit_exprs(x)),
                Expression::NamedComponent.base.(x => expr_visit_exprs(x)),
                Expression::Indexing.{
                    base.(x => expr_visit_exprs(x)),
                    index.(x => expr_visit_exprs(x)),
                },
                Expression::Unary.operand.(x => expr_visit_exprs(x)),
                Expression::Binary.{
                    left.(x => expr_visit_exprs(x)),
                    right.(x => expr_visit_exprs(x)),
                },
                Expression::FunctionCall.arguments.[].(expr_visit_exprs),
            }
        )
    }

    fn stat_visit_exprs(statement: &Statement) -> impl Iterator<Item = &Expression> {
        query!(
            statement.{
                Statement::Compound.statements.[].(stat_visit_exprs),
                Statement::Assignment.{ lhs, rhs },
                Statement::Increment,
                Statement::Decrement,
                Statement::If.{
                    if_clause.{
                        0,
                        1.statements.[].(stat_visit_exprs),
                    },
                    else_if_clauses.[].{
                        0,
                        1.statements.[].(stat_visit_exprs),
                    }
                },
                Statement::Switch.{
                    expression,
                    clauses.[].{
                        case_selectors.[].CaseSelector::Expression,
                        body.statements.[].(stat_visit_exprs),
                    }
                },
                Statement::Loop.{
                    body.statements.[].(stat_visit_exprs),
                    continuing.[].{
                        body.statements.[].(stat_visit_exprs),
                        break_if.[],
                    }
                },
                Statement::For.{
                    initializer.[].(x => stat_visit_exprs(x)),
                    condition.[],
                    update.[].(x => stat_visit_exprs(x)),
                    body.statements.[].(stat_visit_exprs),
                },
                Statement::While.{
                    condition,
                    body.statements.[].(stat_visit_exprs),
                },
                Statement::Return.[],
                Statement::FunctionCall.arguments.[],
                Statement::ConstAssert.expression,
                Statement::Declaration.initializer.[],
            }
        )
    }

    fn expr_visit_type_exprs(expr: &Expression) -> impl Iterator<Item = &TypeExpression> {
        query!(expr.{
            Expression::Parenthesized.(x => expr_visit_type_exprs(x)),
            Expression::NamedComponent.base.(x => expr_visit_type_exprs(x)),
            Expression::Indexing.{ base.(x => expr_visit_type_exprs(x)), index.(x => expr_visit_type_exprs(x)) },
            Expression::Unary.operand.(x => expr_visit_type_exprs(x)),
            Expression::Binary.{ left.(x => expr_visit_type_exprs(x)), right.(x => expr_visit_type_exprs(x)) },
            Expression::FunctionCall.arguments.[].(expr_visit_type_exprs),
            Expression::Type,
        })
    }

    fn mod_visit_type_exprs(module: &TranslationUnit) -> impl Iterator<Item = &TypeExpression> {
        query!(
            module.global_declarations.[].{
                GlobalDeclaration::Declaration.{
                    typ.[],
                    initializer.[].(expr_visit_type_exprs),
                },
                GlobalDeclaration::TypeAlias.typ,
                GlobalDeclaration::Struct.members.[].typ,
                GlobalDeclaration::Function.{
                    parameters.[].typ,
                    return_type.[],
                    body.statements.[].(stat_visit_exprs).(expr_visit_type_exprs),
                }
            }
        )
    }

    for ty in mod_visit_type_exprs(module) {
        println!("ty: {ty:?}");
    }
    for expr in mod_visit_exprs(module) {
        println!("ty: {expr:?}");
    }
}

impl<R: Resolver> Module<R> {
    pub fn mangle(&mut self, mangler: &impl Mangler<R>) -> Result<(), ImportError> {
        for (resource, items) in &self.imports {
            for item in items {
                let old_ident = item.rename.as_ref().unwrap_or(&item.name);
                let new_ident = mangler.mangle(&resource, &item.name);
                replace_imported_ident(&mut self.source, &old_ident, &new_ident);
            }
        }

        Ok(())
    }
}
