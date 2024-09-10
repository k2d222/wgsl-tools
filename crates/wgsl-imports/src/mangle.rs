use std::hash::DefaultHasher;
use std::hash::Hash;
use std::hash::Hasher;

use itertools::chain;
use wgsl_parse::syntax::*;
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
