use std::hash::DefaultHasher;
use std::hash::Hash;
use std::hash::Hasher;

use itertools::chain;
use wgsl_parse::syntax::*;
use wgsl_parse::visit_fields;
use wgsl_parse::visit_variants;

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

impl<R: Resolver> Module<R> {
    pub fn mangle(&mut self, mangler: &impl Mangler<R>) -> Result<(), ImportError> {
        for (resource, items) in self.imports.clone() {
            for item in items {
                let old_ident = item.rename.as_ref().unwrap_or(&item.name);
                let new_ident = mangler.mangle(&resource, &item.name);
                self.replace(&old_ident, &new_ident);
            }
        }

        Ok(())
    }

    fn visit_type_exprs(&mut self) -> impl Iterator<Item = &mut TypeExpression> {
        fn expr_type_exprs(x: &mut Expression) -> impl Iterator<Item = &mut TypeExpression> {
            visit_variants!(x, {
                Expression::Parenthesized(x) => expr_type_exprs(x),
                Expression::NamedComponent(x) => expr_type_exprs(&mut x.base),
                Expression::Indexing(x) => visit_fields!(x, {
                    base => expr_type_exprs(base),
                    index => expr_type_exprs(index),
                }),
                Expression::Unary(x) => expr_type_exprs(&mut x.operand),
                Expression::Binary(x) => visit_fields!(x, {
                    left => expr_type_exprs(left),
                    right => expr_type_exprs(right),
                }),
                Expression::FunctionCall(x) => x.arguments.visit_mut().each().flat_map(expr_type_exprs),
                Expression::Type(x) => x.visit_mut(),
            })
        }

        fn stat_type_exprs(x: &mut Statement) -> impl Iterator<Item = &mut TypeExpression> {
            visit_variants!(x, {
                Statement::Compound(x) => x.statements.visit_mut().each().flat_map(stat_type_exprs),
                Statement::Assignment(x) => visit_fields!(x, {
                    lhs => expr_type_exprs(lhs),
                    rhs => expr_type_exprs(rhs),
                }),
                Statement::Increment(x) => expr_type_exprs(x),
                Statement::Decrement(x) => expr_type_exprs(x),
                Statement::If(x) => visit_fields!(x, {
                    if_clause => {
                        let (expr, stat) = if_clause;
                        chain!(expr_type_exprs(expr), stat.statements.visit_mut().each().flat_map(stat_type_exprs))
                    },
                    else_if_clauses => else_if_clauses.visit_mut().each().flat_map(|(expr, stat)| {
                        chain!(expr_type_exprs(expr), stat.statements.visit_mut().each().flat_map(stat_type_exprs))
                    }),
                    else_clause => else_clause.visit_mut().some().statements().each().flat_map(stat_type_exprs),
                }),
                Statement::Switch(x) => visit_fields!(x , {
                    expression => expr_type_exprs(expression),
                    clauses => clauses.visit_mut().each().flat_map(visit_fields! {
                        case_selectors => case_selectors.visit_mut().each().match_expression().flat_map(expr_type_exprs),
                        body => body.statements.visit_mut().each().flat_map(stat_type_exprs),
                    }),
                }),
                Statement::Loop(x) => visit_fields!(x, {
                    body => body.statements.visit_mut().each().flat_map(stat_type_exprs),
                    continuing => continuing.visit_mut().some().flat_map(visit_fields! {
                        body => body.statements.visit_mut().each().flat_map(stat_type_exprs),
                        break_if => break_if.visit_mut().some().flat_map(expr_type_exprs),
                    }),
                }),
                Statement::For(x) => visit_fields!(x, {
                    initializer => initializer.visit_mut().some().flat_map(|x| stat_type_exprs(x)),
                    condition => condition.visit_mut().some().flat_map(expr_type_exprs),
                    update => update.visit_mut().some().flat_map(|x| stat_type_exprs(x)),
                    body => body.statements.visit_mut().each().flat_map(stat_type_exprs),
                }),
                Statement::While(x) => visit_fields!(x, {
                    condition => expr_type_exprs(condition),
                    body => body.statements.visit_mut().each().flat_map(stat_type_exprs),
                }),
                Statement::Return(x) => x.visit_mut().some().flat_map(expr_type_exprs),
                Statement::FunctionCall(x) => x.arguments.visit_mut().each().flat_map(expr_type_exprs),
                Statement::ConstAssert(x) => expr_type_exprs(&mut x.expression),
                Statement::Declaration(x) => visit_fields!(x, {
                    typ => typ.visit_mut().some(),
                    initializer => initializer.visit_mut().some().flat_map(expr_type_exprs),
                }),
            })
        }

        let type_exprs = self
            .source
            .visit_mut()
            .global_declarations()
            .each()
            .flat_map(visit_variants! {
                GlobalDeclaration::Declaration(x) => visit_fields!(x, {
                    typ => typ.visit_mut().some(),
                    initializer => initializer.visit_mut().some().flat_map(expr_type_exprs),
                }),
                GlobalDeclaration::TypeAlias(x) => x.typ.visit_mut(),
                GlobalDeclaration::Struct(x) => x.members.visit_mut().each().typ(),
                GlobalDeclaration::Function(x) => visit_fields!(x, {
                    parameters => parameters.visit_mut().each().typ(),
                    return_type => return_type.visit_mut().some(),
                    body => body.visit_mut().statements().each().flat_map(stat_type_exprs),
                }),
            });
        type_exprs
    }

    fn replace(&mut self, old_ident: &str, new_ident: &str) {
        for type_expr in self.visit_type_exprs() {
            if type_expr.name == old_ident {
                type_expr.name = new_ident.to_string();
            }
        }
    }
}
