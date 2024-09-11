use std::hash::DefaultHasher;
use std::hash::Hash;
use std::hash::Hasher;

use wgsl_parse::syntax::*;
use wgsl_parse::visit::VisitMut;
use wgsl_parse_macros::query;
use wgsl_parse_macros::query_mut;

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

// TODO: handle shadowing
fn replace_idents(module: &mut TranslationUnit, old_ident: &str, new_ident: &str) {
    let type_exprs = VisitMut::<TypeExpression>::visit_mut(module);
    for ty in type_exprs {
        if ty.name == old_ident {
            ty.name = new_ident.to_owned();
        }
    }

    fn replace_in_expr(expr: &mut Expression, old_ident: &str, new_ident: &str) {
        if let Expression::FunctionCall(call) = expr {
            if call.name == old_ident {
                call.name = new_ident.to_owned();
            }
        } else {
            VisitMut::<Expression>::visit_mut(expr)
                .for_each(|expr| replace_in_expr(expr, old_ident, new_ident))
        }
    }

    VisitMut::<Expression>::visit_mut(module)
        .for_each(|expr| replace_in_expr(expr, old_ident, new_ident));

    fn replace_in_stat(stat: &mut Statement, old_ident: &str, new_ident: &str) {
        if let Statement::FunctionCall(call) = stat {
            if call.name == old_ident {
                call.name = new_ident.to_owned();
            }
        } else {
            VisitMut::<Statement>::visit_mut(stat)
                .for_each(|stat| replace_in_stat(stat, old_ident, new_ident))
        }
    }

    VisitMut::<Statement>::visit_mut(module)
        .for_each(|stat| replace_in_stat(stat, old_ident, new_ident));
}

impl<R: Resolver> Module<R> {
    pub fn mangle(&mut self, mangler: &impl Mangler<R>) -> Result<(), ImportError> {
        // replace imported idents
        for (resource, items) in &self.imports {
            for item in items {
                let old_ident = item.rename.as_ref().unwrap_or(&item.name);
                let new_ident = mangler.mangle(&resource, &item.name);
                replace_idents(&mut self.source, &old_ident, &new_ident);
            }
        }

        // replace exposed idents
        let decl_idents = {
            let source = &mut self.source;
            query_mut!(source.global_declarations.[].{
                GlobalDeclaration::Declaration.name,
                GlobalDeclaration::TypeAlias.name,
                GlobalDeclaration::Struct.name,
                GlobalDeclaration::Function.name,
            })
            .map(|decl_ident| {
                let old_ident = decl_ident.to_string();
                let new_ident = mangler.mangle(&self.resource, &old_ident);
                *decl_ident = new_ident.clone();
                (old_ident, new_ident)
            })
            .collect::<Vec<_>>()
        };
        for (old_ident, new_ident) in decl_idents {
            replace_idents(&mut self.source, &old_ident, &new_ident);
        }

        Ok(())
    }
}
