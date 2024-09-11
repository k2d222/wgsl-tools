use std::hash::DefaultHasher;
use std::hash::Hash;
use std::hash::Hasher;

use itertools::chain;
use wgsl_parse::syntax::*;
use wgsl_parse::visit::Visit;
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
    let type_exprs = Visit::<TypeExpression>::visit(module);
    for ty in type_exprs {
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
