use wgsl_parse::syntax;

use crate::resolve::{ImportError, Module, Resolver};

pub trait Mangler<R: Resolver> {
    fn mangle(&self, resource: &R::Resource, item: &str) -> String;
}

impl<R: Resolver> Module<R> {
    pub fn mangle(&mut self, mangler: &impl Mangler<R>) -> Result<(), ImportError> {
        for (resource, items) in &self.imports {
            for item in items {
                let old_ident = item.rename.as_ref().unwrap_or(&item.name);
                let new_ident = mangler.mangle(resource, &item.name);
                // self.source.replace(&old_ident, &new_ident);
            }
        }

        Ok(())
    }

    fn replace(&mut self, old_ident: &str, new_ident: &str) {}
}
