use std::collections::HashMap;

use super::module::imports_to_resources;
use super::Module;
use crate::syntax_util::IterUses;
use crate::{Mangler, Resource};
use wgsl_parse::syntax::*;

pub trait Mangle {
    fn mangle(&mut self, resource: &Resource, mangler: &impl Mangler);
}

impl Mangle for TranslationUnit {
    fn mangle(&mut self, resource: &Resource, mangler: &impl Mangler) {
        // find declared idents
        let mut replace: HashMap<String, String> = self
            .global_declarations
            .iter_mut()
            .filter_map(|decl| decl.name_mut())
            .map(|decl_ident| {
                let old_ident = decl_ident.to_string();
                let new_ident = mangler.mangle(&resource, &old_ident);
                *decl_ident = new_ident.clone();
                (old_ident, new_ident)
            })
            .collect();

        // find imported idents
        let imports = imports_to_resources(&self.imports, &resource);
        for (resource, items) in imports.iter() {
            for item in items {
                let old_ident = item.rename.as_ref().unwrap_or(&item.name).clone();
                let new_ident = mangler.mangle(&resource, &item.name);
                replace.insert(old_ident, new_ident);
            }
        }

        // run replacements
        fn rec(ty: &mut TypeExpression, replace: &HashMap<String, String>) {
            if let Some(new_name) = replace.get(&ty.name) {
                ty.name = new_name.clone();
            }
            for ty in ty.uses_mut() {
                rec(ty, replace);
            }
        }
        for ty in self.uses_mut() {
            rec(ty, &replace);
        }
    }
}

impl Module {
    pub fn mangle(&mut self, mangler: &impl Mangler) {
        // TODO: should we mangle items in main?
        // self.source.mangle(resource, mangler);
        for (resource, source) in &mut self.resolutions {
            source.mangle(resource, mangler);
        }
    }
}
