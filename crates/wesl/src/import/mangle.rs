use std::collections::HashMap;

use super::module::imports_to_resources;
use super::Module;
use crate::syntax_util::IterUses;
use crate::{Mangler, Resource};
use wgsl_parse::syntax::*;

fn mangle_decls(wgsl: &mut TranslationUnit, resource: &Resource, mangler: &impl Mangler) {
    // find declared idents
    let replace: HashMap<String, String> = wgsl
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

    replace_idents(wgsl, &replace);
}

fn mangle_imports(wgsl: &mut TranslationUnit, resource: &Resource, mangler: &impl Mangler) {
    // find imported idents
    let imports = imports_to_resources(&wgsl.imports, &resource);
    let mut replace: HashMap<String, String> = HashMap::new();
    for (resource, items) in imports.iter() {
        for item in items {
            let old_ident = item.rename.as_ref().unwrap_or(&item.name).clone();
            let new_ident = mangler.mangle(&resource, &item.name);
            replace.insert(old_ident, new_ident);
        }
    }

    replace_idents(wgsl, &replace);
}

fn replace_idents(wgsl: &mut TranslationUnit, replace: &HashMap<String, String>) {
    fn rec(ty: &mut TypeExpression, replace: &HashMap<String, String>) {
        if let Some(new_name) = replace.get(&ty.name) {
            ty.name = new_name.clone();
        }
        for ty in ty.uses_mut() {
            rec(ty, replace);
        }
    }
    for ty in wgsl.uses_mut() {
        rec(ty, &replace);
    }
}

impl Module {
    pub fn mangle(&mut self, mangler: &impl Mangler) {
        mangle_imports(&mut self.source, &self.resource, mangler);
        for (resource, source) in &mut self.resolutions {
            mangle_decls(source, resource, mangler);
            mangle_imports(source, resource, mangler);
        }
    }
}
