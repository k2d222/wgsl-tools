use super::module::imports_to_resources;
use super::Module;
use crate::{Mangler, Resource};
use wgsl_parse::syntax::*;

fn mangle_decls(wgsl: &mut TranslationUnit, resource: &Resource, mangler: &impl Mangler) {
    wgsl.global_declarations
        .iter_mut()
        .filter_map(|decl| decl.ident_mut())
        .for_each(|ident| {
            let new_name = mangler.mangle(&resource, ident.name().as_str());
            ident.rename(new_name.clone());
        });
}

fn mangle_imports(wgsl: &mut TranslationUnit, resource: &Resource, mangler: &impl Mangler) {
    // find imported idents
    let mut imports = imports_to_resources(&wgsl.imports, &resource);
    for (resource, items) in imports.iter_mut() {
        for item in items {
            let new_name = mangler.mangle(&resource, &item.ident.name().as_str());
            let ident = item.rename.as_mut().unwrap_or(&mut item.ident);
            ident.rename(new_name);
        }
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
