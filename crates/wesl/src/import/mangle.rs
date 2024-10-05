use std::collections::HashMap;

use super::module::imports_to_resources;
use super::Module;
use crate::syntax_util::IterUses;
use crate::{Mangler, Resource};
use wgsl_parse::syntax::*;
use wgsl_parse_macros::query_mut;

fn mangle_file(wesl: &mut TranslationUnit, resource: Resource, mangler: &impl Mangler) {
    // delared idents
    let mut replace: HashMap<String, String> = query_mut!(wesl.global_declarations.[].{
        GlobalDeclaration::Declaration.name,
        GlobalDeclaration::TypeAlias.name,
        GlobalDeclaration::Struct.name,
        GlobalDeclaration::Function.name,
    })
    .map(|decl_ident| {
        let old_ident = decl_ident.to_string();
        let new_ident = mangler.mangle(&resource, &old_ident);
        *decl_ident = new_ident.clone();
        (old_ident, new_ident)
    })
    .collect();

    // imported idents
    let imports = imports_to_resources(&wesl.imports, &resource);
    for (resource, items) in imports.iter() {
        for item in items {
            let old_ident = item.rename.as_ref().unwrap_or(&item.name).clone();
            let new_ident = mangler.mangle(&resource, &item.name);
            replace.insert(old_ident, new_ident);
        }
    }

    for ty in wesl.uses_mut() {
        if let Some(new_ident) = replace.get(&ty.name) {
            ty.name = new_ident.clone();
        }
    }
}

impl Module {
    pub fn mangle(&mut self, mangler: &impl Mangler) {
        mangle_file(&mut self.source, self.resource.clone(), mangler);
        for (resource, source) in &mut self.resolutions {
            mangle_file(source, resource.clone(), mangler);
        }
    }
}
