use std::collections::HashMap;

use super::{ImportError, Module};
use crate::syntax_util::IterUses;
use crate::Mangler;
use wgsl_parse::syntax::*;
use wgsl_parse_macros::query_mut;

impl Module {
    pub fn mangle(&mut self, mangler: &(impl Mangler + ?Sized)) -> Result<(), ImportError> {
        // delared idents
        let mut replace: HashMap<String, String> = query_mut!(self.source.global_declarations.[].{
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
        .collect();

        // imported idents
        for (resource, items) in &self.imports {
            for item in items {
                let old_ident = item.rename.as_ref().unwrap_or(&item.name).clone();
                let new_ident = mangler.mangle(&resource, &item.name);
                replace.insert(old_ident, new_ident);
            }
        }

        for name in self.source.uses_mut() {
            if let Some(new_ident) = replace.get(name) {
                *name = new_ident.clone();
            }
        }

        Ok(())
    }
}
