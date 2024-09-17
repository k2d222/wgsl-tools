use std::collections::HashSet;

use wgsl_parse::syntax::TranslationUnit;

use super::Module;
use crate::Resource;

impl Module {
    pub fn assemble(&self) -> TranslationUnit {
        let mut visited = HashSet::new();

        fn rec_insert<'s>(
            module: &'s Module,
            resource: &'s Resource,
            wgsl: &mut TranslationUnit,
            visited: &mut HashSet<&'s Resource>,
        ) {
            if !visited.contains(resource) {
                visited.insert(resource);
                for (resource, module) in &module.resolutions {
                    rec_insert(module, resource, wgsl, visited);
                }
                wgsl.global_declarations
                    .extend(module.source.global_declarations.iter().cloned());
            }
        }

        let mut wgsl = TranslationUnit::default();

        for (path, module) in &self.resolutions {
            rec_insert(module, path, &mut wgsl, &mut visited);
        }

        wgsl.global_declarations
            .extend(self.source.global_declarations.iter().cloned());
        wgsl.global_directives
            .extend(self.source.global_directives.iter().cloned());

        wgsl
    }
}
