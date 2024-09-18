use wgsl_parse::syntax::TranslationUnit;

use super::Module;

impl Module {
    pub fn assemble(&self) -> TranslationUnit {
        let mut wgsl = self.source.clone();
        wgsl.imports.clear();

        for (_, module) in &self.resolutions {
            wgsl.global_declarations
                .extend(module.global_declarations.iter().cloned());
        }

        wgsl
    }
}
