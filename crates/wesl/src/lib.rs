#[cfg(feature = "condcomp")]
pub mod condcomp;
#[cfg(feature = "imports")]
pub mod import;

mod mangle;
mod resolve;
mod strip;
mod syntax_util;

pub use condcomp::CondCompError;
pub use import::ImportError;
pub use mangle::{
    FileManglerEscape, FileManglerHash, Mangler, NoMangler, MANGLER_ESCAPE, MANGLER_HASH,
    MANGLER_NONE,
};
pub use resolve::{
    DispatchResolver, FileResolver, PreprocessResolver, ResolveError, Resolver, Resource,
    VirtualFileResolver,
};
pub use strip::strip;

pub use wgsl_parse::syntax;

use syntax_util::{entry_points, rename_decl};

use itertools::Itertools;
use std::collections::HashMap;
use wgsl_parse::syntax::TranslationUnit;

use crate::import::Module;

#[derive(Clone, Debug, thiserror::Error)]
pub enum Error {
    #[error("import resolution failure: {0}")]
    ResolveError(#[from] ResolveError),
    #[cfg(feature = "imports")]
    #[error("import error: {0}")]
    ImportError(#[from] ImportError),
    #[cfg(feature = "condcomp")]
    #[error("conditional compilation error: {0}")]
    CondCompError(#[from] CondCompError),
}

pub struct CompileOptions {
    pub use_imports: bool,
    pub use_condcomp: bool,
    pub strip: bool,
    pub entry_points: Option<Vec<String>>,
    pub features: HashMap<String, bool>,
}

impl Default for CompileOptions {
    fn default() -> Self {
        Self {
            use_imports: true,
            use_condcomp: true,
            strip: true,
            entry_points: Default::default(),
            features: Default::default(),
        }
    }
}

pub fn compile(
    entrypoint: &Resource,
    resolver: impl Resolver,
    mangler: &impl Mangler,
    options: &CompileOptions,
) -> Result<TranslationUnit, Error> {
    let resolver: Box<dyn Resolver> = if cfg!(feature = "condcomp") && options.use_condcomp {
        Box::new(PreprocessResolver(resolver, |wesl| {
            condcomp::run(wesl, &options.features)?;
            Ok(())
        }))
    } else {
        Box::new(resolver)
    };

    let wesl = resolver.resolve_file(entrypoint)?;

    let entry_names = entry_points(&wesl)
        .map(|name| name.to_string())
        .collect_vec();

    let mut wesl = if cfg!(feature = "imports") && options.use_imports {
        let mut module = Module::new(wesl, entrypoint.clone());
        module.resolve(&resolver)?;
        module.mangle(mangler)?;
        let wesl = module.assemble();
        wesl
    } else {
        wesl
    };

    if options.strip {
        if let Some(entry_names) = &options.entry_points {
            let mangled_names = entry_names
                .iter()
                .map(|name| mangler.mangle(entrypoint, name))
                .collect_vec();
            strip(&mut wesl, &mangled_names);
        } else {
            let mangled_names = entry_names
                .iter()
                .map(|name| mangler.mangle(entrypoint, name))
                .collect_vec();
            strip(&mut wesl, &mangled_names);
        }
    }

    for entry_name in &entry_names {
        rename_decl(
            &mut wesl,
            &mangler.mangle(entrypoint, &entry_name),
            entry_name,
        );
    }

    Ok(wesl)
}
