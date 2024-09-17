#[cfg(feature = "cond-comp")]
pub mod condcomp;
#[cfg(feature = "imports")]
pub mod imports;

mod mangle;
mod resolve;
mod strip;
mod syntax_util;

pub use mangle::{
    FileManglerEscape, FileManglerHash, Mangler, NoMangler, MANGLER_ESCAPE, MANGLER_HASH,
    MANGLER_NONE,
};

pub use resolve::{
    DispatchResolver, FileResolver, PreprocessResolver, Resolver, Resource, VirtualFileResolver,
};

pub use strip::strip;

use std::collections::HashMap;
use wgsl_parse::syntax::TranslationUnit;

#[derive(Clone, Debug, thiserror::Error)]
pub enum Error {
    #[error("import resolution failure: {0}")]
    ResolveError(#[from] resolve::ResolveError),
    #[cfg(feature = "imports")]
    #[error("import error: {0}")]
    ImportError(#[from] imports::ImportError),
    #[cfg(feature = "cond-comp")]
    #[error("conditional compilation error: {0}")]
    CondCompError(#[from] condcomp::CondCompError),
}

pub struct CompileOptions {
    pub use_imports: bool,
    pub use_condcomp: bool,
    pub strip: bool,
    pub entry_points: Vec<String>,
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

pub fn compile<M: Mangler + ?Sized>(
    entrypoint: &Resource,
    resolver: impl Resolver,
    mangler: &M,
    options: &CompileOptions,
) -> Result<TranslationUnit, Error> {
    let mut wgsl = if cfg!(feature = "imports") && options.use_imports {
        let module = if cfg!(feature = "cond-comp") && options.use_condcomp {
            let resolver = PreprocessResolver(resolver, |wesl| {
                condcomp::run(wesl, &options.features)?;
                Ok(())
            });
            imports::resolve(entrypoint, &resolver, mangler)?
        } else {
            imports::resolve(entrypoint, &resolver, mangler)?
        };
        module.assemble()
    } else {
        let mut wesl = resolver.resolve_file(entrypoint)?;
        if options.use_condcomp {
            condcomp::run(&mut wesl, &options.features)?;
        }
        wesl
    };

    if options.strip {
        strip(&mut wgsl, &options.entry_points);
    }

    Ok(wgsl)
}
