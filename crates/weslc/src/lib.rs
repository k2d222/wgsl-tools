#[cfg(feature = "cond-comp")]
pub mod condcomp;
#[cfg(feature = "imports")]
pub mod imports;

mod mangle;
mod resolve;

pub use mangle::{
    FileManglerEscape, FileManglerHash, Mangler, NoMangler, FILE_MANGLER_ESCAPE, FILE_MANGLER_HASH,
    FILE_MANGLER_NONE,
};

pub use resolve::{
    FileResolver, FileResource, PreprocessResolver, Resolver, Resource, VirtualFileResolver,
};

use std::collections::HashMap;
use wgsl_parse::syntax::TranslationUnit;

#[derive(Clone, Debug, thiserror::Error)]
pub enum Error {
    #[error("import resolution failure: `{0}`")]
    ResolveError(#[from] resolve::ResolveError),
    #[cfg(feature = "imports")]
    #[error("import error: `{0}`")]
    ImportError(#[from] imports::ImportError),
    #[cfg(feature = "cond-comp")]
    #[error("conditional compilation error: `{0}`")]
    CondCompError(#[from] condcomp::CondCompError),
}

pub struct CompileOptions {
    pub use_imports: bool,
    pub use_condcomp: bool,
    pub features: HashMap<String, bool>,
}

impl Default for CompileOptions {
    fn default() -> Self {
        Self {
            use_imports: true,
            use_condcomp: true,
            features: Default::default(),
        }
    }
}

pub fn compile<R: Resource, M: Mangler<R> + ?Sized>(
    entrypoint: &R,
    resolver: impl Resolver<Resource = R>,
    mangler: &M,
    options: &CompileOptions,
) -> Result<TranslationUnit, Error> {
    let wgsl = if cfg!(feature = "imports") && options.use_imports {
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

    Ok(wgsl)
}
