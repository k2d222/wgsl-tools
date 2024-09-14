use std::collections::HashMap;

use imports::{Mangler, PreprocessResolver, Resolver, Resource};
use wgsl_parse::syntax::TranslationUnit;

pub mod condcomp;
pub mod imports;

#[derive(Clone, Debug, thiserror::Error)]
pub enum Error {
    // #[error("command-line error: `{0}`")]
    // CliError(#[from] CliError),
    #[error("import error: `{0}`")]
    ImportError(#[from] imports::ImportError),
    #[error("conditional compilation error: `{0}`")]
    CondCompError(#[from] condcomp::CondcompError),
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
    let wgsl = if options.use_imports {
        let module = if options.use_condcomp {
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
