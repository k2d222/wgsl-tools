#[cfg(feature = "attributes")]
mod attributes;
#[cfg(feature = "condcomp")]
mod condcomp;
#[cfg(feature = "eval")]
pub mod eval;
#[cfg(feature = "generics")]
mod generics;
#[cfg(feature = "imports")]
mod import;

mod error;
mod lower;
mod mangle;
mod resolve;
mod sourcemap;
mod strip;
mod syntax_util;

#[cfg(feature = "condcomp")]
pub use condcomp::CondCompError;

#[cfg(feature = "imports")]
pub use import::{resolve, ImportError, Module};

#[cfg(feature = "eval")]
use eval::{Context, Eval, EvalError, Exec, Instance};

#[cfg(feature = "generics")]
pub use generics::GenericsError;

pub use mangle::{
    CachedMangler, FileManglerEscape, FileManglerHash, Mangler, NoMangler, MANGLER_ESCAPE,
    MANGLER_HASH, MANGLER_NONE,
};

pub use resolve::{
    CacheResolver, DispatchResolver, FileResolver, PreprocessResolver, ResolveError, Resolver,
    Resource, VirtualFileResolver,
};

pub use error::{Diagnostic, Error};

pub use sourcemap::{BasicSourceMap, SourceMap, SourceMapper};

pub use strip::strip;

pub use wgsl_parse::syntax;

pub use lower::{lower, lower_sourcemap};

use syntax_util::entry_points;

use itertools::Itertools;
use std::collections::HashMap;
use wgsl_parse::syntax::TranslationUnit;

pub struct CompileOptions {
    pub use_imports: bool,
    pub use_condcomp: bool,
    pub use_generics: bool,
    pub strip: bool,
    pub entry_points: Option<Vec<String>>,
    pub features: HashMap<String, bool>,
}

impl Default for CompileOptions {
    fn default() -> Self {
        Self {
            use_imports: true,
            use_condcomp: true,
            use_generics: true,
            strip: true,
            entry_points: Default::default(),
            features: Default::default(),
        }
    }
}

fn compile_impl(
    entrypoint: &Resource,
    resolver: &impl Resolver,
    mangler: &impl Mangler,
    options: &CompileOptions,
    entry_names: &mut Vec<String>,
) -> Result<TranslationUnit, Error> {
    let resolver = Box::new(resolver);
    let resolver: Box<dyn Resolver> = if cfg!(feature = "condcomp") && options.use_condcomp {
        Box::new(PreprocessResolver::new(resolver, |wesl| {
            condcomp::run(wesl, &options.features)?;
            Ok(())
        }))
    } else {
        resolver
    };

    let source = resolver.resolve_source(entrypoint)?;
    let wesl = resolver.source_to_module(&source, entrypoint)?;

    // hack, entry_names is passed by &mut just to return it from the function even in error case.
    *entry_names = entry_points(&wesl)
        .map(|name| name.to_string())
        .collect_vec();

    let mut wesl = if cfg!(feature = "imports") && options.use_imports {
        let mut module = Module::new(wesl, entrypoint.clone());
        module.resolve(&resolver)?;
        module.mangle(mangler);
        let wesl = module.assemble();
        wesl
    } else {
        wesl
    };

    if cfg!(feature = "generics") && options.use_generics {
        generics::generate_variants(&mut wesl)?;
        generics::replace_calls(&mut wesl)?;
    };

    if options.strip {
        let entry_names = options.entry_points.as_ref().unwrap_or(entry_names);
        // TODO: should we mangle names in main?
        // let mangled_names = entry_names
        //     .iter()
        //     .map(|name| mangler.mangle(entrypoint, name))
        //     .collect_vec();
        // strip(&mut wesl, &mangled_names);
        strip(&mut wesl, entry_names);
    }

    // TODO: should we mangle names in main?
    // for entry_name in entry_names {
    //     rename_decl(
    //         &mut wesl,
    //         &mangler.mangle(entrypoint, &entry_name),
    //         entry_name,
    //     );
    // }

    Ok(wesl)
}

pub fn compile(
    entrypoint: &Resource,
    resolver: &impl Resolver,
    mangler: &impl Mangler,
    options: &CompileOptions,
) -> Result<TranslationUnit, Error> {
    let mut entry_names = Vec::new();
    let mut wesl = compile_impl(entrypoint, resolver, mangler, options, &mut entry_names)?;
    lower(&mut wesl)?;
    Ok(wesl)
}

pub fn compile_with_sourcemap(
    entrypoint: &Resource,
    resolver: &impl Resolver,
    mangler: &impl Mangler,
    options: &CompileOptions,
) -> (Result<TranslationUnit, Error>, BasicSourceMap) {
    let resolver = Box::new(resolver);
    let mangler = Box::new(mangler);
    let sourcemapper = SourceMapper::new(resolver, mangler);
    let mut entry_names = Vec::new();
    let comp = compile_impl(
        entrypoint,
        &sourcemapper,
        &sourcemapper,
        options,
        &mut entry_names,
    );
    let mut sourcemap = sourcemapper.finish();
    for entry in &entry_names {
        sourcemap.add_decl(entry.clone(), entrypoint.clone(), entry.clone());
    }
    let comp = comp.and_then(|mut wesl| {
        lower_sourcemap(&mut wesl, &sourcemap)?;
        Ok(wesl)
    });
    (comp, sourcemap)
}

#[cfg(feature = "eval")]
pub fn eval<'s>(
    expr: &syntax::Expression,
    wgsl: &'s TranslationUnit,
) -> (Result<Instance, EvalError>, Context<'s>) {
    let mut ctx = Context::new(wgsl);
    let res = wgsl.exec(&mut ctx).and_then(|_| expr.eval(&mut ctx));
    (res, ctx)
}
