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
pub use import::ImportError;

#[cfg(feature = "eval")]
pub use eval::{Eval, EvalError, Exec};

#[cfg(feature = "generics")]
pub use generics::GenericsError;

pub use mangle::{
    CachedMangler, FileManglerEscape, FileManglerHash, Mangler, NoMangler, UnicodeMangler,
    MANGLER_ESCAPE, MANGLER_HASH, MANGLER_NONE, MANGLER_UNICODE,
};

pub use resolve::{
    CacheResolver, DispatchResolver, FileResolver, PreprocessResolver, ResolveError, Resolver,
    Resource, VirtualFileResolver,
};

pub use error::{Diagnostic, Error};

pub use sourcemap::{BasicSourceMap, SourceMap, SourceMapper};

pub use strip::strip_except;

pub use wgsl_parse::syntax;

pub use lower::{lower, lower_sourcemap};

use itertools::Itertools;
use std::collections::HashMap;
use wgsl_parse::syntax::TranslationUnit;

#[derive(Debug)]
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
    main_names: &mut Vec<String>,
) -> Result<TranslationUnit, Error> {
    let resolver = Box::new(resolver);

    #[cfg(feature = "condcomp")]
    let resolver: Box<dyn Resolver> = if options.use_condcomp {
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
    // *main_names = entry_points(&wesl)
    //     .map(|name| name.to_string())
    //     .collect_vec();
    *main_names = wesl
        .global_declarations
        .iter()
        .filter_map(|decl| decl.name())
        .map(|name| name.to_string())
        .collect_vec();

    #[cfg(feature = "imports")]
    let wesl = if options.use_imports {
        let mut module = import::Module::new(wesl, entrypoint.clone());
        module.resolve(&resolver)?;
        module.mangle(mangler);
        let wesl = module.assemble();
        wesl
    } else {
        wesl
    };
    let mut wesl = wesl;

    #[cfg(feature = "generics")]
    if options.use_generics {
        generics::generate_variants(&mut wesl)?;
        generics::replace_calls(&mut wesl)?;
    };

    if options.strip {
        let entry_names = options.entry_points.as_ref().unwrap_or(main_names);
        strip_except(&mut wesl, entry_names);
    }

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
    let mut main_names = Vec::new();
    let comp = compile_impl(
        entrypoint,
        &sourcemapper,
        &sourcemapper,
        options,
        &mut main_names,
    );
    let mut sourcemap = sourcemapper.finish();
    for entry in &main_names {
        sourcemap.add_decl(entry.clone(), entrypoint.clone(), entry.clone());
    }
    let comp = comp.and_then(|mut wesl| {
        lower_sourcemap(&mut wesl, &sourcemap)?;
        Ok(wesl)
    });
    (comp, sourcemap)
}

#[cfg(feature = "eval")]
pub fn eval_const<'s>(
    expr: &syntax::Expression,
    wgsl: &'s TranslationUnit,
) -> (Result<eval::Instance, EvalError>, eval::Context<'s>) {
    let mut ctx = eval::Context::new(wgsl);
    let res = wgsl.exec(&mut ctx).and_then(|_| expr.eval(&mut ctx));
    (res, ctx)
}

#[cfg(feature = "eval")]
pub fn eval_runtime<'s>(
    expr: &syntax::Expression,
    wgsl: &'s TranslationUnit,
    bindings: HashMap<(u32, u32), eval::RefInstance>,
    overrides: HashMap<String, eval::Instance>,
) -> (Result<eval::Instance, EvalError>, eval::Context<'s>) {
    let mut ctx = eval::Context::new(wgsl);
    ctx.add_bindings(bindings);
    ctx.add_overrides(overrides);
    ctx.set_stage(eval::EvalStage::Exec);
    let res = wgsl.exec(&mut ctx).and_then(|_| expr.eval(&mut ctx));
    (res, ctx)
}
