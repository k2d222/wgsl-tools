#[cfg(feature = "condcomp")]
mod condcomp;
#[cfg(feature = "consteval")]
pub mod consteval;
#[cfg(feature = "imports")]
mod import;

mod error;
mod mangle;
mod resolve;
mod sourcemap;
mod strip;
mod syntax_util;

#[cfg(feature = "condcomp")]
pub use condcomp::{run as run_condcomp, CondCompError};

#[cfg(feature = "imports")]
pub use import::{resolve, ImportError, Module};

#[cfg(feature = "consteval")]
pub use consteval::{Context, Eval, EvalError, Exec, Instance};

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

use syntax_util::{entry_points, rename_decl};

use itertools::Itertools;
use std::collections::HashMap;
use wgsl_parse::syntax::TranslationUnit;

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
    resolver: &impl Resolver,
    mangler: &impl Mangler,
    options: &CompileOptions,
) -> Result<TranslationUnit, Diagnostic> {
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
    let wesl = resolver
        .source_to_module(&source, entrypoint)
        .map_err(|e| {
            Diagnostic::from(e)
                .source(source.into_owned())
                .file(entrypoint.clone())
        })?;

    let entry_names = entry_points(&wesl)
        .map(|name| name.to_string())
        .collect_vec();

    let mut wgsl = if cfg!(feature = "imports") && options.use_imports {
        let mut module = Module::new(wesl, entrypoint.clone());
        module.resolve(&resolver)?;
        module.mangle(mangler);
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
            strip(&mut wgsl, &mangled_names);
        } else {
            let mangled_names = entry_names
                .iter()
                .map(|name| mangler.mangle(entrypoint, name))
                .collect_vec();
            strip(&mut wgsl, &mangled_names);
        }
    }

    for entry_name in &entry_names {
        rename_decl(
            &mut wgsl,
            &mangler.mangle(entrypoint, &entry_name),
            entry_name,
        );
    }

    Ok(wgsl)
}

pub fn compile_with_sourcemap(
    entrypoint: &Resource,
    resolver: &impl Resolver,
    mangler: &impl Mangler,
    options: &CompileOptions,
) -> Result<(TranslationUnit, BasicSourceMap), Diagnostic> {
    let resolver = Box::new(resolver);
    let mangler = Box::new(mangler);
    let sourcemapper = SourceMapper::new(resolver, mangler);
    let wgsl = compile(entrypoint, &sourcemapper, &sourcemapper, options)?;
    let sourcemap = sourcemapper.finish();
    Ok((wgsl, sourcemap))
}

pub fn eval(expr: &str, wgsl: &TranslationUnit) -> Result<Instance, Diagnostic> {
    let mut ctx = Context::new(wgsl);
    let expr = expr
        .parse::<syntax::Expression>()
        .map_err(|e| Diagnostic::from(e).source(expr.to_string()))?;

    let instance = wgsl
        .exec(&mut ctx)
        .and_then(|_| expr.eval(&mut ctx))
        .map_err(|e| {
            let mut diagnostic = Diagnostic::new(e.into());
            let (decl, span) = ctx.err_ctx();
            diagnostic.declaration = decl;
            diagnostic.span = span;
            diagnostic
        })?;
    Ok(instance)
}

pub fn eval_with_sourcemap(
    expr: &str,
    wgsl: &TranslationUnit,
    sourcemap: &impl SourceMap,
) -> Result<Instance, Diagnostic> {
    let mut ctx = Context::new(wgsl);
    let expr = expr
        .parse::<syntax::Expression>()
        .map_err(|e| Diagnostic::from(e).source(expr.to_string()))?;

    let instance = wgsl
        .exec(&mut ctx)
        .and_then(|_| expr.eval(&mut ctx))
        .map_err(|e| {
            let mut diagnostic = Diagnostic::new(e.into());
            let (decl, span) = ctx.err_ctx();
            diagnostic.span = span;

            if let Some(decl) = decl {
                if let Some((resource, decl)) = sourcemap.get_decl(&decl) {
                    diagnostic.file = Some(resource.clone());
                    diagnostic.declaration = Some(decl.to_string());
                    diagnostic.source = sourcemap.get_source(resource).map(|s| s.to_string());
                } else {
                    diagnostic.declaration = Some(decl);
                    diagnostic.source = sourcemap.get_default_source().map(|s| s.to_string());
                }
            } else {
                println!(
                    "{:?} {decl:?} {:?}",
                    ctx.err_ctx(),
                    sourcemap.get_default_source()
                );

                diagnostic.source = sourcemap.get_default_source().map(|s| s.to_string());
            }
            diagnostic
        })?;
    Ok(instance)
}
