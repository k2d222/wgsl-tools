//! # WESL: A Community Standard for Enhanced WGSL
//!
//! This is the crate for all your [WESL](https://github.com/wgsl-tooling-wg/wesl-spec) needs.
//!
//! Work in progress! Both the WESL specification and this API are subject to frequent
//! changes.
//!
//! See also: the [standalone CLI](https://github.com/k2d222/wgsl-tools) and the
//! [wgsl/wesl parser](https://github.com/k2d222/wgsl-tools/tree/main/crates/wgsl-parse).
//!
//! ## Basic Usage
//!
//! See [`Wesl`] for an overview of the high-level API.
//! ```rust
//! let compiler = Wesl::new_spec_compliant();
//!
//! // compile a WESL file to a WGSL string
//! let wgsl_str = compiler
//!     .compile("main.wesl")
//!     .inspect_err(|e| eprintln!("WESL error: {e}")) // pretty errors with `display()`
//!     .unwrap()
//!     .to_string();
//! ```
//!
//! ## Usage in [`build.rs`](https://doc.rust-lang.org/cargo/reference/build-scripts.html)
//!
//! In your rust project you probably want to have your WESL code converted automatically
//! to a WGSL string at build-time, unless your WGSL code must be assembled at runtime.
//!
//! Add this crate to your build dependencies in `Cargo.toml`:
//! ```toml
//! [build-dependencies]
//! wesl = "0.1"
//! ```
//!
//! Create the `build.rs` file with the following content:
//! ```rust
//! # use wesl::{Wesl, FileResolver};
//! fn main() {
//!     Wesl::new_spec_compliant()
//!         .set_resolver(FileResolver::new("src/shaders"))
//!         .build("main.wesl");
//! }
//! ```
//!
//! Include the compiled WGSL string in your code:
//! ```rust
//! # use wesl::include_wesl;
//! let module = device.create_shader_module(ShaderModuleDescriptor {
//!     label: Some("main.wesl"),
//!     source: ShaderSource::Wgsl(include_wesl!("main.wesl")),
//! })
//! ```
//! NOTE: [`include_wesl`] is a very simple convenience macro.
//!
//! ## Advanced Examples
//!
//! Evaluate const-expressions.
//!```rust
//! # use wesl::{Wesl};
//! # let compiler = Wesl::new_spec_compliant();
//! // ...standalone expression
//! let wgsl_expr = compiler.eval("abs(3 - 5)").unwrap().to_string();
//!
//! // ...expression using declarations in a WESL file
//! let wgsl_expr = compiler.compile("main.wesl").unwrap().eval("my_fn(my_const) + 5").unwrap().to_string();
//! ```
//!
//! Custom resolver: customize how import paths are translated to wesl modules.
//!```rust
//! # use wesl::{FileResolver, Router, VirtualResolver, Wesl};
//! // in this example, `import runtime::constants::PI` is in a custom module mounted at runtime.
//! let mut resolver = VirtualResolver::new();
//! resolver.add_file("constants", "const PI = 3.1415; const TAU = PI * 2.0;");
//! let mut router = Router::new();
//! router.mount_fallback_resolver(FileResolver::new("src/shaders"));
//! router.mount_resolver("runtime", resolver);
//! let compiler = Wesl::new_spec_compliant().set_resolver(router);
//! ```
//!
//! ## Features
//!
//! | name     | description                                           | WESL Specification       |
//! |----------|-------------------------------------------------------|--------------------------|
//! | imports  | import statements and qualified identifiers with `::` | [in progress][imports]   |
//! | condcomp | conditional compilation with `@if` attributes         | [complete][cond-trans]   |
//! | generics | user-defined type-generators and generic functions    | [experimental][generics] |
//! | eval     | execute shader code on the CPU and `@const` attribute | not compliant            |
//!
//! [cond-trans]: https://github.com/wgsl-tooling-wg/wesl-spec/blob/main/ConditionalTranslation.md
//! [imports]: https://github.com/wgsl-tooling-wg/wesl-spec/blob/main/Imports.md
//! [generics]: https://github.com/wgsl-tooling-wg/wesl-spec/blob/main/Generics.md

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

use eval::HostShareable;
#[cfg(feature = "imports")]
pub use import::ImportError;

#[cfg(feature = "eval")]
pub use eval::{Eval, EvalError, Exec};

#[cfg(feature = "generics")]
pub use generics::GenericsError;

pub use mangle::{CacheMangler, EscapeMangler, HashMangler, Mangler, NoMangler, UnicodeMangler};

use resolve::NoResolver;
pub use resolve::{
    CacheResolver, FileResolver, Preprocessor, ResolveError, Resolver, Resource, Router,
    VirtualResolver,
};

pub use error::{Diagnostic, Error};

pub use sourcemap::{BasicSourceMap, SourceMap, SourceMapper};

pub use strip::strip_except;

pub use wgsl_parse::syntax;

pub use lower::{lower, lower_sourcemap};

use itertools::Itertools;
use std::{collections::HashMap, path::Path, sync::LazyLock};
use wgsl_parse::syntax::{Ident, TranslationUnit};

#[derive(Debug)]
pub struct CompileOptions {
    pub use_imports: bool,
    pub use_condcomp: bool,
    pub use_generics: bool,
    pub use_stripping: bool,
    pub entry_points: Option<Vec<String>>,
    pub features: HashMap<String, bool>,
}

impl Default for CompileOptions {
    fn default() -> Self {
        Self {
            use_imports: true,
            use_condcomp: true,
            use_generics: false,
            use_stripping: true,
            entry_points: Default::default(),
            features: Default::default(),
        }
    }
}

#[derive(Default, Clone, Copy, Debug, PartialEq, Eq)]
pub enum ManglerKind {
    /// Escaped path mangler. `foo/bar/{item} -> foo_bar_item`
    #[default]
    Escape,
    /// Hash mangler. `foo/bar/{item} -> item_1985638328947`
    Hash,
    /// Make valid identifiers with unicode "confusables" characters.
    /// `foo/{bar<baz, moo>} -> foo::barᐸbazˏmooᐳ`
    Unicode,
    /// Disable mangling (warning: will break if case of name conflicts!)
    None,
}

fn make_mangler(kind: ManglerKind) -> Box<dyn Mangler> {
    match kind {
        ManglerKind::Escape => Box::new(EscapeMangler),
        ManglerKind::Hash => Box::new(HashMangler),
        ManglerKind::Unicode => Box::new(UnicodeMangler),
        ManglerKind::None => Box::new(NoMangler),
    }
}

/// Include a WGSL file compiled with [`Wesl::build`] as a string.
///
/// This is a very simple convenience macro. See the crate documentation for a usage
/// example.
#[macro_export]
macro_rules! include_wesl {
    ($root:literal) => {
        include_str!(concat!(env!("OUT_DIR"), "/", $root))
    };
}

/// The WESL compiler high-level API (builder pattern).
///
/// # Basic Usage
///
/// ```rust
/// let compiler = Wesl::new_spec_compliant();
/// let wgsl_string = compiler.compile("path/to/main.wesl").unwrap().to_string();
/// ```
pub struct Wesl {
    options: CompileOptions,
    use_sourcemap: bool,
    resolver: Box<dyn Resolver>,
    mangler: Box<dyn Mangler>,
}

impl Wesl {
    /// Get a WESL compiler with all *mandatory* and *optional* WESL extensions enabled,
    /// but not *experimental* and *non-standard* extensions.
    ///
    /// It's probably what you want to use in most cases. You can customize it with other
    /// functions.
    ///
    /// See also: [`Wesl::new_barebones`], [`Wesl::new_spec_compliant`].
    ///
    /// # WESL Reference
    /// * (mandatory) Imports: [`Imports.md`](https://github.com/wgsl-tooling-wg/wesl-spec/blob/main/Imports.md) (specification for `imports` is not stabilized yet)
    /// * (mandatory) Conditional translation: [`ConditionalTranslation.md`](https://github.com/wgsl-tooling-wg/wesl-spec/blob/main/ConditionalTranslation.md)
    /// * (optional)  Stripping: spec not yet available.
    pub fn new_spec_compliant() -> Self {
        Self {
            options: CompileOptions {
                use_imports: true,
                use_condcomp: true,
                use_generics: false,
                use_stripping: true,
                entry_points: None,
                features: Default::default(),
            },
            use_sourcemap: true,
            resolver: Box::new(FileResolver::new("")),
            mangler: Box::new(EscapeMangler),
        }
    }

    /// Get a WESL compiler with all functionalities enabled, including *experimental*
    /// ones.
    ///
    /// See also: [`Wesl::new_barebones`] and [`Wesl::new_spec_compliant`].
    ///
    /// # WESL Reference
    /// This Wesl compiler is *not* spec-compliant because it enables all extensions
    /// including *experimental* and *non-standard* ones. See [`Wesl::new_spec_compliant`].
    ///
    /// Experimental extensions: `generics`
    /// Non-standard extensions: `@const`
    pub fn new_experimental() -> Self {
        Self {
            options: CompileOptions {
                use_imports: true,
                use_condcomp: true,
                use_generics: true,
                use_stripping: true,
                entry_points: None,
                features: Default::default(),
            },
            use_sourcemap: true,
            resolver: Box::new(FileResolver::new("")),
            mangler: Box::new(EscapeMangler),
        }
    }

    /// Get WESL compiler with all extensions disabled.
    ///
    /// You *must* set a [`Mangler`] and a [`Resolver`] manually to use this compiler, see
    /// [`Wesl::set_mangler`] and [`Wesl::set_resolver`].
    ///
    /// # WESL Reference
    /// This Wesl compiler is *not* spec-compliant because it does not enable *mandatory*
    /// WESL extensions. See [`Wesl::new_spec_compliant`].
    pub fn new_barebones() -> Self {
        Self {
            options: CompileOptions {
                use_imports: false,
                use_condcomp: false,
                use_generics: false,
                use_stripping: false,
                entry_points: None,
                features: Default::default(),
            },
            use_sourcemap: false,
            resolver: Box::new(NoResolver),
            mangler: Box::new(EscapeMangler),
        }
    }

    pub fn set_options(mut self, options: CompileOptions) -> Self {
        self.options = options;
        self
    }

    /// Set the [`Mangler`].
    ///
    /// The default mangler is [`EscapeMangler`].
    ///
    /// # WESL Reference
    /// Custom manglers *must* conform to the constraints described in [`Mangler`].
    /// Spec: not yet available.
    pub fn set_mangler(mut self, kind: ManglerKind) -> Self {
        self.mangler = make_mangler(kind);
        self
    }

    /// Set a custom [`Mangler`].
    ///
    /// The default mangler is [`EscapeMangler`].
    ///
    /// # WESL Reference
    /// All [builtin manglers](ManglerKind) are spec-compliant, except [`NoMangler`] ([`ManglerKind::None`]).
    /// Spec: not yet available.
    pub fn set_custom_mangler(mut self, mangler: impl Mangler + 'static) -> Self {
        self.mangler = Box::new(mangler);
        self
    }

    /// Set a custom [`Resolver`].
    ///
    /// The default resolver is a [`FileResolver`] rooted in the current directory.
    ///
    /// # WESL Reference
    /// Both [`FileResolver`] and [`VirtualResolver`] are spec-compliant.
    /// Custom resolvers *must* conform to the constraints described in [`Resolver`].
    /// Spec: not yet available.
    pub fn set_resolver(mut self, resolver: impl Resolver + 'static) -> Self {
        self.resolver = Box::new(resolver);
        self
    }

    /// Enable source-mapping (experimental).
    ///
    /// Turning "on" this option will improve the quality of error messages.
    ///
    /// # WESL Reference
    /// Sourcemapping is not part of the WESL Specification and does not impact compliance.
    pub fn use_sourcemap(mut self, val: bool) -> Self {
        self.use_sourcemap = val;
        self
    }

    /// Enable imports.
    ///
    /// Inline import identifiers (identifiers with `::`) are not yet implemented.
    ///
    /// # WESL Reference
    /// Imports is a *mandatory* WESL extension.
    /// Spec: not yet available.
    #[cfg(feature = "imports")]
    pub fn use_imports(mut self, val: bool) -> Self {
        self.options.use_imports = val;
        self
    }

    /// Enable conditional compilation.
    ///
    /// # WESL Reference
    /// Conditional Compilation is a *mandatory* WESL extension.
    /// Spec: [`ConditionalTranslation.md`](https://github.com/wgsl-tooling-wg/wesl-spec/blob/main/ConditionalTranslation.md)
    #[cfg(feature = "condcomp")]
    pub fn use_condcomp(mut self, val: bool) -> Self {
        self.options.use_condcomp = val;
        self
    }

    /// Enable generics.
    ///
    /// # WESL Reference
    /// Generics is an *experimental* WESL extension.
    /// Spec: not yet available.
    #[cfg(feature = "generics")]
    pub fn use_generics(mut self, val: bool) -> Self {
        self.options.use_generics = val;
        self
    }
    /// Set a conditional compilation feature flag.
    ///
    /// # WESL Reference
    /// Conditional translation is a *mandatory* WESL extension.
    /// Spec: [`ConditionalTranslation.md`](https://github.com/wgsl-tooling-wg/wesl-spec/blob/main/ConditionalTranslation.md)
    #[cfg(feature = "condcomp")]
    pub fn set_feature(mut self, feat: &str, val: bool) -> Self {
        self.options.features.insert(feat.to_string(), val);
        self
    }
    /// Set conditional compilation feature flags.
    ///
    /// # WESL Reference
    /// Conditional translation is a *mandatory* WESL extension.
    /// Spec: [`ConditionalTranslation.md`](https://github.com/wgsl-tooling-wg/wesl-spec/blob/main/ConditionalTranslation.md)
    #[cfg(feature = "condcomp")]
    pub fn set_features<'a>(mut self, feats: impl IntoIterator<Item = (&'a str, bool)>) -> Self {
        self.options
            .features
            .extend(feats.into_iter().map(|(k, v)| (k.to_string(), v)));
        self
    }
    /// Unset a conditional compilation feature flag.
    ///
    /// # WESL Reference
    /// Conditional translation is a *mandatory* WESL extension.
    /// Spec: [`ConditionalTranslation.md`](https://github.com/wgsl-tooling-wg/wesl-spec/blob/main/ConditionalTranslation.md)
    #[cfg(feature = "condcomp")]
    pub fn unset_feature(mut self, feat: &str) -> Self {
        self.options.features.remove(feat);
        self
    }
    /// Removes unused declarations from the final WGSL output.
    ///
    /// Unused declarations are all declarations not used (directly or indirectly) by any
    /// of the entrypoints (functions marked `@compute`, `@vertex` or `@fragment`) in the
    /// root module.
    ///
    /// see also: [`Wesl::keep_entrypoints`]
    ///
    /// # WESL Reference
    /// Code stripping is an *optional* WESL extension.
    /// Customizing entrypoints returned by the compiler is explicitly allowed by the spec.
    /// Spec: not yet available.
    pub fn use_stripping(mut self, val: bool) -> Self {
        self.options.use_stripping = val;
        self
    }
    /// If stripping is enabled, specifies which entrypoints to keep in the final WGSL.
    /// All entrypoints are kept by default.
    ///
    /// # WESL Reference
    /// Code stripping is an *optional* WESL extension.
    /// Customizing entrypoints returned by the compiler is explicitly allowed by the spec.
    /// Spec: not yet available.
    pub fn keep_entrypoints(mut self, entries: Vec<String>) -> Self {
        self.options.entry_points = Some(entries);
        self
    }
    /// If stripping is enabled, keep all entrypoints in the root WESL module.
    /// This is the default. See [`Wesl::keep_entrypoints`].
    ///
    /// # WESL Reference
    /// Code stripping is an *optional* WESL extension.
    /// Customizing entrypoints returned by the compiler is explicitly allowed by the spec.
    /// Spec: not yet available.
    pub fn keep_all_entrypoints(mut self) -> Self {
        self.options.entry_points = None;
        self
    }
}

#[derive(Clone)]
pub struct CompileResult {
    pub syntax: TranslationUnit,
    pub sourcemap: Option<BasicSourceMap>,
}

impl CompileResult {
    pub fn to_file(&self, path: impl AsRef<Path>) -> std::io::Result<()> {
        std::fs::write(path, self.to_string())
    }
}

impl ToString for CompileResult {
    /// Get the WGSL string resulting from the compilation of a WESL source.
    fn to_string(&self) -> String {
        self.syntax.to_string()
    }
}

#[cfg(feature = "eval")]
pub struct ExecResult<'a> {
    /// The executed function return value
    pub inst: eval::Instance,
    /// Context after execution
    pub ctx: eval::Context<'a>,
}

impl<'a> ExecResult<'a> {
    /// Get the function return value.
    ///
    /// "void" functions return [`eval::Instance::Void`].
    pub fn return_value(&self) -> &eval::Instance {
        &self.inst
    }

    /// Get a [shader resource](https://www.w3.org/TR/WGSL/#resource).
    ///
    /// Shader resources (aka. bindings) with `write` [access mode](https://www.w3.org/TR/WGSL/#memory-access-mode)
    /// can be modified after executing an entry point.
    pub fn resource(&self, group: u32, binding: u32) -> Option<&eval::RefInstance> {
        self.ctx.resource(group, binding)
    }
}

impl<'a> ToString for ExecResult<'a> {
    /// Get the WGSL string representing the function return value expression.
    fn to_string(&self) -> String {
        self.inst.to_string()
    }
}

#[cfg(feature = "eval")]
pub struct EvalResult<'a> {
    /// The expression evaluation result
    pub inst: eval::Instance,
    /// Context after evaluation
    pub ctx: eval::Context<'a>,
}

impl<'a> EvalResult<'a> {
    // TODO: make context non-mut
    /// Get the WGSL string representing the evaluated expression.
    pub fn to_buffer(&mut self) -> Option<Vec<u8>> {
        self.inst.to_buffer(&mut self.ctx)
    }
}

impl<'a> ToString for EvalResult<'a> {
    /// Get the WGSL string representing the evaluated expression.
    fn to_string(&self) -> String {
        self.inst.to_string()
    }
}

#[cfg(feature = "eval")]
impl CompileResult {
    /// Evaluate a const-expression in the context of this compilation result.
    ///
    /// Contrary to [`Wesl::eval`], the provided expression can reference declarations
    /// in the compiled WGSL: global const-declarations and user-defined functions with
    /// the `@const` attribute.
    ///
    /// # WESL Reference
    /// The `@const` attribute is non-standard.
    pub fn eval(&self, source: &str) -> Result<EvalResult, Error> {
        static EMPTY_MODULE: LazyLock<TranslationUnit> =
            LazyLock::new(|| TranslationUnit::default());
        let expr = source
            .parse::<syntax::Expression>()
            .map_err(|e| Error::Error(Diagnostic::from(e).with_source(source.to_string())))?;
        let (inst, ctx) = eval_const(&expr, &EMPTY_MODULE);
        let inst = inst.map_err(|e| {
            Diagnostic::from(e)
                .with_source(source.to_string())
                .with_ctx(&ctx)
        });

        let inst = if let Some(sourcemap) = &self.sourcemap {
            inst.map_err(|e| Error::Error(e.with_sourcemap(sourcemap)))
        } else {
            inst.map_err(|e| Error::Error(e))
        }?;

        let res = EvalResult { inst, ctx };
        Ok(res)
    }

    /// Execute an entrypoint in the same way it would be executed on the GPU.
    ///
    /// /!\ This function is highly experimental.
    ///
    ///
    /// # WESL Reference
    /// The `@const` attribute is non-standard.
    pub fn exec(
        &self,
        entrypoint: &str,
        bindings: HashMap<(u32, u32), eval::RefInstance>,
        overrides: HashMap<String, eval::Instance>,
    ) -> Result<ExecResult, Error> {
        // TODO: this is not the right way.
        // BUG: this is no longer working because of Ident PartialEq impl
        let expr = syntax::Expression::FunctionCall(syntax::FunctionCall {
            ty: syntax::TypeExpression {
                ident: Ident::new(entrypoint.to_string()),
                template_args: None,
            },
            arguments: Vec::new(),
        });

        let (inst, ctx) = eval_runtime(&expr, &self.syntax, bindings, overrides);
        let inst = inst.map_err(|e| {
            Diagnostic::from(e)
                .with_source(expr.to_string())
                .with_ctx(&ctx)
        });

        let inst = if let Some(sourcemap) = &self.sourcemap {
            inst.map_err(|e| Error::Error(e.with_sourcemap(sourcemap)))
        } else {
            inst.map_err(|e| Error::Error(e))
        }?;

        Ok(ExecResult { inst, ctx })
    }
}

impl Wesl {
    /// Compile a WESL program from a root file.
    ///
    /// The result of `compile` is not necessarily a valid WGSL string. See (TODO) to
    /// validate the output and (TODO) perform convert the output to valid WGSL.
    ///
    /// # WESL Reference
    /// Spec: not available yet.
    pub fn compile(&self, entrypoint: impl AsRef<Path>) -> Result<CompileResult, Error> {
        let entrypoint = Resource::from(entrypoint.as_ref().to_path_buf());

        if self.use_sourcemap {
            let (syntax, sourcemap) =
                compile_sourcemap(&entrypoint, &self.resolver, &self.mangler, &self.options);
            Ok(CompileResult {
                syntax: syntax?,
                sourcemap: Some(sourcemap),
            })
        } else {
            let syntax = compile(&entrypoint, &self.resolver, &self.mangler, &self.options);
            Ok(CompileResult {
                syntax: syntax?,
                sourcemap: None,
            })
        }
    }

    /// Compile a WESL program from a string.
    ///
    /// The result of `compile` is not necessarily a valid WGSL string. See (TODO) to
    /// validate the output and (TODO) perform convert the output to valid WGSL.
    ///
    /// # WESL Reference
    /// Spec: not available yet.
    pub fn compile_str(&self, entrypoint: impl AsRef<Path>) -> Result<CompileResult, Error> {
        let entrypoint = Resource::from(entrypoint.as_ref().to_path_buf());

        if self.use_sourcemap {
            let (syntax, sourcemap) =
                compile_sourcemap(&entrypoint, &self.resolver, &self.mangler, &self.options);
            Ok(CompileResult {
                syntax: syntax?,
                sourcemap: Some(sourcemap),
            })
        } else {
            let syntax = compile(&entrypoint, &self.resolver, &self.mangler, &self.options);
            Ok(CompileResult {
                syntax: syntax?,
                sourcemap: None,
            })
        }
    }

    /// Compile a WESL program from a root file an output the result in rust's OUT_DIR.
    ///
    /// This function is meant to be used in a `build.rs` workflow. The compiled WGSL will
    /// be available with the [`include_wesl`] macro. See the crate documentation for a
    /// usage example.
    ///
    /// # Panics
    /// Panics when compilation fails or if the output file cannot be written.
    /// Pretty-prints the WESL error message to stderr.
    pub fn build(&self, entrypoint: impl AsRef<Path>) {
        let entrypoint = entrypoint.as_ref();
        let dirname = std::env::var("OUT_DIR").unwrap();
        let filename = entrypoint.file_name().unwrap();
        let output = Path::new(&dirname).join(filename);
        self.compile(entrypoint)
            .inspect_err(|e| {
                eprintln!(
                    "failed to build WESL shader `{}`.\n{e}",
                    entrypoint.display()
                );
                panic!();
            })
            .unwrap()
            .to_file(output)
            .expect("failed to write output shader");
    }

    /// Evaluate a const-expression.
    ///
    /// Only function declarations marked `@const` can be called from const-expressions.
    /// User-defined expressions with the `@const` attribute is an *non-standard*
    /// extension.
    ///
    /// # WESL Reference
    /// Highly experimental. Not all builtin `@const` WGSL functions are supported yet.
    /// Spec: not available yet.
    /// see issue [#46](https://github.com/wgsl-tooling-wg/wesl-spec/issues/46#issuecomment-2389531479).
    #[cfg(feature = "eval")]
    pub fn eval(source: &str) -> Result<eval::Instance, Error> {
        let expr = source
            .parse::<syntax::Expression>()
            .map_err(|e| Error::Error(Diagnostic::from(e).with_source(source.to_string())))?;
        let wgsl = TranslationUnit::default();
        let (inst, ctx) = eval_const(&expr, &wgsl);
        inst.map_err(|e| {
            Error::Error(
                Diagnostic::from(e)
                    .with_source(source.to_string())
                    .with_ctx(&ctx),
            )
        })
    }
}

impl Default for Wesl {
    fn default() -> Self {
        Self::new_spec_compliant()
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
        Box::new(Preprocessor::new(resolver, |wesl| {
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
        .filter_map(|decl| decl.ident())
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

    if options.use_stripping {
        let entry_names = options.entry_points.as_ref().unwrap_or(main_names);
        strip_except(&mut wesl, entry_names);
    }

    Ok(wesl)
}

/// Low-level version of [`Wesl::compile`].
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

/// Like [`compile`], but provides better error diagnostics and returns the sourcemap.
pub fn compile_sourcemap(
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

/// Low-level version of [`Wesl::eval`].
#[cfg(feature = "eval")]
pub fn eval_const<'s>(
    expr: &syntax::Expression,
    wgsl: &'s TranslationUnit,
) -> (Result<eval::Instance, EvalError>, eval::Context<'s>) {
    let mut ctx = eval::Context::new(wgsl);
    let res = wgsl.exec(&mut ctx).and_then(|_| expr.eval(&mut ctx));
    (res, ctx)
}

/// Low-level version of [`CompileResult::exec`].
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
