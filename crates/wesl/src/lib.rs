//! # WESL: A Community Standard for Enhanced WGSL
//!
//! This is the crate for all your [WESL](https://github.com/wgsl-tooling-wg/wesl-spec) needs.
//!
//! Work in progress! Both the WESL specification and this API are subject to frequent
//! changes.
//!
//! See also: the [standalone CLI](https://github.com/wgsl-tooling-wg/wesl-rs).
//!
//! ## Basic Usage
//!
//! See [`Wesl`] for an overview of the high-level API.
//! ```ignore
//! # use wesl::Wesl;
//! let compiler = Wesl::new("src/shaders");
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
//! ```ignore
//! # use wesl::{Wesl, FileResolver};
//! fn main() {
//!     Wesl::new("src/shaders")
//!         .build_artefact("main.wesl", "my_shader");
//! }
//! ```
//!
//! Include the compiled WGSL string in your code:
//! ```ignore
//! let module = device.create_shader_module(ShaderModuleDescriptor {
//!     label: Some("my_shader"),
//!     source: ShaderSource::Wgsl(include_wesl!("my_shader")),
//! });
//! ```
//!
//! ## Advanced Examples
//!
//! Evaluate const-expressions.
//!```rust
//! # use wesl::{Wesl, VirtualResolver, eval_str};
//! // ...standalone expression
//! let wgsl_expr = eval_str("abs(3 - 5)").unwrap().to_string();
//! assert_eq!(wgsl_expr, "2");
//!
//! // ...expression using declarations in a WESL file
//! let source = "const my_const = 4; @const fn my_fn(v: u32) -> u32 { return v * 10; }";
//! # let mut resolver = VirtualResolver::new();
//! # resolver.add_module("source", source.into());
//! # let compiler = Wesl::new_barebones().set_custom_resolver(resolver);
//! let wgsl_expr = compiler.compile("source").unwrap().eval("my_fn(my_const) + 2").unwrap().to_string();
//! assert_eq!(wgsl_expr, "42u");
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

#[cfg(feature = "condcomp")]
mod condcomp;
#[cfg(feature = "eval")]
pub mod eval;
#[cfg(feature = "generics")]
mod generics;
#[cfg(feature = "imports")]
mod import;
#[cfg(feature = "package")]
mod package;

mod error;
mod lower;
mod mangle;
mod resolve;
mod sourcemap;
mod strip;
mod syntax_util;
mod validate;
mod visit;

#[cfg(feature = "condcomp")]
pub use condcomp::CondCompError;

use eval::HostShareable;
#[cfg(feature = "imports")]
pub use import::ImportError;

#[cfg(feature = "eval")]
pub use eval::{Eval, EvalError, Exec};

#[cfg(feature = "generics")]
pub use generics::GenericsError;

#[cfg(feature = "package")]
pub use package::PkgBuilder;

pub use error::{Diagnostic, Error};
pub use lower::lower;
pub use mangle::{CacheMangler, EscapeMangler, HashMangler, Mangler, NoMangler, UnicodeMangler};
pub use resolve::{
    CacheResolver, FileResolver, NoResolver, PkgModule, PkgResolver, Preprocessor, ResolveError,
    Resolver, Resource, Router, StandardResolver, VirtualResolver,
};
pub use sourcemap::{BasicSourceMap, SourceMap, SourceMapper};
pub use strip::strip_except;
pub use syntax_util::SyntaxUtil;
use validate::validate_wesl;
pub use validate::{validate_wgsl, ValidateError};

pub use wgsl_parse::syntax;

use itertools::Itertools;
use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    path::Path,
};
use wgsl_parse::syntax::{Ident, TranslationUnit};

#[derive(Debug)]
pub struct CompileOptions {
    pub imports: bool,
    pub condcomp: bool,
    pub generics: bool,
    pub strip: bool,
    pub lower: bool,
    pub validate: bool,
    pub lazy: bool,
    pub keep: Option<Vec<String>>,
    pub features: HashMap<String, bool>,
}

impl Default for CompileOptions {
    fn default() -> Self {
        Self {
            imports: true,
            condcomp: true,
            generics: false,
            strip: true,
            lower: false,
            validate: true,
            lazy: true,
            keep: Default::default(),
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

fn make_mangler(kind: ManglerKind) -> Box<dyn Mangler + Send + Sync + 'static> {
    match kind {
        ManglerKind::Escape => Box::new(EscapeMangler),
        ManglerKind::Hash => Box::new(HashMangler),
        ManglerKind::Unicode => Box::new(UnicodeMangler),
        ManglerKind::None => Box::new(NoMangler),
    }
}

/// Include a WGSL file compiled with [`Wesl::build_artefact`] as a string.
///
/// The argument corresponds to the `out_name` passed to [`Wesl::build_artefact`].
///
/// This is a very simple convenience macro. See the crate documentation for a usage
/// example.
#[macro_export]
macro_rules! include_wesl {
    ($root:literal) => {
        include_str!(concat!(env!("OUT_DIR"), "/", $root, ".wgsl"))
    };
}

/// Include a generated package.
///
/// See [`PkgBuilder`] for more information about building WESL packages.
#[macro_export]
macro_rules! wesl_pkg {
    ($pkg_name:ident) => {
        wesl_pkg!($pkg_name, concat!("/", stringify!($pkg_name), ".rs"));
    };
    ($pkg_name:ident, $source:expr) => {
        pub mod $pkg_name {
            use wesl::PkgModule;

            include!(concat!(env!("OUT_DIR"), $source));
        }
    };
}

/// The WESL compiler high-level API (builder pattern).
///
/// # Basic Usage
///
/// ```ignore
/// # use wesl::Wesl;
/// let compiler = Wesl::new("path/to/dir/containing/shaders");
/// let wgsl_string = compiler.compile("main.wesl").unwrap().to_string();
/// ```
pub struct Wesl<R: Resolver> {
    options: CompileOptions,
    use_sourcemap: bool,
    resolver: R,
    mangler: Box<dyn Mangler + Send + Sync + 'static>,
}

impl Wesl<StandardResolver> {
    /// Get a WESL compiler with all *mandatory* and *optional* WESL extensions enabled,
    /// but not *experimental* and *non-standard* extensions.
    ///
    /// It's probably what you want to use in most cases. You can customize it with other
    /// functions.
    ///
    /// See also: [`Wesl::new_barebones`], [`Wesl::new_experimental`].
    ///
    /// # WESL Reference
    /// * (mandatory) Imports: [`Imports.md`](https://github.com/wgsl-tooling-wg/wesl-spec/blob/main/Imports.md) (specification for `imports` is not stabilized yet)
    /// * (mandatory) Conditional translation: [`ConditionalTranslation.md`](https://github.com/wgsl-tooling-wg/wesl-spec/blob/main/ConditionalTranslation.md)
    /// * (optional)  Stripping: spec not yet available.
    pub fn new(base: impl AsRef<Path>) -> Self {
        Self {
            options: CompileOptions {
                imports: true,
                condcomp: true,
                generics: false,
                strip: true,
                lower: false,
                validate: true,
                lazy: true,
                keep: None,
                features: Default::default(),
            },
            use_sourcemap: true,
            resolver: StandardResolver::new(base),
            mangler: Box::new(EscapeMangler),
        }
    }

    /// Get a WESL compiler with all functionalities enabled, including *experimental*
    /// ones.
    ///
    /// See also: [`Wesl::new`] and [`Wesl::new_barebones`].
    ///
    /// # WESL Reference
    /// This Wesl compiler is *not* spec-compliant because it enables all extensions
    /// including *experimental* and *non-standard* ones. See [`Wesl::new`].
    ///
    /// Experimental extensions: `generics`
    /// Non-standard extensions: `@const`
    pub fn new_experimental(base: impl AsRef<Path>) -> Self {
        Self {
            options: CompileOptions {
                imports: true,
                condcomp: true,
                generics: true,
                strip: true,
                lower: true,
                validate: true,
                lazy: true,
                keep: None,
                features: Default::default(),
            },
            use_sourcemap: true,
            resolver: StandardResolver::new(base),
            mangler: Box::new(EscapeMangler),
        }
    }

    /// Add a package dependency.
    ///
    /// Learn more about packages in [`PkgBuilder`].
    pub fn add_package(&mut self, pkg: &'static dyn PkgModule) -> &mut Self {
        self.resolver.add_package(pkg);
        self
    }

    /// Add several package dependencies.
    ///
    /// Learn more about packages in [`PkgBuilder`].
    pub fn add_packages(
        &mut self,
        pkgs: impl IntoIterator<Item = &'static dyn PkgModule>,
    ) -> &mut Self {
        for pkg in pkgs {
            self.resolver.add_package(pkg);
        }
        self
    }
}

impl Wesl<NoResolver> {
    /// Get WESL compiler with all extensions disabled.
    ///
    /// You *should* set a [`Mangler`] and a [`Resolver`] manually to use this compiler, see
    /// [`Wesl::set_mangler`] and [`Wesl::set_resolver`].
    ///
    /// # WESL Reference
    /// This Wesl compiler is *not* spec-compliant because it does not enable *mandatory*
    /// WESL extensions. See [`Wesl::new`].
    pub fn new_barebones() -> Self {
        Self {
            options: CompileOptions {
                imports: false,
                condcomp: false,
                generics: false,
                strip: false,
                lower: false,
                validate: false,
                lazy: false,
                keep: None,
                features: Default::default(),
            },
            use_sourcemap: false,
            resolver: NoResolver,
            mangler: Box::new(NoMangler),
        }
    }
}

impl<R: Resolver> Wesl<R> {
    /// Set all compilation options.
    pub fn set_options(&mut self, options: CompileOptions) -> &mut Self {
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
    pub fn set_mangler(&mut self, kind: ManglerKind) -> &mut Self {
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
    pub fn set_custom_mangler(
        &mut self,
        mangler: impl Mangler + Send + Sync + 'static,
    ) -> &mut Self {
        self.mangler = Box::new(mangler);
        self
    }

    /// Set a custom [`Resolver`] (customize how import paths are translated to wesl modules).
    ///
    ///```rust
    /// # use wesl::{FileResolver, Router, VirtualResolver, Wesl};
    /// // in this example, `import runtime::constants::PI` is in a custom module mounted at runtime.
    /// let mut resolver = VirtualResolver::new();
    /// resolver.add_module("constants", "const PI = 3.1415; const TAU = PI * 2.0;".into());
    /// let mut router = Router::new();
    /// router.mount_fallback_resolver(FileResolver::new("src/shaders"));
    /// router.mount_resolver("runtime", resolver);
    /// let compiler = Wesl::new("").set_custom_resolver(router);
    /// ```
    ///
    /// # WESL Reference
    /// Both [`FileResolver`] and [`VirtualResolver`] are spec-compliant.
    /// Custom resolvers *must* conform to the constraints described in [`Resolver`].
    /// Spec: not yet available.
    pub fn set_custom_resolver(self, resolver: impl Resolver + 'static) -> Wesl<Box<dyn Resolver>> {
        Wesl {
            options: self.options,
            use_sourcemap: self.use_sourcemap,
            mangler: self.mangler,
            resolver: Box::new(resolver),
        }
    }

    /// Enable source-mapping (experimental).
    ///
    /// Turning "on" this option will improve the quality of error messages.
    ///
    /// # WESL Reference
    /// Sourcemapping is not part of the WESL Specification and does not impact compliance.
    pub fn use_sourcemap(&mut self, val: bool) -> &mut Self {
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
    pub fn use_imports(&mut self, val: bool) -> &mut Self {
        self.options.imports = val;
        self
    }

    /// Enable conditional compilation.
    ///
    /// # WESL Reference
    /// Conditional Compilation is a *mandatory* WESL extension.
    /// Spec: [`ConditionalTranslation.md`](https://github.com/wgsl-tooling-wg/wesl-spec/blob/main/ConditionalTranslation.md)
    #[cfg(feature = "condcomp")]
    pub fn use_condcomp(&mut self, val: bool) -> &mut Self {
        self.options.condcomp = val;
        self
    }

    /// Enable generics.
    ///
    /// # WESL Reference
    /// Generics is an *experimental* WESL extension.
    /// Spec: not yet available.
    #[cfg(feature = "generics")]
    pub fn use_generics(&mut self, val: bool) -> &mut Self {
        self.options.generics = val;
        self
    }
    /// Set a conditional compilation feature flag.
    ///
    /// # WESL Reference
    /// Conditional translation is a *mandatory* WESL extension.
    /// Spec: [`ConditionalTranslation.md`](https://github.com/wgsl-tooling-wg/wesl-spec/blob/main/ConditionalTranslation.md)
    #[cfg(feature = "condcomp")]
    pub fn set_feature(&mut self, feat: &str, val: bool) -> &mut Self {
        self.options.features.insert(feat.to_string(), val);
        self
    }
    /// Set conditional compilation feature flags.
    ///
    /// # WESL Reference
    /// Conditional translation is a *mandatory* WESL extension.
    /// Spec: [`ConditionalTranslation.md`](https://github.com/wgsl-tooling-wg/wesl-spec/blob/main/ConditionalTranslation.md)
    #[cfg(feature = "condcomp")]
    pub fn set_features<'a>(
        &mut self,
        feats: impl IntoIterator<Item = (&'a str, bool)>,
    ) -> &mut Self {
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
    pub fn unset_feature(&mut self, feat: &str) -> &mut Self {
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
    pub fn use_stripping(&mut self, val: bool) -> &mut Self {
        self.options.strip = val;
        self
    }
    /// Transform an output into a simplified WGSL that is better supported by implementors.
    ///
    /// Currently, lower performs the following transforms:
    /// * remove aliases (inlined)
    /// * remove consts (inlined)
    /// * remove deprecated, non-standard attributes
    /// * remove import declarations
    ///
    /// Customizing this behavior is not possible currently. The following transforms may
    /// be available in the future:
    /// * make variable types explicit
    /// * make implicit conversions (conversion rank) explicit
    /// * concretize abstract literals
    /// * remove unused variables / code with no side-effects
    /// * evaluate const-expressions
    ///
    /// # WESL Reference
    /// Lowering is an *experimental* WESL extension.
    /// Spec: not yet available.
    pub fn use_lower(&mut self, val: bool) -> &mut Self {
        self.options.lower = val;
        self
    }
    /// If stripping is enabled, specifies which entrypoints to keep in the final WGSL.
    /// All entrypoints are kept by default.
    ///
    /// # WESL Reference
    /// Code stripping is an *optional* WESL extension.
    /// Customizing entrypoints returned by the compiler is explicitly allowed by the spec.
    /// Spec: not yet available.
    pub fn keep_entrypoints(&mut self, entries: Vec<String>) -> &mut Self {
        self.options.keep = Some(entries);
        self
    }
    /// If stripping is enabled, keep all entrypoints in the root WESL module.
    /// This is the default. See [`Wesl::keep_entrypoints`].
    ///
    /// # WESL Reference
    /// Code stripping is an *optional* WESL extension.
    /// Customizing entrypoints returned by the compiler is explicitly allowed by the spec.
    /// Spec: not yet available.
    pub fn keep_all_entrypoints(&mut self) -> &mut Self {
        self.options.keep = None;
        self
    }
}

/// This type implements `Display`, call `to_string()` to get the compiled WGSL.
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

impl Display for CompileResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.syntax.fmt(f)
    }
}

/// This type implements `Display`, call `to_string()` to get the function return value.
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

impl<'a> Display for ExecResult<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.inst.fmt(f)
    }
}

/// This type implements `Display`, call `to_string()` to get the evaluation result.
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

impl<'a> Display for EvalResult<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.inst.fmt(f)
    }
}

#[cfg(feature = "eval")]
impl CompileResult {
    /// Evaluate a const-expression in the context of this compilation result.
    ///
    /// Highly experimental. Not all builtin `@const` WGSL functions are supported yet.
    /// Contrary to [`Wesl::eval`], the provided expression can reference declarations
    /// in the compiled WGSL: global const-declarations and user-defined functions with
    /// the `@const` attribute.
    ///
    /// # WESL Reference
    /// The user-defined `@const` attribute is non-standard.
    /// See issue [#46](https://github.com/wgsl-tooling-wg/wesl-spec/issues/46#issuecomment-2389531479).
    pub fn eval(&self, source: &str) -> Result<EvalResult, Error> {
        let expr = source
            .parse::<syntax::Expression>()
            .map_err(|e| Error::Error(Diagnostic::from(e).with_source(source.to_string())))?;
        let (inst, ctx) = eval(&expr, &self.syntax);
        let inst = inst.map_err(|e| {
            Diagnostic::from(e)
                .with_source(source.to_string())
                .with_ctx(&ctx)
        });

        let inst = if let Some(sourcemap) = &self.sourcemap {
            inst.map_err(|e| Error::Error(e.with_sourcemap(sourcemap)))
        } else {
            inst.map_err(Error::Error)
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
            ty: syntax::TypeExpression::new(Ident::new(entrypoint.to_string())),
            arguments: Vec::new(),
        });

        let (inst, ctx) = exec(&expr, &self.syntax, bindings, overrides);
        let inst = inst.map_err(|e| {
            Diagnostic::from(e)
                .with_source(expr.to_string())
                .with_ctx(&ctx)
        });

        let inst = if let Some(sourcemap) = &self.sourcemap {
            inst.map_err(|e| Error::Error(e.with_sourcemap(sourcemap)))
        } else {
            inst.map_err(Error::Error)
        }?;

        Ok(ExecResult { inst, ctx })
    }
}

impl<R: Resolver> Wesl<R> {
    /// Compile a WESL program from a root file.
    ///
    /// The result of `compile` is not necessarily a valid WGSL string. See (TODO) to
    /// validate the output and (TODO) perform convert the output to valid WGSL.
    ///
    /// # WESL Reference
    /// Spec: not available yet.
    pub fn compile(&self, entrypoint: impl AsRef<Path>) -> Result<CompileResult, Error> {
        let entrypoint = Resource::new(entrypoint.as_ref().to_path_buf());

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

    /// Compile a WESL program from a root file and output the result in rust's OUT_DIR.
    ///
    /// This function is meant to be used in a `build.rs` workflow. The compiled WGSL will
    /// be available with the [`include_wesl`] macro. See the crate documentation for a
    /// usage example.
    ///
    /// * The first argument is the path to the entrypoint file relative to the base directory.
    /// * The second argument is the name of the artefact, used in [`include_wesl`].
    ///
    /// # Panics
    /// Panics when compilation fails or if the output file cannot be written.
    /// Pretty-prints the WESL error message to stderr.
    pub fn build_artefact(&self, entrypoint: impl AsRef<Path>, out_name: &str) {
        let entrypoint = entrypoint.as_ref();
        let dirname = std::env::var("OUT_DIR").unwrap();
        let out_name = Path::new(out_name);
        if out_name.iter().count() != 1 || out_name.extension().is_some() {
            eprintln!("`out_name` cannot contain path separators or file extension");
            panic!()
        }
        let mut output = Path::new(&dirname).join(out_name);
        output.set_extension("wgsl");
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
}

fn keep_idents(wesl: &TranslationUnit, keep: &Option<Vec<String>>, strip: bool) -> HashSet<Ident> {
    if strip {
        if let Some(keep) = keep {
            wesl.global_declarations
                .iter()
                .filter_map(|decl| {
                    let ident = decl.ident()?;
                    keep.iter()
                        .any(|name| name == &*ident.name())
                        .then_some(ident.clone())
                })
                .collect()
        } else {
            // when stripping is enabled and keep is unset, we keep the entrypoints (default)
            wesl.entry_points().cloned().collect()
        }
    } else {
        // when stripping is disabled, we keep all declarations in the root module.
        wesl.global_declarations
            .iter()
            .filter_map(|decl| decl.ident())
            .cloned()
            .collect()
    }
}

fn compile_pre_assembly(
    root_resource: &Resource,
    resolver: &impl Resolver,
    mangler: &impl Mangler,
    options: &CompileOptions,
    root_decls: &mut Vec<String>,
) -> Result<TranslationUnit, Error> {
    let resolver = Box::new(resolver);

    #[cfg(feature = "condcomp")]
    let resolver: Box<dyn Resolver> = if options.condcomp {
        Box::new(Preprocessor::new(resolver, |wesl| {
            condcomp::run(wesl, &options.features)?;
            Ok(())
        }))
    } else {
        resolver
    };

    let wesl = resolver.resolve_module(root_resource)?;

    // hack, this is passed by &mut just to return it from the function even in error case.
    *root_decls = wesl
        .global_declarations
        .iter()
        .filter_map(|decl| decl.ident())
        .map(|name| name.to_string())
        .collect_vec();

    #[cfg(feature = "imports")]
    let wesl = if options.imports {
        let mut resolution = if options.lazy {
            let keep = keep_idents(&wesl, &options.keep, options.strip);
            import::resolve_lazy(wesl, root_resource, keep, &resolver)?
        } else {
            import::resolve_eager(wesl, root_resource, &resolver)?
        };
        resolution.mangle(mangler)?;
        if options.validate {
            for module in resolution.modules() {
                validate_wesl(&module.source).map_err(|d| {
                    d.with_resource(
                        module.resource.clone(),
                        resolver.display_name(&module.resource),
                    )
                })?;
            }
        }
        resolution.assemble(options.strip)
    } else {
        wesl
    };
    Ok(wesl)
}

fn compile_post_assembly(
    wesl: &mut TranslationUnit,
    options: &CompileOptions,
    keep: &[String],
) -> Result<(), Error> {
    #[cfg(feature = "generics")]
    if options.generics {
        generics::generate_variants(wesl)?;
        generics::replace_calls(wesl)?;
    };
    if options.validate {
        validate_wgsl(&wesl)?;
    }
    if options.lower {
        lower(wesl, keep)?;
    }
    Ok(())
}

/// Low-level version of [`Wesl::compile`].
pub fn compile(
    root_module: &Resource,
    resolver: &impl Resolver,
    mangler: &impl Mangler,
    options: &CompileOptions,
) -> Result<TranslationUnit, Diagnostic<Error>> {
    let mut root_names = Vec::new();
    let mut wesl = compile_pre_assembly(root_module, resolver, mangler, options, &mut root_names)?;
    let keep = options.keep.as_deref().unwrap_or(&root_names);
    compile_post_assembly(&mut wesl, options, keep)?;
    Ok(wesl)
}

/// Like [`compile`], but provides better error diagnostics and returns the sourcemap.
pub fn compile_sourcemap(
    root_module: &Resource,
    resolver: &impl Resolver,
    mangler: &impl Mangler,
    options: &CompileOptions,
) -> (Result<TranslationUnit, Error>, BasicSourceMap) {
    let sourcemapper = SourceMapper::new(&resolver, &mangler);
    let mut root_names = Vec::new();
    let comp = compile_pre_assembly(
        root_module,
        &sourcemapper,
        &sourcemapper,
        options,
        &mut root_names,
    );
    let mut sourcemap = sourcemapper.finish();
    for name in &root_names {
        sourcemap.add_decl(name.clone(), root_module.clone(), name.clone());
    }
    let keep = options.keep.as_deref().unwrap_or(&root_names);

    let comp = match comp {
        Ok(mut wesl) => compile_post_assembly(&mut wesl, options, keep)
            .map_err(|e| {
                Diagnostic::from(e)
                    .with_output(wesl.to_string())
                    .with_sourcemap(&sourcemap)
                    .unmangle(Some(&sourcemap), Some(&mangler))
                    .into()
            })
            .map(|()| wesl),
        Err(e) => Err(Diagnostic::from(e)
            .with_sourcemap(&sourcemap)
            .unmangle(Some(&sourcemap), Some(&mangler))
            .into()),
    };

    (comp, sourcemap)
}

/// Evaluate a const-expression.
///
/// Only builtin function declarations marked `@const` can be called from const-expressions.
/// Highly experimental. Not all builtin `@const` WGSL functions are supported yet.
#[cfg(feature = "eval")]
pub fn eval_str(expr: &str) -> Result<eval::Instance, Error> {
    let expr = expr
        .parse::<syntax::Expression>()
        .map_err(|e| Error::Error(Diagnostic::from(e).with_source(expr.to_string())))?;
    let wgsl = TranslationUnit::default();
    let (inst, ctx) = eval(&expr, &wgsl);
    inst.map_err(|e| {
        Error::Error(
            Diagnostic::from(e)
                .with_source(expr.to_string())
                .with_ctx(&ctx),
        )
    })
}

/// Low-level version of [`eval_str`].
#[cfg(feature = "eval")]
pub fn eval<'s>(
    expr: &syntax::Expression,
    wgsl: &'s TranslationUnit,
) -> (Result<eval::Instance, EvalError>, eval::Context<'s>) {
    let mut ctx = eval::Context::new(wgsl);
    let res = wgsl.exec(&mut ctx).and_then(|_| expr.eval(&mut ctx));
    (res, ctx)
}

/// Low-level version of [`CompileResult::exec`].
#[cfg(feature = "eval")]
pub fn exec<'s>(
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

#[test]
fn test_send_sync() {
    fn assert_send_sync<T: Send + Sync>() {}
    assert_send_sync::<Wesl<StandardResolver>>();
}
