use std::path::{Path, PathBuf};

use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use wesl::{ResolveError, Resolver};

pub struct PkgResolver {
    packages: Vec<&'static dyn Module>,
    fallback: Option<Box<dyn Resolver>>,
}

impl PkgResolver {
    pub fn new() -> Self {
        Self {
            packages: Vec::new(),
            fallback: None,
        }
    }

    pub fn add_package(&mut self, pkg: &'static dyn Module) {
        self.packages.push(pkg);
    }

    pub fn mount_fallback_resolver(&mut self, fallback: impl Resolver + 'static) {
        self.fallback = Some(Box::new(fallback));
    }
}

impl Resolver for PkgResolver {
    fn resolve_source<'a>(
        &'a self,
        resource: &wesl::Resource,
    ) -> Result<std::borrow::Cow<'a, str>, wesl::ResolveError> {
        if resource.path().has_root() {
            let path = resource.path().strip_prefix("/").unwrap();
            for pkg in &self.packages {
                // TODO: the resolution algorithm is currently not spec-compliant.
                // https://github.com/wgsl-tooling-wg/wesl-spec/blob/imports-update/Imports.md
                if path.starts_with(pkg.name()) {
                    let mut cur_mod = *pkg;
                    for segment in path.iter().skip(1) {
                        let name = segment.to_str().ok_or_else(|| {
                            ResolveError::InvalidResource(
                                resource.clone(),
                                "invalid unicode".to_string(),
                            )
                        })?;
                        if let Some(submod) = pkg.submodule(name) {
                            cur_mod = submod;
                        } else {
                            return Err(ResolveError::FileNotFound(
                                path.to_path_buf(),
                                format!("in package {}", pkg.name()),
                            ));
                        }
                    }
                    return Ok(cur_mod.source().into());
                }
            }
            Err(ResolveError::FileNotFound(
                resource.path().to_path_buf(),
                "no package found".to_string(),
            ))
        } else {
            self.fallback
                .as_ref()
                .map(|fallback| fallback.resolve_source(resource))
                .unwrap_or_else(|| {
                    Err(ResolveError::FileNotFound(
                        resource.path().to_path_buf(),
                        "no package found".to_string(),
                    ))
                })
        }
    }
}

pub trait Module: Sync {
    fn name(&self) -> &'static str;
    fn source(&self) -> &'static str;
    fn submodule(&self, name: &str) -> Option<&'static dyn Module>;
}

#[macro_export]
macro_rules! wesl_pkg {
    ($pkg_name:ident) => {
        wesl_pkg!($pkg_name, concat!("/", stringify!($pkg_name), ".rs"));
    };
    ($pkg_name:ident, $source:expr) => {
        pub mod $pkg_name {
            use wesl_pkg::Module;

            include!(concat!(env!("OUT_DIR"), $source));
        }
    };
}

/// A builder that generates code for WESL packages.
///
/// It is designed to be used in a build script (`build.rs` file).
/// ```rs
/// // in build.rs
/// fn main() {
///    wesl_pkg::PackageBuilder::new("my_package")
///        .set_directory("src/shaders")
///        .build()
///        .expect("failed to build WESL package");
/// }
/// ```
/// Then, in your `lib.rs` file, expose the generated module with the [`wesl_pkg`] macro.
/// ```rs
/// // in src/lib.rs
/// wesl_pkg!(my_package);
/// ```
///
/// The package name must be a valid rust identifier, E.g. it must not contain dashes `-`.
#[derive(Clone)]
pub struct PackageBuilder {
    name: String,
    dir: PathBuf,
}

impl PackageBuilder {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            dir: PathBuf::from("src/shader"),
        }
    }

    pub fn set_root(mut self, path: impl AsRef<Path>) -> Self {
        self.dir = path.as_ref().into();
        self
    }

    pub fn build(self) -> std::io::Result<()> {
        struct Module {
            name: String,
            source: String,
            submodules: Vec<Module>,
        }

        // we look for a file with the same name as the dir in the same directory
        let mut lib_path = self.dir.clone();
        lib_path.set_extension("wesl");

        let source = if lib_path.is_file() {
            std::fs::read_to_string(&lib_path)?
        } else {
            lib_path.set_extension("wgsl");
            if lib_path.is_file() {
                std::fs::read_to_string(&lib_path)?
            } else {
                String::from("")
            }
        };

        let mut module = Module {
            name: self.name.clone(),
            source,
            submodules: Vec::new(),
        };

        fn process_dir(module: &mut Module, dir: &Path) -> std::io::Result<()> {
            for entry in std::fs::read_dir(dir)? {
                let entry = entry?;
                let path = entry.path();
                if path.is_file()
                    && path
                        .extension()
                        .is_some_and(|ext| ext == "wesl" || ext == "wgsl")
                {
                    let source = std::fs::read_to_string(&path)?;
                    let name = path.file_stem().unwrap().to_string_lossy().to_string();
                    // we look for a dir with the same name as the file in the same directory
                    let mut subdir = dir.to_path_buf();
                    subdir.push(&name);

                    let mut submod = Module {
                        name,
                        source,
                        submodules: Vec::new(),
                    };

                    if subdir.is_dir() {
                        process_dir(&mut submod, &subdir)?;
                    }

                    module.submodules.push(submod);
                }
            }

            Ok(())
        }

        if self.dir.is_dir() {
            process_dir(&mut module, &self.dir)?;
        }

        fn codegen_module(module: &Module) -> TokenStream {
            let name = &module.name;
            let source = &module.source;

            let match_arms = module.submodules.iter().map(|submod| {
                let name = &submod.name;
                let ident = format_ident!("{}", name.replace('-', "_"));
                quote! {
                    #name => Some(&#ident::Mod),
                }
            });

            let subquotes = module.submodules.iter().map(|submod| {
                let ident = format_ident!("{}", module.name.replace('-', "_"));
                let module = codegen_module(submod);
                quote! {
                    pub mod #ident {
                        use super::Module;
                        #module
                    }
                }
            });

            quote! {
                pub struct Mod;

                impl Module for Mod {
                    fn name(&self) -> &'static str {
                        #name
                    }
                    fn source(&self) -> &'static str {
                        #source
                    }
                    fn submodule(&self, name: &str) -> Option<&'static dyn Module> {
                        match name {
                            #(#match_arms)*
                            _ => None,
                        }
                    }
                }

                #(#subquotes)*
            }
        }

        let tokens = codegen_module(&module);

        let out_dir =
            Path::new(&std::env::var_os("OUT_DIR").unwrap()).join(format!("{}.rs", self.name));
        std::fs::write(&out_dir, tokens.to_string())?;
        Ok(())
    }
}
