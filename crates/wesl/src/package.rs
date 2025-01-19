use std::path::{Path, PathBuf};

use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use wgsl_parse::syntax::TranslationUnit;

use crate::{validate, Diagnostic, Error, Resource, SyntaxUtil};

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

/// A builder that generates code for WESL packages.
///
/// It is designed to be used in a build script (`build.rs` file). Add `wesl` to the
/// build-dependencies of your project and enable the `package` feature flag.
///
/// ```rs
/// // in build.rs
/// fn main() {
///    wesl::PkgBuilder::new("my_package")
///        // read all wesl files in the directory "src/shaders"
///        .scan_directory("src/shaders")
///        .expect("failed to scan WESL files")
///        // validation is optional
///        .validate()
///        .map_err(|e| eprintln!("{e}"))
///        .expect("validation error")
///        // write "my_package.rs" in OUT_DIR
///        .build_artefact()
///        .expect("failed to build artefact");
/// }
/// ```
/// Then, in your `lib.rs` file, expose the generated module with the [`wesl_pkg`] macro.
/// ```rs
/// // in src/lib.rs
/// use wesl::wesl_pkg;
/// wesl_pkg!(my_package);
/// ```
///
/// The package name must be a valid rust identifier, E.g. it must not contain dashes `-`.
/// Dashes are replaced with underscores `_`.
pub struct PkgBuilder {
    name: String,
}

pub struct Module {
    name: String,
    source: String,
    submodules: Vec<Module>,
}

impl PkgBuilder {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.replace('-', "_"),
        }
    }

    /// Reads all files in a directory to build the package.
    pub fn scan_directory(self, path: impl AsRef<Path>) -> std::io::Result<Module> {
        let dir = path.as_ref().to_path_buf();
        // we look for a file with the same name as the dir in the same directory
        let mut lib_path = dir.clone();
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
                    let name = path
                        .file_stem()
                        .unwrap()
                        .to_string_lossy()
                        .replace('-', "_");
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

        if dir.is_dir() {
            process_dir(&mut module, &dir)?;
        }

        Ok(module)
    }
}

impl Module {
    /// generate the rust code that holds the packaged wesl files.
    /// you probably want to use [`Self::build`] instead.
    pub fn codegen(&self) -> std::io::Result<String> {
        fn codegen_module(module: &Module) -> TokenStream {
            let name = &module.name;
            let source = &module.source;

            let submodules = module.submodules.iter().map(|submod| {
                let name = &submod.name;
                quote! {
                    #name,
                }
            });

            let match_arms = module.submodules.iter().map(|submod| {
                let name = &submod.name;
                let ident = format_ident!("{}", name);
                quote! {
                    #name => Some(&#ident::Mod),
                }
            });

            let subquotes = module.submodules.iter().map(|submod| {
                let ident = format_ident!("{}", module.name);
                let module = codegen_module(submod);
                quote! {
                    pub mod #ident {
                        use super::PkgModule;
                        #module
                    }
                }
            });

            quote! {
                pub struct Mod;

                impl PkgModule for Mod {
                    fn name(&self) -> &'static str {
                        #name
                    }
                    fn source(&self) -> &'static str {
                        #source
                    }
                    fn submodules(&self) -> &[&dyn PkgModule] {
                        static SUBMODULES: &[&dyn PkgModule] = &[
                            #(#submodules)*
                        ];
                        &SUBMODULES
                    }
                    fn submodule(&self, name: &str) -> Option<&'static dyn PkgModule> {
                        match name {
                            #(#match_arms)*
                            _ => None,
                        }
                    }
                }

                #(#subquotes)*
            }
        }

        let tokens = codegen_module(self);
        Ok(tokens.to_string())
    }

    /// run validation checks on each of the scanned files.
    pub fn validate(self) -> Result<Self, Error> {
        fn validate_module(module: &Module) -> Result<(), Error> {
            let resource = Resource::from(PathBuf::from(&module.name));
            let mut wesl: TranslationUnit = module.source.parse().map_err(|e| {
                Diagnostic::from(e)
                    .with_file(resource)
                    .with_source(module.source.clone())
            })?;
            wesl.retarget_idents();
            validate(&wesl)?;
            for module in &module.submodules {
                validate_module(module)?;
            }
            Ok(())
        }
        validate_module(&self)?;
        Ok(self)
    }

    /// generate the build artefact that can then be exposed by the [`wesl_pkg`] macro.
    ///
    /// this function must be called from a `build.rs` file. Refer to the crate documentation
    /// for more details.
    ///
    /// # Panics
    /// panics if the OUT_DIR environment variable is not set. This should not happen if
    /// ran from a `build.rs` file.
    pub fn build_artefact(&self) -> std::io::Result<()> {
        let code = self.codegen()?;
        let out_dir = Path::new(
            &std::env::var_os("OUT_DIR").expect("OUT_DIR environment variable is not defined"),
        )
        .join(format!("{}.rs", self.name));
        std::fs::write(&out_dir, code)?;
        Ok(())
    }
}
