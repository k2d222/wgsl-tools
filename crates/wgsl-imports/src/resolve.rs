use itertools::Itertools;
use thiserror::Error;
use wgsl_parse::{
    syntax::{self, TranslationUnit},
    Parser,
};

use std::{collections::HashMap, fmt::Display, fs, hash::Hash, path::PathBuf, rc::Rc};

/// convenience functions to build a module resolver

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct ImportPath(Vec<String>);

impl ImportPath {
    fn to_pathbuf(&self) -> PathBuf {
        PathBuf::from_iter(self.0.iter())
    }
}

impl Display for ImportPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.iter().format("/"))
    }
}

type Imports = HashMap<ImportPath, Vec<syntax::ImportItem>>;
type Modules = HashMap<ImportPath, Rc<Module>>;

#[derive(Clone, Debug, Error)]
pub enum ImportError {
    #[error("parse error: `{0}`")]
    ParseError(wgsl_parse::error::Error),
    #[error("duplicate imported symbol `{0}`")]
    DuplicateSymbol(String),
    #[error("failed to resolve import path `{0}`")]
    ResolutionFailure(ImportPath),
    #[error("failed to read file `{0}`")]
    FileNotFound(String),
    #[error("module `{0}` has no exported symbol `{1}`")]
    MissingExport(ImportPath, String),
    #[error("circular dependency between `{0}` and `{1}`")]
    CircularDependency(ImportPath, ImportPath),
}

/// Flatten imports to a list of imports path and items to import.
fn normalize_imports(imports: Vec<syntax::Import>) -> Imports {
    let mut res = Imports::new();

    for import in imports.into_iter() {
        let import_path = ImportPath(import.path);
        match import.content {
            syntax::ImportContent::Star(item) => {
                if let Some(entry) = res.get_mut(&import_path) {
                    entry.push(item);
                } else {
                    res.insert(import_path, vec![item]);
                }
            }
            syntax::ImportContent::Item(item) => {
                if let Some(entry) = res.get_mut(&import_path) {
                    entry.push(item);
                } else {
                    res.insert(import_path, vec![item]);
                }
            }
            syntax::ImportContent::Collection(imports) => {
                let normalized = normalize_imports(imports);
                for (path, items) in normalized.into_iter() {
                    let mut import_path = import_path.0.clone();
                    import_path.extend(path.0);
                    let import_path = ImportPath(import_path);
                    if let Some(entry) = res.get_mut(&import_path) {
                        entry.extend_from_slice(items.as_slice());
                    } else {
                        res.insert(import_path, items);
                    }
                }
            }
        }
    }
    res
}

pub struct Module {
    pub(crate) source: TranslationUnit,
    pub(crate) imports: Imports,
    pub(crate) exports: Option<Vec<String>>,
    pub(crate) resolutions: Modules,
}

impl Module {
    pub fn new(source: syntax::TranslationUnit) -> Result<Self, ImportError> {
        let imports = normalize_imports(source.imports.clone());

        Ok(Self {
            source,
            imports,
            exports: None,
            resolutions: HashMap::new(),
        })
    }

    pub fn resolve_import(&mut self, import_path: &ImportPath, module: Rc<Module>) {
        self.resolutions.insert(import_path.clone(), module);
    }

    fn is_resolved(&self) -> bool {
        self.imports
            .keys()
            .all(|path| self.resolutions.contains_key(path))
    }

    pub fn imports(&self) -> &Imports {
        &self.imports
    }
}

/// a Resolver is responsible for turning a import path into a unique resource identifer (`Resource`),
/// and providing the source file.
pub trait Resolver {
    type Resource: Display + Clone + Eq + Hash;
    /// Tries to resolve the `import_path` to a source file.
    /// `parent_resource` is the resource identifier of the importer module.
    fn resolve_path(
        &self,
        import_path: &ImportPath,
        parent_resource: &Self::Resource,
    ) -> Option<Self::Resource>;

    fn resolve_file(
        &self,
        resource: &Self::Resource,
    ) -> Result<syntax::TranslationUnit, ImportError>;
}

pub struct FileResolver {
    base: PathBuf,
}

// just a wrapper around PathBuf to satisfy the `Display` requirement of Resolver
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct FileResource(PathBuf);

impl Display for FileResource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.display())
    }
}

impl From<PathBuf> for FileResource {
    fn from(path: PathBuf) -> Self {
        Self(path)
    }
}

impl FileResolver {
    /// `base` is the root directory to which absolute paths refer to.
    pub fn new(base: PathBuf) -> Self {
        Self { base }
    }
}

impl Default for FileResolver {
    fn default() -> Self {
        Self {
            base: Default::default(),
        }
    }
}

impl Resolver for FileResolver {
    type Resource = FileResource;

    fn resolve_path(
        &self,
        import_path: &ImportPath,
        parent_path: &FileResource,
    ) -> Option<FileResource> {
        let mut rel_path = import_path.to_pathbuf();
        rel_path.set_extension("wgsl");

        let mut base_path = if rel_path.is_absolute() {
            self.base.clone()
        } else {
            parent_path.0.parent().unwrap().to_path_buf()
        };

        base_path.extend(rel_path.into_iter());

        Some(FileResource(base_path))
    }

    fn resolve_file(&self, path: &FileResource) -> Result<syntax::TranslationUnit, ImportError> {
        let source = fs::read_to_string(&path.0)
            .map_err(|_| ImportError::FileNotFound(path.0.to_string_lossy().to_string()))?;
        Parser::parse_str(&source).map_err(|e| ImportError::ParseError(e.into_owned()))
    }
}

fn parse_module_impl<R: Resolver>(
    resource: &R::Resource,
    resolver: &R,
    visited: &mut Modules,
) -> Result<Module, ImportError> {
    let source = resolver.resolve_file(resource)?;

    let mut module = Module::new(source)?;
    let imports = module.imports().clone();

    for (path, _) in imports.iter() {
        if let Some(submodule) = visited.get(path) {
            module.resolve_import(path, submodule.clone());
        } else {
            let resource = resolver
                .resolve_path(path, resource)
                .ok_or_else(|| ImportError::ResolutionFailure(path.clone()))?;
            let submodule = parse_module_impl(&resource, resolver, visited)?;
            visited.insert(path.clone(), Rc::new(submodule));
        }
    }

    Ok(module)
}

impl Module {
    pub fn resolve<R: Resolver>(
        resource: &R::Resource,
        resolver: &R,
    ) -> Result<Module, ImportError> {
        let mut visited_modules = HashMap::new();
        let module = parse_module_impl(resource, resolver, &mut visited_modules)?;
        Ok(module)
    }
}
