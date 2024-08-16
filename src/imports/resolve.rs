use itertools::Itertools;
use ropey::Rope;
use thiserror::Error;

use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    fs,
    hash::Hash,
    ops::Range,
    path::{Path, PathBuf},
    rc::Rc,
};

use super::parse::{self, WgslParseError};

/// convenience functions to build a module resolver

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ImportPath {
    Quoted(String),
    Hierarchy(Vec<String>),
}

impl ImportPath {
    fn to_pathbuf(&self) -> PathBuf {
        match self {
            ImportPath::Quoted(s) => PathBuf::from(s),
            ImportPath::Hierarchy(v) => PathBuf::from_iter(v.iter()),
        }
    }
}

impl Display for ImportPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ImportPath::Quoted(s) => write!(f, "\"{s}\""),
            ImportPath::Hierarchy(v) => write!(f, "{}", v.iter().format("::")),
        }
    }
}

impl<'s> From<&parse::Path<'s>> for ImportPath {
    fn from(path: &parse::Path<'s>) -> Self {
        match path {
            parse::Path::Quoted(v) => Self::Quoted(v.to_string()),
            parse::Path::Hierarchy(v) => Self::Hierarchy(v.iter().map(|s| s.to_string()).collect()),
        }
    }
}

impl<'a> PartialEq<&parse::Path<'a>> for &ImportPath {
    fn eq(&self, path: &&parse::Path) -> bool {
        match (self, path) {
            (ImportPath::Quoted(v1), parse::Path::Quoted(v2)) => v1 == v2,
            (ImportPath::Hierarchy(v1), parse::Path::Hierarchy(v2)) => v1 == v2,
            _ => false,
        }
    }
}

// convenience functions for library users
impl<'s> parse::ImportSection<'s> {
    pub fn import_paths(&self) -> Vec<ImportPath> {
        self.imports
            .iter()
            .map(|import| ImportPath::from(&import.path))
            .collect()
    }

    pub fn imported_items(&self, import_path: &ImportPath) -> Option<Vec<String>> {
        self.imports
            .iter()
            .find(|import| import_path == &import.path)
            .map(|import| import.items.iter().map(|s| s.name.to_string()).collect())
    }
}

type Imports = HashMap<ImportPath, Vec<(String, String)>>;
type Modules = HashMap<ImportPath, Rc<Module>>;

#[derive(Clone, Debug, Error)]
pub enum ImportError {
    #[error("parse error: {0}")]
    ParseError(#[from] WgslParseError),
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

pub struct Module {
    pub(crate) source: String,
    pub(crate) imports: Imports,
    pub(crate) imports_span: Range<usize>,
    pub(crate) exports: Option<Vec<String>>,
    pub(crate) resolutions: Modules,
}

impl Module {
    pub fn new(source: String) -> Result<Self, ImportError> {
        let parse_imports = parse::parse_imports(&source).map_err(ImportError::ParseError)?;
        let imports = parse_imports.canonicalize()?;
        let imports_span = parse_imports.span;

        Ok(Self {
            source,
            imports,
            imports_span,
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

    pub fn source(&self) -> &str {
        &self.source
    }

    pub fn imports(&self) -> &Imports {
        &self.imports
    }
}

impl<'s> parse::ImportSection<'s> {
    fn canonicalize(&self) -> Result<Imports, ImportError> {
        // ensure all imported symbols are unique
        {
            let mut symbols = HashSet::new();
            for import in self.imports.iter() {
                for item in import.items.iter() {
                    let name = item.rename.unwrap_or(item.name);
                    if symbols.contains(name) {
                        return Err(ImportError::DuplicateSymbol(name.to_owned()));
                    } else {
                        symbols.insert(name);
                    }
                }
            }
        }

        let mut res: Imports = HashMap::new();

        for import in self.imports.iter() {
            let path = ImportPath::from(&import.path);

            let symbols = import.items.iter().map(|item| {
                (
                    item.name.to_owned(),
                    item.rename.unwrap_or(item.name).to_owned(),
                )
            });

            if let Some(v) = res.get_mut(&path) {
                v.extend(symbols);
            } else {
                res.insert(path, symbols.collect());
            }
        }

        Ok(res)
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

    fn resolve_file(&self, resource: &Self::Resource) -> Option<String>;
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
        let rel_path = import_path.to_pathbuf();

        let mut base_path = if rel_path.is_absolute() {
            self.base.clone()
        } else {
            parent_path.0.parent().unwrap().to_path_buf()
        };

        base_path.extend(rel_path.into_iter());

        Some(FileResource(base_path))
    }

    fn resolve_file(&self, path: &FileResource) -> Option<String> {
        fs::read_to_string(&path.0).ok()
    }
}

fn parse_module_impl<R: Resolver>(
    resource: &R::Resource,
    resolver: &R,
    visited: &mut Modules,
) -> Result<Module, ImportError> {
    let source = resolver
        .resolve_file(resource)
        .ok_or_else(|| ImportError::FileNotFound(resource.to_string()))?;

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
