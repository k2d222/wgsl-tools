use crate::Error;
use itertools::Itertools;
use thiserror::Error;
use wgsl_parse::{
    syntax::{self, TranslationUnit},
    Parser,
};

use std::{
    collections::HashMap,
    fmt::Display,
    fs,
    hash::Hash,
    path::{Path, PathBuf},
    rc::Rc,
};

use super::mangle::Mangler;

/// convenience functions to build a module resolver

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ImportPath(Vec<String>);

impl ImportPath {
    pub fn to_pathbuf(&self) -> PathBuf {
        PathBuf::from_iter(self.0.iter())
    }
}

impl Display for ImportPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.iter().format("/"))
    }
}

// XXX: are imports supposed to be order-independent?
#[allow(type_alias_bounds)]
type Imports<R: Resource> = HashMap<R, Vec<syntax::ImportItem>>;
#[allow(type_alias_bounds)]
type Modules<R: Resource> = HashMap<R, Rc<Module<R>>>;

#[derive(Clone, Debug, Error)]
pub enum ImportError {
    #[error("parse error: `{0}`")]
    ParseError(wgsl_parse::Error),
    #[error("duplicate imported item `{0}`")]
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

/// Flatten imports to a list of resources and items to import.
fn resolve_import_paths<R: Resolver>(
    imports: &[syntax::Import],
    parent: &R::Resource,
    resolver: &R,
) -> Result<Imports<R::Resource>, Error> {
    let mut res = Imports::<R::Resource>::new();

    for import in imports {
        match &import.content {
            syntax::ImportContent::Star(item) => {
                let mut path = import.path.clone();
                path.push(item.name.clone());
                let resource = resolver.resolve_path(&path, Some(parent))?;
                if let Some(entry) = res.get_mut(&resource) {
                    entry.push(item.clone());
                } else {
                    res.insert(resource, vec![item.clone()]);
                }
            }
            syntax::ImportContent::Item(item) => {
                let resource = resolver.resolve_path(&import.path, Some(parent))?;
                if let Some(entry) = res.get_mut(&resource) {
                    entry.push(item.clone());
                } else {
                    res.insert(resource, vec![item.clone()]);
                }
            }
            syntax::ImportContent::Collection(imports) => {
                // prepend the parent import path to the children in the collection
                let imports = imports
                    .clone()
                    .into_iter()
                    .map(|mut child| {
                        let mut path = import.path.clone();
                        path.extend(child.path.into_iter());
                        child.path = path;
                        child
                    })
                    .collect::<Vec<_>>();

                let resolved = resolve_import_paths(&imports, parent, resolver)?;
                for (resource, items) in resolved {
                    if let Some(entry) = res.get_mut(&resource) {
                        entry.extend_from_slice(items.as_slice());
                    } else {
                        res.insert(resource, items);
                    }
                }
            }
        }
    }

    Ok(res)
}

pub struct Module<R: Resource> {
    pub(crate) source: TranslationUnit,
    pub(crate) resource: R,
    pub(crate) imports: Imports<R>,
    pub(crate) resolutions: Modules<R>,
}

impl<R: Resource> Module<R> {
    pub fn new(
        source: syntax::TranslationUnit,
        resource: R,
        imports: Imports<R>,
    ) -> Result<Self, Error> {
        Ok(Self {
            source,
            resource,
            imports,
            resolutions: HashMap::new(),
        })
    }

    fn resolve_import(&mut self, resource: &R, module: Rc<Module<R>>) {
        self.resolutions.insert(resource.clone(), module);
    }

    #[allow(unused)]
    fn is_resolved(&self) -> bool {
        self.imports
            .keys()
            .all(|path| self.resolutions.contains_key(path))
    }
}

pub trait Resource: Display + Clone + Eq + Hash {}
impl<T: Display + Clone + Eq + Hash> Resource for T {}

/// a Resolver is responsible for turning a import path into a unique resource identifer (`Resource`),
/// and providing the source file.
pub trait Resolver {
    type Resource: Resource;
    /// Tries to resolve the `import_path` to a source file.
    /// `parent_resource` is the resource identifier of the importer module.
    fn resolve_path(
        &self,
        import_path: &[String],
        parent_resource: Option<&Self::Resource>,
    ) -> Result<Self::Resource, Error>;

    /// Tries to resolve a source file identified by a resource.
    fn resolve_file(&self, resource: &Self::Resource) -> Result<syntax::TranslationUnit, Error>;
}

#[derive(Default)]
pub struct FileResolver {
    base: PathBuf,
}

// just a wrapper around PathBuf to satisfy the `Display` requirement of Resolver
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct FileResource(PathBuf);

impl FileResource {
    pub fn path(&self) -> &Path {
        &self.0
    }
}

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
    #[allow(dead_code)]
    pub fn new(base: PathBuf) -> Self {
        Self { base }
    }
}

impl Resolver for FileResolver {
    type Resource = FileResource;

    fn resolve_path(
        &self,
        import_path: &[String],
        parent_path: Option<&FileResource>,
    ) -> Result<FileResource, Error> {
        let mut rel_path = PathBuf::from_iter(import_path.iter());
        rel_path.set_extension("wgsl");

        let mut base_path = if rel_path.is_absolute() {
            PathBuf::new()
        } else if let Some(parent_path) = parent_path {
            // SAFETY: parent_path must be a file, therefore must have a contaning directory
            parent_path.0.parent().unwrap().to_path_buf()
        } else {
            PathBuf::new()
        };

        base_path.extend(&rel_path);

        Ok(FileResource(base_path))
    }

    fn resolve_file(&self, path: &FileResource) -> Result<TranslationUnit, Error> {
        let mut with_base = self.base.to_path_buf();
        with_base.extend(&path.0);
        let source = fs::read_to_string(&with_base)
            .map_err(|_| ImportError::FileNotFound(path.0.to_string_lossy().to_string()))?;
        let wesl =
            Parser::parse_str(&source).map_err(|e| ImportError::ParseError(e.into_owned()))?;
        Ok(wesl)
    }
}

pub struct PreprocessResolver<R: Resolver, F: Fn(&mut TranslationUnit) -> Result<(), Error>>(
    pub R,
    pub F,
);

impl<R: Resolver, F: Fn(&mut TranslationUnit) -> Result<(), Error>> Resolver
    for PreprocessResolver<R, F>
{
    type Resource = R::Resource;

    fn resolve_path(
        &self,
        import_path: &[String],
        parent_resource: Option<&Self::Resource>,
    ) -> Result<Self::Resource, Error> {
        self.0.resolve_path(import_path, parent_resource)
    }

    fn resolve_file(&self, resource: &Self::Resource) -> Result<TranslationUnit, Error> {
        let mut res = self.0.resolve_file(resource)?;
        self.1(&mut res)?;
        Ok(res)
    }
}

fn resolve_rec<R: Resource>(
    resource: &R,
    resolver: &impl Resolver<Resource = R>,
    mangler: &(impl Mangler<R> + ?Sized),
    visited: &mut Modules<R>,
) -> Result<Module<R>, Error> {
    let source = resolver.resolve_file(resource)?;
    let imports = resolve_import_paths(&source.imports, resource, resolver)?;
    let mut module = Module::new(source, resource.clone(), imports.clone())?;

    for child in imports.keys() {
        if let Some(submodule) = visited.get(child) {
            module.resolve_import(child, submodule.clone());
        } else {
            let submodule = resolve_rec(child, resolver, mangler, visited)?;
            let submodule = Rc::new(submodule);
            module.resolve_import(child, submodule.clone());
            visited.insert(child.clone(), submodule);
        }
    }

    // mangle only after imports have been resolved.
    assert!(module.is_resolved());
    module.mangle(mangler)?;

    Ok(module)
}

pub fn resolve<R: Resource, M: Mangler<R> + ?Sized>(
    resource: &R,
    resolver: &impl Resolver<Resource = R>,
    mangler: &M,
) -> Result<Module<R>, crate::Error> {
    let mut submodules = Modules::new();
    let module = resolve_rec(resource, resolver, mangler, &mut submodules)?;
    Ok(module)
}
