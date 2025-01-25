use crate::{Diagnostic, Error, SyntaxUtil};

use itertools::Itertools;
use wgsl_parse::syntax::{Ident, TranslationUnit};

use std::{
    borrow::Cow,
    cell::RefCell,
    collections::HashMap,
    fmt::Display,
    fs,
    path::{Component, Path, PathBuf},
};

#[derive(Clone, Debug, thiserror::Error)]
pub enum ResolveError {
    #[error("invalid import path: `{0}` ({1})")]
    InvalidResource(Resource, String),
    #[error("file not found: `{0}` ({1})")]
    FileNotFound(PathBuf, String),
    #[error("{0}")]
    Error(#[from] Diagnostic<Error>),
}

type E = ResolveError;

/// A resource uniquely identify an importable module (file).
///
/// Each module must be associated with a unique `Resource`, and a `Resource` must
/// identify a unique module.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Resource {
    path: PathBuf,
}

/// An importable uniquely identify a declaration.
///
/// Each declaration must be associated with a unique `Importable`, and an `Importable` must
/// identify a unique declaration.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Importable {
    pub resource: Resource,
    pub ident: Ident,
}

fn clean_path(path: impl AsRef<Path>) -> PathBuf {
    let mut res = PathBuf::new();
    for comp in path.as_ref().with_extension("").components() {
        match comp {
            Component::Prefix(_) => {}
            Component::RootDir => {
                res.push(comp);
            }
            Component::CurDir => {
                if res == Path::new("") {
                    res.push(comp)
                }
            }
            Component::ParentDir => {
                if !res.pop() {
                    res.push(comp);
                }
            }
            Component::Normal(_) => res.push(comp),
        }
    }
    res
}

impl Resource {
    pub fn new(path: impl AsRef<Path>) -> Self {
        Self {
            path: clean_path(path),
        }
    }
    pub fn path(&self) -> &Path {
        &self.path
    }
    pub fn push(&mut self, item: &str) {
        self.path.push(item);
    }
    pub fn parent(&self) -> Option<Self> {
        self.path.parent().map(|p| Self {
            path: p.to_path_buf(),
        })
    }
    pub fn first(&self) -> Option<&str> {
        self.path.iter().next().map(|p| p.to_str().unwrap())
    }
    pub fn last(&self) -> Option<&str> {
        self.path.iter().last().map(|p| p.to_str().unwrap())
    }
    pub fn join(&self, suffix: impl AsRef<Path>) -> Self {
        let mut path = self.path.clone();
        path.push(suffix);
        Self {
            path: clean_path(path),
        }
    }
}

impl Display for Resource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn fmt_path<'a>(path: impl Iterator<Item = Component<'a>> + 'a) -> impl Display + 'a {
            path.filter_map(|seg| match seg {
                std::path::Component::Prefix(_) | std::path::Component::RootDir => None,
                std::path::Component::CurDir => Some("self"),
                std::path::Component::ParentDir => Some("super"),
                std::path::Component::Normal(str) => str.to_str(),
            })
            .format("::")
        }
        if self.path.has_root() {
            write!(f, "{}", fmt_path(self.path.components().skip(1)))
        } else if self.path.starts_with(".") || self.path.starts_with("..") {
            write!(f, "{}", fmt_path(self.path.components()))
        } else {
            write!(f, "crate::{}", fmt_path(self.path.components()))
        }
    }
}

/// A Resolver is responsible for turning a import path into a unique resource identifer ([`Resource`]),
/// and providing the source file.
///
/// `Resolver` implementations must respect the following constraints:
/// * TODO
pub trait Resolver {
    /// Tries to resolve a source file identified by a resource.
    fn resolve_source<'a>(&'a self, resource: &Resource) -> Result<Cow<'a, str>, E>;
    fn source_to_module(&self, source: &str, resource: &Resource) -> Result<TranslationUnit, E> {
        let mut wesl: TranslationUnit = source.parse().map_err(|e| {
            Diagnostic::from(e)
                .with_file(resource.clone())
                .with_source(source.to_string())
        })?;
        wesl.retarget_idents(); // it's important to call that early on to have identifiers point at the right declaration.
        Ok(wesl)
    }
    fn resolve_module(&self, resource: &Resource) -> Result<TranslationUnit, E> {
        let source = self.resolve_source(resource)?;
        let wesl = self.source_to_module(&source, resource)?;
        Ok(wesl)
    }
}

impl<T: Resolver + ?Sized> Resolver for Box<T> {
    fn resolve_source<'a>(&'a self, resource: &Resource) -> Result<Cow<'a, str>, E> {
        (**self).resolve_source(resource)
    }
    fn source_to_module(&self, source: &str, resource: &Resource) -> Result<TranslationUnit, E> {
        (**self).source_to_module(source, resource)
    }
    fn resolve_module(&self, resource: &Resource) -> Result<TranslationUnit, E> {
        (**self).resolve_module(resource)
    }
}

impl<T: Resolver> Resolver for &T {
    fn resolve_source<'a>(&'a self, resource: &Resource) -> Result<Cow<'a, str>, E> {
        (**self).resolve_source(resource)
    }
    fn source_to_module(&self, source: &str, resource: &Resource) -> Result<TranslationUnit, E> {
        (**self).source_to_module(source, resource)
    }
    fn resolve_module(&self, resource: &Resource) -> Result<TranslationUnit, E> {
        (**self).resolve_module(resource)
    }
}

/// A resolver that never resolves anything.
///
/// Returns [`ResolveError::FileNotFound`] when calling [`Resolver::resolve_source`].
#[derive(Default, Clone, Debug)]
pub struct NoResolver;

impl Resolver for NoResolver {
    fn resolve_source<'a>(&'a self, resource: &Resource) -> Result<Cow<'a, str>, E> {
        Err(E::InvalidResource(
            resource.clone(),
            "no resolver".to_string(),
        ))
    }
}

/// A resolver that remembers to avoid multiple fetches.
pub struct CacheResolver<'a> {
    resolver: Box<dyn Resolver + 'a>,
    cache: RefCell<HashMap<Resource, String>>,
}

impl<'a> CacheResolver<'a> {
    pub fn new(resolver: Box<dyn Resolver + 'a>) -> Self {
        Self {
            resolver,
            cache: Default::default(),
        }
    }
}

impl<'a> Resolver for CacheResolver<'a> {
    fn resolve_source<'b>(&'b self, resource: &Resource) -> Result<Cow<'b, str>, E> {
        let mut cache = self.cache.borrow_mut();

        let source = if let Some(source) = cache.get(resource) {
            source
        } else {
            let source = self.resolver.resolve_source(resource)?;
            cache.insert(resource.clone(), source.into_owned());
            cache.get(resource).unwrap()
        };

        Ok(source.clone().into())
    }
}

/// A resolver that looks for files in the filesystem.
///
/// This is the default resolver.
#[derive(Default)]
pub struct FileResolver {
    base: PathBuf,
    extension: &'static str,
}

impl FileResolver {
    /// `base` is the root directory to which absolute paths refer to.
    pub fn new(base: impl AsRef<Path>) -> Self {
        Self {
            base: base.as_ref().to_path_buf(),
            extension: "wesl",
        }
    }

    /// Look for files that ends with a different extension. Default: "wesl".
    pub fn set_extension(&mut self, extension: &'static str) {
        self.extension = extension;
    }
}

impl Resolver for FileResolver {
    fn resolve_source<'a>(&'a self, resource: &Resource) -> Result<Cow<'a, str>, E> {
        if resource.path().has_root() {
            return Err(E::InvalidResource(
                resource.clone(),
                "not a file".to_string(),
            ));
        }
        let mut path = self.base.to_path_buf();
        path.extend(resource.path());
        let has_extension = path.extension().is_some();
        if !has_extension {
            path.set_extension(self.extension);
        }

        let source = fs::read_to_string(&path)
            .or_else(|e| {
                if !has_extension {
                    path.set_extension("wgsl");
                    fs::read_to_string(&path)
                } else {
                    Err(e)
                }
            })
            .map_err(|_| E::FileNotFound(path, "physical file".to_string()))?;

        Ok(source.into())
    }
}

/// A resolver that resolves WESL modules added with [`VirtualResolver::add_module`].
///
/// This can be used in platforms that lack a filesystem (e.g. WASM) or for
/// runtime-generated files.
#[derive(Default)]
pub struct VirtualResolver {
    files: HashMap<Resource, String>,
}

impl VirtualResolver {
    pub fn new() -> Self {
        Self {
            files: HashMap::new(),
        }
    }

    /// resolves imports in `path` with the given WESL string.
    pub fn add_module(&mut self, path: impl AsRef<Path>, file: String) {
        self.files.insert(Resource::new(path), file);
    }

    pub fn get_module(&self, resource: &Resource) -> Result<&str, Error> {
        let source = self.files.get(resource).ok_or_else(|| {
            E::FileNotFound(resource.path.to_path_buf(), "virtual module".to_string())
        })?;
        Ok(source)
    }
}

impl Resolver for VirtualResolver {
    fn resolve_source<'a>(&'a self, resource: &Resource) -> Result<Cow<'a, str>, E> {
        let source = self
            .get_module(resource)
            .map_err(|e| Diagnostic::from(e).with_file(resource.clone()))?;
        Ok(source.into())
    }
}

// trait alias
pub trait ResolveFn: Fn(&mut TranslationUnit) -> Result<(), Error> {}
impl<T: Fn(&mut TranslationUnit) -> Result<(), Error>> ResolveFn for T {}

/// A WESL module preprocessor.
///
/// The preprocess function will be called each time the WESL compiler tries accesses a
/// module. The preprocessor can modify the module syntax tree at will.
///
/// The preprocess function *must* always return the same syntax tree for a given module
/// path.
pub struct Preprocessor<'a, F: ResolveFn> {
    pub resolver: Box<dyn Resolver + 'a>,
    pub preprocess: F,
}

impl<'a, F: ResolveFn> Preprocessor<'a, F> {
    pub fn new(resolver: Box<dyn Resolver + 'a>, preprocess: F) -> Self {
        Self {
            resolver,
            preprocess,
        }
    }
}

impl<'a, F: ResolveFn> Resolver for Preprocessor<'a, F> {
    fn resolve_source<'b>(&'b self, resource: &Resource) -> Result<Cow<'b, str>, E> {
        let res = self.resolver.resolve_source(resource)?;
        Ok(res)
    }
    fn source_to_module(&self, source: &str, resource: &Resource) -> Result<TranslationUnit, E> {
        let mut wesl: TranslationUnit = source.parse().map_err(|e| {
            Diagnostic::from(e)
                .with_file(resource.clone())
                .with_source(source.to_string())
        })?;
        wesl.retarget_idents(); // it's important to call that early on to have identifiers point at the right declaration.
        (self.preprocess)(&mut wesl).map_err(|e| {
            Diagnostic::from(e)
                .with_file(resource.clone())
                .with_source(source.to_string())
        })?;
        Ok(wesl)
    }
}

/// A router is a resolver that can dispatch imports to several sub-resolvers based on the
/// import path prefix.
///
/// Add sub-resolvers with [`Router::mount_resolver`].
pub struct Router {
    mount_points: Vec<(PathBuf, Box<dyn Resolver>)>,
    fallback: Option<(PathBuf, Box<dyn Resolver>)>,
}

/// Dispatches resolution of a resource to sub-resolvers.
impl Router {
    pub fn new() -> Self {
        Self {
            mount_points: Vec::new(),
            fallback: None,
        }
    }

    /// Mount a resolver at a given path prefix. All imports that start with this prefix
    /// will be dispatched to that resolver.
    pub fn mount_resolver(&mut self, path: impl AsRef<Path>, resolver: impl Resolver + 'static) {
        let path = path.as_ref().to_path_buf();
        let resolver: Box<dyn Resolver> = Box::new(resolver);
        if path.iter().count() == 0 {
            self.fallback = Some((path, resolver));
        } else {
            self.mount_points.push((path, resolver));
        }
    }

    /// Mount a fallback resolver that is used when no other prefix match.
    pub fn mount_fallback_resolver(&mut self, resolver: impl Resolver + 'static) {
        self.mount_resolver("", resolver);
    }
}

impl Default for Router {
    fn default() -> Self {
        Self::new()
    }
}

impl Resolver for Router {
    fn resolve_source<'a>(&'a self, resource: &Resource) -> Result<Cow<'a, str>, E> {
        let (mount_path, resolver) = self
            .mount_points
            .iter()
            .filter(|(path, _)| resource.path().starts_with(path))
            .max_by_key(|(path, _)| path.iter().count())
            .or(self
                .fallback
                .as_ref()
                .take_if(|(path, _)| resource.path().starts_with(path)))
            .ok_or_else(|| E::InvalidResource(resource.clone(), "no mount point".to_string()))?;

        // SAFETY: we just checked that resource.path() starts with mount_path
        let suffix = resource.path().strip_prefix(mount_path).unwrap();
        let resource = Resource::new(suffix.to_path_buf());
        resolver.resolve_source(&resource)
    }
}

pub trait PkgModule: Sync {
    fn name(&self) -> &'static str;
    fn source(&self) -> &'static str;
    fn submodules(&self) -> &[&dyn PkgModule];
    fn submodule(&self, name: &str) -> Option<&dyn PkgModule> {
        self.submodules()
            .iter()
            .find(|sm| sm.name() == name)
            .copied()
    }
}

pub struct PkgResolver {
    packages: Vec<&'static dyn PkgModule>,
}

impl PkgResolver {
    pub fn new() -> Self {
        Self {
            packages: Vec::new(),
        }
    }

    pub fn add_package(&mut self, pkg: &'static dyn PkgModule) {
        self.packages.push(pkg);
    }
}

impl Default for PkgResolver {
    fn default() -> Self {
        Self::new()
    }
}

impl Resolver for PkgResolver {
    fn resolve_source<'a>(&'a self, resource: &Resource) -> Result<std::borrow::Cow<'a, str>, E> {
        let path = resource.path();
        for pkg in &self.packages {
            // TODO: the resolution algorithm is currently not spec-compliant.
            // https://github.com/wgsl-tooling-wg/wesl-spec/blob/imports-update/Imports.md
            if resource.path().starts_with(pkg.name()) {
                let mut cur_mod = *pkg;
                for segment in path.iter().skip(1) {
                    let name = segment.to_str().ok_or_else(|| {
                        E::InvalidResource(resource.clone(), "invalid unicode".to_string())
                    })?;
                    if let Some(submod) = pkg.submodule(name) {
                        cur_mod = submod;
                    } else {
                        return Err(E::FileNotFound(
                            path.to_path_buf(),
                            format!("in package {}", pkg.name()),
                        ));
                    }
                }
                return Ok(cur_mod.source().into());
            }
        }
        Err(E::FileNotFound(
            resource.path().to_path_buf(),
            "no package found".to_string(),
        ))
    }
}

pub struct StandardResolver {
    pkg: PkgResolver,
    router: Router,
}

impl StandardResolver {
    pub fn new(base: impl AsRef<Path>) -> Self {
        let mut router = Router::new();
        router.mount_fallback_resolver(FileResolver::new(base));
        Self {
            pkg: PkgResolver::new(),
            router,
        }
    }

    pub fn add_package(&mut self, pkg: &'static dyn PkgModule) {
        self.pkg.add_package(pkg)
    }

    pub fn mount_resolver(&mut self, path: impl AsRef<Path>, resolver: impl Resolver + 'static) {
        self.router.mount_resolver(path, resolver);
    }
}

impl Resolver for StandardResolver {
    fn resolve_source<'a>(&'a self, resource: &Resource) -> Result<Cow<'a, str>, E> {
        if resource.path().has_root() {
            let path = resource.path().iter().skip(1).collect::<PathBuf>();
            self.pkg.resolve_source(&Resource::new(path))
        } else {
            self.router.resolve_source(resource)
        }
    }
}
