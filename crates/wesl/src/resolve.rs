use crate::{error::Diagnostic, Error};

use wgsl_parse::{
    syntax::{self, TranslationUnit},
    Parser,
};

use std::{
    borrow::Cow,
    cell::RefCell,
    collections::HashMap,
    fmt::Display,
    fs,
    path::{Path, PathBuf},
};

#[derive(Clone, Debug, thiserror::Error)]
pub enum ResolveError {
    #[error("failed to read file `{0}`")]
    FileNotFound(String),
}

/// A resource uniquely identify an importable module (file).
///
/// Each module must be associated with a unique Resource, and a Resource must
/// identify a unique Module.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Resource {
    path: PathBuf,
}

impl Resource {
    pub fn path(&self) -> &Path {
        &self.path
    }
}

impl From<PathBuf> for Resource {
    fn from(path: PathBuf) -> Self {
        Self { path }
    }
}

impl Display for Resource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.path.display())
    }
}

/// a Resolver is responsible for turning a import path into a unique resource identifer (`Resource`),
/// and providing the source file.
pub trait Resolver {
    /// Tries to resolve a source file identified by a resource.
    fn resolve_file<'a>(&'a self, resource: &Resource) -> Result<Cow<'a, str>, Error>;
    fn resolve_module(&self, resource: &Resource) -> Result<syntax::TranslationUnit, Diagnostic> {
        let source = self.resolve_file(resource)?;
        let wesl = Parser::parse_str(&source).map_err(|e| {
            Diagnostic::from(e)
                .file(resource.clone())
                .source(source.into_owned())
        })?;
        Ok(wesl)
    }
}

impl<T: Resolver + ?Sized> Resolver for Box<T> {
    fn resolve_file<'a>(&'a self, resource: &Resource) -> Result<Cow<'a, str>, Error> {
        (**self).resolve_file(resource)
    }
    fn resolve_module(&self, resource: &Resource) -> Result<syntax::TranslationUnit, Diagnostic> {
        (**self).resolve_module(resource)
    }
}

impl<T: Resolver> Resolver for &T {
    fn resolve_file<'a>(&'a self, resource: &Resource) -> Result<Cow<'a, str>, Error> {
        (**self).resolve_file(resource)
    }
    fn resolve_module(&self, resource: &Resource) -> Result<syntax::TranslationUnit, Diagnostic> {
        (**self).resolve_module(resource)
    }
}

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
    fn resolve_file<'b>(&'b self, resource: &Resource) -> Result<Cow<'b, str>, Error> {
        let mut cache = self.cache.borrow_mut();

        let source = if let Some(source) = cache.get(resource) {
            source
        } else {
            let source = self.resolver.resolve_file(resource)?;
            cache.insert(resource.clone(), source.into_owned());
            cache.get(resource).unwrap()
        };

        Ok(source.clone().into())
    }
}

#[derive(Default)]
pub struct FileResolver {
    base: PathBuf,
}

impl FileResolver {
    /// `base` is the root directory to which absolute paths refer to.
    pub fn new(base: PathBuf) -> Self {
        Self { base }
    }
}

impl Resolver for FileResolver {
    fn resolve_file<'a>(&'a self, resource: &Resource) -> Result<Cow<'a, str>, Error> {
        let mut path = self.base.to_path_buf();
        path.extend(resource.path());
        path.set_extension("wgsl");

        let source = fs::read_to_string(&path).map_err(|_| {
            ResolveError::FileNotFound(format!("{} (physical file)", path.display()))
        })?;

        Ok(source.into())
    }
}

#[derive(Default)]
pub struct VirtualFileResolver {
    files: HashMap<PathBuf, String>,
}

impl VirtualFileResolver {
    /// `base` is the root directory to which absolute paths refer to.
    pub fn new() -> Self {
        Self {
            files: HashMap::new(),
        }
    }

    pub fn add_file(&mut self, path: PathBuf, file: String) -> Result<(), Error> {
        self.files.insert(path, file);
        Ok(())
    }

    pub fn get_file(&self, resource: &Resource) -> Result<&str, Error> {
        let path = resource.path().with_extension("wgsl");
        let source = self.files.get(&path).ok_or_else(|| {
            ResolveError::FileNotFound(format!("{} (virtual file)", path.display()))
        })?;
        Ok(source)
    }
}

impl Resolver for VirtualFileResolver {
    fn resolve_file<'a>(&'a self, resource: &Resource) -> Result<Cow<'a, str>, Error> {
        let source = self.get_file(resource)?;
        Ok(source.into())
    }
}

pub struct PreprocessResolver<'a, F: Fn(&mut TranslationUnit) -> Result<(), Error>> {
    pub resolver: Box<dyn Resolver + 'a>,
    pub preprocess: F,
}

impl<'a, F: Fn(&mut TranslationUnit) -> Result<(), Error>> PreprocessResolver<'a, F> {
    pub fn new(resolver: Box<dyn Resolver + 'a>, preprocess: F) -> Self {
        Self {
            resolver,
            preprocess,
        }
    }
}

impl<'a, F: Fn(&mut TranslationUnit) -> Result<(), Error>> Resolver for PreprocessResolver<'a, F> {
    fn resolve_file<'b>(&'b self, resource: &Resource) -> Result<Cow<'b, str>, Error> {
        let res = self.resolver.resolve_file(resource)?;
        Ok(res)
    }
    fn resolve_module(&self, resource: &Resource) -> Result<TranslationUnit, Diagnostic> {
        let source = self.resolve_file(resource)?;
        let mut wesl = Parser::parse_str(&source).map_err(|e| {
            Diagnostic::from(e)
                .file(resource.clone())
                .source(source.clone().into())
        })?;
        (self.preprocess)(&mut wesl).map_err(|e| {
            Diagnostic::new(e)
                .file(resource.clone())
                .source(source.into_owned())
        })?;
        Ok(wesl)
    }
}

pub struct DispatchResolver {
    mount_points: Vec<(PathBuf, Box<dyn Resolver>)>,
    fallback: Option<(PathBuf, Box<dyn Resolver>)>,
}

/// Dispatches resolution of a resource to sub-resolvers.
impl DispatchResolver {
    pub fn new() -> Self {
        Self {
            mount_points: Vec::new(),
            fallback: None,
        }
    }

    pub fn mount_resolver(&mut self, path: PathBuf, resolver: Box<dyn Resolver>) {
        if path.iter().count() == 0 {
            self.fallback = Some((path, resolver));
        } else {
            self.mount_points.push((path, resolver));
        }
    }

    pub fn mount_fallback_resolver(&mut self, resolver: Box<dyn Resolver>) {
        self.mount_resolver(PathBuf::new(), resolver);
    }
}

impl Resolver for DispatchResolver {
    fn resolve_file<'a>(&'a self, resource: &Resource) -> Result<Cow<'a, str>, Error> {
        let (mount_path, resolver) = self
            .mount_points
            .iter()
            .filter(|(path, _)| resource.path().starts_with(path))
            .max_by_key(|(path, _)| path.iter().count())
            .or(self.fallback.as_ref())
            .ok_or_else(|| ResolveError::FileNotFound(format!("{resource} (no mount point)")))?;

        // SAFETY: we just checked that resource.path() starts with mount_path
        let suffix = resource.path().strip_prefix(mount_path).unwrap();
        let resource = Resource::from(suffix.to_path_buf());
        resolver.resolve_file(&resource).map_err(|mut e| {
            if let Error::ResolveError(e) = &mut e {
                match e {
                    ResolveError::FileNotFound(msg) => {
                        *msg = format!("{}/{msg}", mount_path.display())
                    }
                }
            };
            e
        })
    }
}
