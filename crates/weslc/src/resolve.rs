use crate::Error;

use wgsl_parse::{
    syntax::{self, TranslationUnit},
    Parser,
};

use std::{
    fmt::Display,
    fs,
    hash::Hash,
    path::{Path, PathBuf},
};

#[derive(Clone, Debug, thiserror::Error)]
pub enum ResolveError {
    #[error("parse error: `{0}`")]
    ParseError(wgsl_parse::Error),
    #[error("failed to read file `{0}`")]
    FileNotFound(String),
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
            .map_err(|_| ResolveError::FileNotFound(path.0.to_string_lossy().to_string()))?;
        let wesl =
            Parser::parse_str(&source).map_err(|e| ResolveError::ParseError(e.into_owned()))?;
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
