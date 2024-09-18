use std::{fmt::Display, path::PathBuf};

mod assemble;
mod mangle;
mod module;

use itertools::Itertools;
pub use module::{resolve, Module};

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

#[derive(Clone, Debug, thiserror::Error)]
pub enum ImportError {
    #[error("duplicate imported item `{0}`")]
    DuplicateSymbol(String),
    #[error("failed to resolve import path `{0}`")]
    ResolutionFailure(ImportPath),
    #[error("module `{0}` has no exported symbol `{1}`")]
    MissingExport(ImportPath, String),
    #[error("circular dependency between `{0}` and `{1}`")]
    CircularDependency(ImportPath, ImportPath),
}
