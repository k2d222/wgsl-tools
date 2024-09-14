use std::path::{Path, PathBuf};
use wgsl_parse::syntax::TranslationUnit;

pub use mangle::{
    FileManglerEscape, FileManglerHash, Mangler, NoMangler, FILE_MANGLER_ESCAPE, FILE_MANGLER_HASH,
    FILE_MANGLER_NONE,
};
pub use resolve::{
    resolve, FileResolver, FileResource, ImportError, Module, PreprocessResolver, Resolver,
    Resource,
};

mod assemble;
mod mangle;
mod resolve;
