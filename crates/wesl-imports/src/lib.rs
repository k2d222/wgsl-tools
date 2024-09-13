use std::path::{Path, PathBuf};
use wgsl_parse::syntax::TranslationUnit;

pub use mangle::{
    FileManglerEscape, FileManglerHash, Mangler, FILE_MANGLER_ESCAPE, FILE_MANGLER_HASH,
};
pub use resolve::{Error, FileResolver, FileResource, Module, Resolver};

mod assemble;
mod mangle;
mod resolve;

pub fn run(entry_point: &Path) -> Result<TranslationUnit, Error> {
    let base = entry_point
        .parent()
        .ok_or_else(|| Error::FileNotFound(entry_point.to_string_lossy().to_string()))?
        .to_path_buf();
    let name = PathBuf::from(
        entry_point
            .file_name()
            .ok_or_else(|| Error::FileNotFound(entry_point.to_string_lossy().to_string()))?,
    );

    let resolver = FileResolver::new(base);
    let mangler = FileManglerEscape::default();
    let entry_point = FileResource::from(name);

    let module = Module::resolve(&entry_point, &resolver, &mangler)?;

    let wgsl = module.assemble();
    Ok(wgsl)
}
