use std::path::{Path, PathBuf};

use mangle::FileManglerEscape;
use resolve::{FileResolver, FileResource, ImportError, Module};
use wgsl_parse::syntax::TranslationUnit;

mod assemble;
mod mangle;
mod resolve;

pub fn compile(entry_point: &Path) -> Result<TranslationUnit, ImportError> {
    let base = entry_point
        .parent()
        .ok_or_else(|| ImportError::FileNotFound(entry_point.to_string_lossy().to_string()))?
        .to_path_buf();
    let name = PathBuf::from(
        entry_point
            .file_name()
            .ok_or_else(|| ImportError::FileNotFound(entry_point.to_string_lossy().to_string()))?,
    );

    let resolver = FileResolver::new(base);
    let mangler = FileManglerEscape::default();
    let entry_point = FileResource::from(name);

    let module = Module::resolve(&entry_point, &resolver, &mangler)?;

    let wgsl = module.assemble();
    Ok(wgsl)
}
