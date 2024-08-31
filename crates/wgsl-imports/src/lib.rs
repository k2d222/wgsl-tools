use std::path::Path;

use resolve::{FileResolver, FileResource, ImportError, Module};
use wgsl_parse::syntax::TranslationUnit;

mod assemble;
mod mangle;
mod resolve;

pub fn compile(entry_point: &Path) -> Result<TranslationUnit, ImportError> {
    let resolver = FileResolver::default();
    let entry_point = FileResource::from(entry_point.to_path_buf());

    let module = Module::resolve(&entry_point, &resolver)?;

    let wgsl = module.assemble();
    Ok(wgsl)
}
