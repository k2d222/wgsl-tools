pub mod assemble;
pub mod parse;
pub mod resolve;

use core::panic;
use std::path::Path;

use self::resolve::{FileResolver, FileResource, Module, Resolver};

pub(crate) fn run(path: &Path) {
    let resolver = FileResolver::default();
    let entry_point = FileResource::from(path.to_path_buf());
    let module = match Module::resolve(&entry_point, &resolver) {
        Ok(module) => module,
        Err(err) => {
            eprintln!("error parsing `{entry_point}`: {err}");
            panic!("could not parse entry point");
        }
    };

    let assembled = module.assemble();
}
