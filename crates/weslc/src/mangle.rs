use std::hash::DefaultHasher;
use std::hash::Hash;
use std::hash::Hasher;

use itertools::Itertools;

use super::Resource;

pub trait Mangler {
    fn mangle(&self, resource: &Resource, item: &str) -> String;
}

/// A mangler for the filesystem resources hashes the resource identifier.
/// e.g. `foo/bar/baz.wgsl item => item_32938483840293402930392`
#[derive(Default, Clone, Debug)]
pub struct FileManglerHash;

pub const MANGLER_HASH: FileManglerHash = FileManglerHash;

impl Mangler for FileManglerHash {
    fn mangle(&self, resource: &Resource, item: &str) -> String {
        let mut hasher = DefaultHasher::new();
        resource.hash(&mut hasher);
        item.hash(&mut hasher);
        let hash = hasher.finish();
        format!("{item}_{hash}")
    }
}

/// A mangler for the filesystem resources that gives the escaped path to the resource.
/// e.g. `foo/bar/baz.wgsl item => foo_bar_bazwgsl_item`
///
/// Warning: the file path segments must be valid wgsl identifiers.
#[derive(Default, Clone, Debug)]
pub struct FileManglerEscape;

pub const MANGLER_ESCAPE: FileManglerEscape = FileManglerEscape;

impl Mangler for FileManglerEscape {
    fn mangle(&self, resource: &Resource, item: &str) -> String {
        let path = resource.path().with_extension("");
        let path = path
            .iter()
            .map(|p| p.to_string_lossy().replace('_', "__"))
            .format("_");
        format!("{path}_{item}")
    }
}

/// A mangler that just returns the identifer as-is (no mangling).
/// e.g. `foo/bar/baz.wgsl item => item`
///
/// Warning: will break the program in case of name conflicts.
#[derive(Default, Clone, Debug)]
pub struct NoMangler;

pub const MANGLER_NONE: NoMangler = NoMangler;

impl Mangler for NoMangler {
    fn mangle(&self, _resource: &Resource, item: &str) -> String {
        item.to_string()
    }
}
