use std::hash::DefaultHasher;
use std::hash::Hash;
use std::hash::Hasher;
use std::marker::PhantomData;

use itertools::Itertools;

use super::{FileResource, Resource};

pub trait Mangler<R: Resource> {
    fn mangle(&self, resource: &R, item: &str) -> String;
}

/// A mangler for the filesystem resources hashes the resource identifier.
/// e.g. `foo/bar/baz.wgsl item => item_32938483840293402930392`
#[derive(Default, Clone, Debug)]
pub struct FileManglerHash;

pub const FILE_MANGLER_HASH: FileManglerHash = FileManglerHash;

impl Mangler<FileResource> for FileManglerHash {
    fn mangle(&self, resource: &FileResource, item: &str) -> String {
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

pub const FILE_MANGLER_ESCAPE: FileManglerEscape = FileManglerEscape;

impl Mangler<FileResource> for FileManglerEscape {
    fn mangle(&self, resource: &FileResource, item: &str) -> String {
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
pub struct NoMangler<R: Resource>(PhantomData<R>);

pub const FILE_MANGLER_NONE: NoMangler<FileResource> = NoMangler(PhantomData);

impl<R: Resource> Mangler<R> for NoMangler<R> {
    fn mangle(&self, _resource: &R, item: &str) -> String {
        item.to_string()
    }
}
