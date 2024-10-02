use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::DefaultHasher;
use std::hash::Hash;
use std::hash::Hasher;
use std::path::PathBuf;

use itertools::Itertools;

use super::Resource;

pub trait Mangler {
    fn mangle(&self, resource: &Resource, item: &str) -> String;
    fn unmangle(&self, _mangled: &str) -> Option<(Resource, String)> {
        None
    }
}

impl<T: Mangler + ?Sized> Mangler for Box<T> {
    fn mangle(&self, resource: &Resource, item: &str) -> String {
        (**self).mangle(resource, item)
    }
    fn unmangle(&self, mangled: &str) -> Option<(Resource, String)> {
        (**self).unmangle(mangled)
    }
}

impl<T: Mangler> Mangler for &T {
    fn mangle(&self, resource: &Resource, item: &str) -> String {
        (**self).mangle(resource, item)
    }
    fn unmangle(&self, mangled: &str) -> Option<(Resource, String)> {
        (**self).unmangle(mangled)
    }
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
    fn unmangle(&self, mangled: &str) -> Option<(Resource, String)> {
        let mut parts = mangled.split('_').peekable();
        let mut path_parts = Vec::new();

        while let Some(part) = parts.next() {
            let mut part = part.to_string();
            while parts.peek() == Some(&"") {
                part.push('_');
                parts.next();
            }
            path_parts.push(part.to_string());
        }

        if path_parts.len() < 2 {
            return None;
        }

        let item = path_parts.pop()?;
        let path = PathBuf::from_iter(path_parts);

        let resource = Resource::from(path);
        Some((resource, item))
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
    fn unmangle(&self, mangled: &str) -> Option<(Resource, String)> {
        Some((Resource::from(PathBuf::new()), mangled.to_string()))
    }
}

/// A mangler that remembers and can unmangle.
pub struct CachedMangler<'a, T: Mangler> {
    cache: RefCell<HashMap<String, (Resource, String)>>,
    mangler: &'a T,
}

impl<'a, T: Mangler> CachedMangler<'a, T> {
    pub fn new(mangler: &'a T) -> Self {
        Self {
            cache: Default::default(),
            mangler,
        }
    }
}

impl<'a, T: Mangler> Mangler for CachedMangler<'a, T> {
    fn mangle(&self, resource: &Resource, item: &str) -> String {
        let res = self.mangler.mangle(resource, item);
        let mut cache = self.cache.borrow_mut();
        cache.insert(res.clone(), (resource.clone(), item.to_string()));
        res
    }
    fn unmangle(&self, mangled: &str) -> Option<(Resource, String)> {
        {
            let cache = self.cache.borrow();
            if let Some(res) = cache.get(mangled).cloned() {
                return Some(res);
            }
        }

        self.mangler.unmangle(mangled)
    }
}
