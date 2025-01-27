use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::hash::DefaultHasher;
use std::hash::Hash;
use std::hash::Hasher;
use std::path::PathBuf;

use itertools::Itertools;
use wgsl_parse::syntax::Expression;
use wgsl_parse::syntax::TypeExpression;

use super::Resource;

/// A name mangler is responsible for renaming import-qualified identifiers into valid
/// WGSL identifiers.
///
/// `Mangler` implementations must respect the following constraints:
/// * TODO
///
/// # WESL Reference
/// spec: not yet available.
pub trait Mangler {
    fn mangle(&self, resource: &Resource, item: &str) -> String;
    fn unmangle(&self, _mangled: &str) -> Option<(Resource, String)> {
        None
    }
    fn mangle_types(&self, item: &str, variant: u32, _types: &[TypeExpression]) -> String {
        format!("{item}_{variant}")
    }
}

impl<T: Mangler + ?Sized> Mangler for Box<T> {
    fn mangle(&self, resource: &Resource, item: &str) -> String {
        (**self).mangle(resource, item)
    }
    fn unmangle(&self, mangled: &str) -> Option<(Resource, String)> {
        (**self).unmangle(mangled)
    }
    fn mangle_types(&self, item: &str, variant: u32, types: &[TypeExpression]) -> String {
        (**self).mangle_types(item, variant, types)
    }
}

impl<T: Mangler> Mangler for &T {
    fn mangle(&self, resource: &Resource, item: &str) -> String {
        (**self).mangle(resource, item)
    }
    fn unmangle(&self, mangled: &str) -> Option<(Resource, String)> {
        (**self).unmangle(mangled)
    }
    fn mangle_types(&self, item: &str, variant: u32, types: &[TypeExpression]) -> String {
        (**self).mangle_types(item, variant, types)
    }
}

/// A mangler for the filesystem resources hashes the resource identifier.
/// e.g. `foo/bar/baz.wgsl item => item_32938483840293402930392`
#[derive(Default, Clone, Debug)]
pub struct HashMangler;

impl Mangler for HashMangler {
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
pub struct EscapeMangler;

impl Mangler for EscapeMangler {
    fn mangle(&self, resource: &Resource, item: &str) -> String {
        let path = resource.path().with_extension("");
        let path = path
            .iter()
            .map(|p| p.to_string_lossy().replace('_', "__").replace('/', "pkg"))
            .format("_")
            .to_string();
        format!("{path}_{}", item.replace('_', "__"))
    }
    fn unmangle(&self, mangled: &str) -> Option<(Resource, String)> {
        let mut parts = mangled.split('_').peekable();
        let mut path_parts = Vec::new();

        while let Some(part) = parts.next() {
            if part == "pkg" {
                path_parts.push("/".to_string());
            } else {
                let mut part = part.to_string();
                while parts.peek() == Some(&"") {
                    part.push('_');
                    parts.next();
                    if let Some(next) = parts.next() {
                        part.push_str(next);
                    }
                }
                path_parts.push(part);
            }
        }

        if path_parts.len() < 2 {
            return None;
        }

        let item = path_parts.pop()?;
        let path = PathBuf::from_iter(path_parts);

        let resource = Resource::new(path);
        Some((resource, item))
    }
}

/// A mangler that just returns the identifer as-is (no mangling).
/// e.g. `foo/bar/baz.wgsl item => item`
///
/// Warning: will break the program in case of name conflicts.
#[derive(Default, Clone, Debug)]
pub struct NoMangler;

impl Mangler for NoMangler {
    fn mangle(&self, _resource: &Resource, item: &str) -> String {
        item.to_string()
    }
}

/// A mangler that remembers and can unmangle.
pub struct CacheMangler<'a, T: Mangler> {
    cache: RefCell<HashMap<String, (Resource, String)>>,
    mangler: &'a T,
}

impl<'a, T: Mangler> CacheMangler<'a, T> {
    pub fn new(mangler: &'a T) -> Self {
        Self {
            cache: Default::default(),
            mangler,
        }
    }
}

impl<'a, T: Mangler> Mangler for CacheMangler<'a, T> {
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

/// A mangler that uses cryptic unicode symbols that look like :, < and >
/// e.g. `foo/bar/baz.wgsl item => item`
///
/// Warning: will break the program in case of name conflicts.
///
/// # Panics
/// if the TypeExpression is not normalized (i.e. contains only identifiers and literals)
#[derive(Default, Clone, Debug)]
pub struct UnicodeMangler;

impl UnicodeMangler {
    const LT: char = 'ᐸ'; // U+1438
    const GT: char = 'ᐳ'; // U+02CF
    const SEP: &'static str = "::"; // <-- these are NOT colons, they are U+02D0
    const TY_SEP: &'static str = "ˏ"; // U+02CF

    fn display_ty(ty: &TypeExpression) -> impl fmt::Display + '_ {
        format_args!(
            "{}{}",
            ty.ident,
            ty.template_args
                .iter()
                .format_with(Self::TY_SEP, |tplt, f| {
                    f(&format_args!(
                        "{}{}{}",
                        Self::LT,
                        tplt.iter().format_with(Self::TY_SEP, |tplt, f| {
                            match tplt.expression.node() {
                                Expression::Literal(lit) => f(lit),
                                Expression::TypeOrIdentifier(ty) => f(&Self::display_ty(ty)),
                                _ => panic!("only type names can be mangled"),
                            }
                        }),
                        Self::GT
                    ))
                })
        )
        .to_string()
    }
}

impl Mangler for UnicodeMangler {
    fn mangle(&self, resource: &Resource, item: &str) -> String {
        let sep = Self::SEP;
        let path = resource.path();
        let path = path.iter().map(|p| p.to_string_lossy()).format(sep);
        format!("{path}{sep}{item}")
    }
    fn unmangle(&self, mangled: &str) -> Option<(Resource, String)> {
        let mut path = PathBuf::from_iter(mangled.split(Self::SEP));
        let name = path.file_name().unwrap().to_str().unwrap().to_string();
        path.pop();
        Some((Resource::new(path), name))
    }
    fn mangle_types(&self, item: &str, _variant: u32, types: &[TypeExpression]) -> String {
        // these are NOT chevrons and comma!
        format!(
            "{item}{}{}{}",
            Self::LT,
            types
                .iter()
                .format_with(Self::TY_SEP, |ty, f| f(&Self::display_ty(ty))),
            Self::GT
        )
    }
}
