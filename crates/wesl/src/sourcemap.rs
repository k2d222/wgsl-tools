use std::{cell::RefCell, collections::HashMap};

use crate::{Error, Mangler, Resolver, Resource};

pub trait SourceMap {
    fn get_decl(&self, decl: &str) -> Option<(&Resource, &str)>;
    fn get_source(&self, resource: &Resource) -> Option<&str>;
    fn get_default_source(&self) -> Option<&str> {
        None
    }
}

#[derive(Clone, Debug, Default)]
pub struct BasicSourceMap {
    mappings: HashMap<String, (Resource, String)>,
    sources: HashMap<Resource, String>,
    default_source: Option<String>,
}

impl BasicSourceMap {
    pub fn new() -> Self {
        Default::default()
    }
    pub fn add_decl(&mut self, decl: String, resource: Resource, item: String) {
        self.mappings.insert(decl, (resource, item));
    }
    pub fn add_source(&mut self, file: Resource, source: String) {
        self.sources.insert(file, source);
    }
    pub fn set_default_source(&mut self, source: String) {
        self.default_source = Some(source);
    }
}

/// generate sourcemaps by keeping track of name mangling and file resolutions
pub struct SourceMapper<'a> {
    pub resolver: Box<dyn Resolver + 'a>,
    pub mangler: Box<dyn Mangler + 'a>,
    pub sourcemap: RefCell<BasicSourceMap>,
}

impl<'a> SourceMapper<'a> {
    pub fn new(resolver: Box<dyn Resolver + 'a>, mangler: Box<dyn Mangler + 'a>) -> Self {
        Self {
            resolver,
            mangler,
            sourcemap: Default::default(),
        }
    }
    pub fn finish(self) -> BasicSourceMap {
        self.sourcemap.into_inner()
    }
}

impl SourceMap for BasicSourceMap {
    fn get_decl(&self, decl: &str) -> Option<(&Resource, &str)> {
        let (resource, decl) = self.mappings.get(decl)?;
        Some((resource, decl))
    }

    fn get_source(&self, resource: &Resource) -> Option<&str> {
        self.sources.get(resource).map(|source| source.as_str())
    }
    fn get_default_source(&self) -> Option<&str> {
        self.default_source.as_ref().map(|source| source.as_str())
    }
}

impl<'a> Mangler for SourceMapper<'a> {
    fn mangle(&self, resource: &Resource, item: &str) -> String {
        let res = self.mangler.mangle(resource, item);
        let mut sourcemap = self.sourcemap.borrow_mut();
        sourcemap.add_decl(res.clone(), resource.clone(), item.to_string());
        res
    }
}

impl<'a> Resolver for SourceMapper<'a> {
    fn resolve_file<'b>(&'b self, resource: &Resource) -> Result<std::borrow::Cow<'b, str>, Error> {
        let res = self.resolver.resolve_file(resource)?;
        let mut sourcemap = self.sourcemap.borrow_mut();
        sourcemap.add_source(resource.clone(), res.clone().into());
        Ok(res)
    }
}
