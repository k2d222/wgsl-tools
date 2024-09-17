use std::{collections::HashMap, path::Path};

use wgsl_parse::syntax::{self, TranslationUnit};

use crate::{Error, Mangler, Resolver, Resource};

// XXX: are imports supposed to be order-independent?
type Imports = HashMap<Resource, Vec<syntax::ImportItem>>;
#[allow(type_alias_bounds)]
type Modules = HashMap<Resource, syntax::TranslationUnit>;

pub struct Module {
    pub(crate) source: syntax::TranslationUnit,
    pub(crate) resource: Resource,
    pub(crate) resolutions: Modules,
}

impl Module {
    pub fn new(source: syntax::TranslationUnit, resource: Resource) -> Self {
        Self {
            source,
            resource,
            resolutions: HashMap::new(),
        }
    }

    pub fn resolve_import(&mut self, resource: &Resource, module: syntax::TranslationUnit) {
        self.resolutions.insert(resource.clone(), module);
    }

    pub fn is_resolved(&self) -> bool {
        let Ok(imports) = imports_to_resources(&self.source.imports, &self.resource) else {
            return false;
        };
        imports
            .keys()
            .all(|path| self.resolutions.contains_key(path))
    }

    pub fn resolve(&mut self, resolver: &impl Resolver) -> Result<(), Error> {
        fn rec(
            module: &mut Module,
            resolver: &impl Resolver,
            parent_res: Resource,
            imports: &Imports,
        ) -> Result<(), Error> {
            for child_res in imports.keys() {
                if !module.resolutions.contains_key(child_res) {
                    let source = resolver.resolve_file(&child_res)?;
                    let imports = imports_to_resources(&source.imports, &parent_res)?;
                    module.resolve_import(child_res, source);
                    rec(module, resolver, child_res.clone(), &imports)?;
                }
            }
            Ok(())
        }

        let imports = imports_to_resources(&self.source.imports, &self.resource)?;
        rec(self, resolver, self.resource.clone(), &imports)?;
        debug_assert!(self.is_resolved());
        Ok(())
    }
}

pub(crate) fn import_to_resource(
    import_path: &Path,
    parent_resource: Option<&Resource>,
) -> Result<Resource, Error> {
    let resource = if !import_path.starts_with(Path::new(".")) {
        import_path.to_path_buf()
    } else if let Some(parent) = parent_resource {
        // SAFETY: parent_path must be a file, therefore must have a contaning directory
        let mut path = parent.path().parent().unwrap().to_path_buf();
        path.extend(import_path);
        path
    } else {
        import_path.to_path_buf()
    };

    Ok(resource.into())
}

/// Flatten imports to a list of resources and items to import.
pub(crate) fn imports_to_resources(
    imports: &[syntax::Import],
    resource: &Resource,
) -> Result<Imports, Error> {
    let mut res = Imports::new();

    for import in imports {
        match &import.content {
            syntax::ImportContent::Star(item) => {
                let mut path = import.path.clone();
                path.push(item.name.clone());
                let resource = import_to_resource(&path, Some(resource))?;
                if let Some(entry) = res.get_mut(&resource) {
                    entry.push(item.clone());
                } else {
                    res.insert(resource, vec![item.clone()]);
                }
            }
            syntax::ImportContent::Item(item) => {
                let resource = import_to_resource(&import.path, Some(resource))?;
                if let Some(entry) = res.get_mut(&resource) {
                    entry.push(item.clone());
                } else {
                    res.insert(resource, vec![item.clone()]);
                }
            }
            syntax::ImportContent::Collection(imports) => {
                // prepend the parent import path to the children in the collection
                let imports = imports
                    .clone()
                    .into_iter()
                    .map(|mut child| {
                        let mut path = import.path.clone();
                        path.extend(child.path.into_iter());
                        child.path = path;
                        child
                    })
                    .collect::<Vec<_>>();

                let resolved = imports_to_resources(&imports, resource)?;
                for (resource, items) in resolved {
                    if let Some(entry) = res.get_mut(&resource) {
                        entry.extend_from_slice(items.as_slice());
                    } else {
                        res.insert(resource, items);
                    }
                }
            }
        }
    }

    Ok(res)
}

pub fn resolve<M: Mangler + ?Sized>(
    source: TranslationUnit,
    resource: Resource,
    resolver: &impl Resolver,
) -> Result<Module, crate::Error> {
    let mut module = Module::new(source, resource);
    module.resolve(resolver)?;
    Ok(module)
}
