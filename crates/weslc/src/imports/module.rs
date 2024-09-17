use std::{collections::HashMap, path::Path, rc::Rc};

use wgsl_parse::syntax;

use crate::{Error, Mangler, Resolver, Resource};

pub struct Module {
    pub(crate) source: syntax::TranslationUnit,
    pub(crate) resource: Resource,
    pub(crate) imports: Imports,
    pub(crate) resolutions: Modules,
}

impl Module {
    pub fn new(
        source: syntax::TranslationUnit,
        resource: Resource,
        imports: Imports,
    ) -> Result<Self, Error> {
        Ok(Self {
            source,
            resource,
            imports,
            resolutions: HashMap::new(),
        })
    }

    fn resolve_import(&mut self, resource: &Resource, module: Rc<Module>) {
        self.resolutions.insert(resource.clone(), module);
    }

    #[allow(unused)]
    fn is_resolved(&self) -> bool {
        self.imports
            .keys()
            .all(|path| self.resolutions.contains_key(path))
    }
}

// XXX: are imports supposed to be order-independent?
type Imports = HashMap<Resource, Vec<syntax::ImportItem>>;
#[allow(type_alias_bounds)]
type Modules = HashMap<Resource, Rc<Module>>;

pub fn resolve_path(
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
fn resolve_import_paths<R: Resolver>(
    imports: &[syntax::Import],
    parent: &Resource,
    resolver: &R,
) -> Result<Imports, Error> {
    let mut res = Imports::new();

    for import in imports {
        match &import.content {
            syntax::ImportContent::Star(item) => {
                let mut path = import.path.clone();
                path.push(item.name.clone());
                let resource = resolve_path(&path, Some(parent))?;
                if let Some(entry) = res.get_mut(&resource) {
                    entry.push(item.clone());
                } else {
                    res.insert(resource, vec![item.clone()]);
                }
            }
            syntax::ImportContent::Item(item) => {
                let resource = resolve_path(&import.path, Some(parent))?;
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

                let resolved = resolve_import_paths(&imports, parent, resolver)?;
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

fn resolve_rec(
    resource: &Resource,
    resolver: &impl Resolver,
    mangler: &(impl Mangler + ?Sized),
    visited: &mut Modules,
) -> Result<Module, Error> {
    let source = resolver.resolve_file(resource)?;
    let imports = resolve_import_paths(&source.imports, resource, resolver)?;
    let mut module = Module::new(source, resource.clone(), imports.clone())?;

    for child in imports.keys() {
        if let Some(submodule) = visited.get(child) {
            module.resolve_import(child, submodule.clone());
        } else {
            let submodule = resolve_rec(child, resolver, mangler, visited)?;
            let submodule = Rc::new(submodule);
            module.resolve_import(child, submodule.clone());
            visited.insert(child.clone(), submodule);
        }
    }

    // mangle only after imports have been resolved.
    assert!(module.is_resolved());
    module.mangle(mangler)?;

    Ok(module)
}

pub fn resolve<M: Mangler + ?Sized>(
    resource: &Resource,
    resolver: &impl Resolver,
    mangler: &M,
) -> Result<Module, crate::Error> {
    let mut submodules = Modules::new();
    let module = resolve_rec(resource, resolver, mangler, &mut submodules)?;
    Ok(module)
}
