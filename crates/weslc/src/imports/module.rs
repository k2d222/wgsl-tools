use std::{collections::HashMap, rc::Rc};

use wgsl_parse::syntax;

use crate::{Error, Mangler, Resolver, Resource};

pub struct Module<R: Resource> {
    pub(crate) source: syntax::TranslationUnit,
    pub(crate) resource: R,
    pub(crate) imports: Imports<R>,
    pub(crate) resolutions: Modules<R>,
}

impl<R: Resource> Module<R> {
    pub fn new(
        source: syntax::TranslationUnit,
        resource: R,
        imports: Imports<R>,
    ) -> Result<Self, Error> {
        Ok(Self {
            source,
            resource,
            imports,
            resolutions: HashMap::new(),
        })
    }

    fn resolve_import(&mut self, resource: &R, module: Rc<Module<R>>) {
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
#[allow(type_alias_bounds)]
type Imports<R: Resource> = HashMap<R, Vec<syntax::ImportItem>>;
#[allow(type_alias_bounds)]
type Modules<R: Resource> = HashMap<R, Rc<Module<R>>>;

/// Flatten imports to a list of resources and items to import.
fn resolve_import_paths<R: Resolver>(
    imports: &[syntax::Import],
    parent: &R::Resource,
    resolver: &R,
) -> Result<Imports<R::Resource>, Error> {
    let mut res = Imports::<R::Resource>::new();

    for import in imports {
        match &import.content {
            syntax::ImportContent::Star(item) => {
                let mut path = import.path.clone();
                path.push(item.name.clone());
                let resource = resolver.resolve_path(&path, Some(parent))?;
                if let Some(entry) = res.get_mut(&resource) {
                    entry.push(item.clone());
                } else {
                    res.insert(resource, vec![item.clone()]);
                }
            }
            syntax::ImportContent::Item(item) => {
                let resource = resolver.resolve_path(&import.path, Some(parent))?;
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

fn resolve_rec<R: Resource>(
    resource: &R,
    resolver: &impl Resolver<Resource = R>,
    mangler: &(impl Mangler<R> + ?Sized),
    visited: &mut Modules<R>,
) -> Result<Module<R>, Error> {
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

pub fn resolve<R: Resource, M: Mangler<R> + ?Sized>(
    resource: &R,
    resolver: &impl Resolver<Resource = R>,
    mangler: &M,
) -> Result<Module<R>, crate::Error> {
    let mut submodules = Modules::new();
    let module = resolve_rec(resource, resolver, mangler, &mut submodules)?;
    Ok(module)
}
