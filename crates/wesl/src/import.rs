use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    ops::DerefMut,
    path::{Component, Path, PathBuf},
    rc::Rc,
};

use wgsl_parse::syntax::{self, Ident, TranslationUnit, TypeExpression};

use crate::{visit::Visit, Mangler, ResolveError, Resolver, Resource};

type Imports = HashMap<Ident, Resource>;
type Idents = HashMap<Resource, HashSet<Ident>>;
type Modules = HashMap<Resource, Rc<RefCell<Module>>>;

#[derive(Clone, Debug, thiserror::Error)]
pub enum ImportError {
    #[error("duplicate imported item `{0}`")]
    DuplicateSymbol(String),
    #[error("{0}")]
    ResolveError(#[from] ResolveError),
    #[error("module `{0}` has no declaration `{1}`")]
    MissingDecl(Resource, String),
    #[error("circular dependency involving `{0}`")]
    CircularDependency(Resource),
}

type E = ImportError;

struct Module {
    source: TranslationUnit,
    resource: Resource,
    idents: HashSet<Ident>,
    treated_idents: HashSet<Ident>, // used idents that have already been usage-analyzed
    imports: Imports,
}

pub(crate) struct Resolutions(Modules, Resource);

impl Resolutions {
    pub(crate) fn root_resource(&self) -> &Resource {
        &self.1
    }
}

fn resolve_inline_resource(path: &Path, parent_resource: &Resource, imports: &Imports) -> Resource {
    let prefix = path.iter().next().unwrap().to_str().unwrap();
    let suffix = PathBuf::from_iter(path.iter().skip(1));

    let parent_res = imports
        .iter()
        .find_map(|(ident, res)| (ident.name().as_str() == prefix).then_some(res))
        .or(Some(parent_resource));

    canonical_resource(&suffix, parent_res)
}

impl Module {
    fn new(source: TranslationUnit, resource: Resource) -> Self {
        let idents = source
            .global_declarations
            .iter()
            .filter_map(|decl| decl.ident())
            .cloned()
            .collect();
        let imports = imported_resources(&source.imports, &resource);
        Self {
            source,
            resource,
            idents,
            treated_idents: Default::default(),
            imports,
        }
    }
}

// XXX: it's quite messy.
// TODO: current algorithm doesn't allow cyclic module dependencies (even if there is no cycles in imported items)
pub fn resolve(
    root: TranslationUnit,
    resource: &Resource,
    keep: HashSet<Ident>,
    resolver: &impl Resolver,
) -> Result<Resolutions, E> {
    fn load_module(
        resource: &Resource,
        resolutions: &mut Modules,
        resolver: &impl Resolver,
    ) -> Result<Rc<RefCell<Module>>, E> {
        if let Some(module) = resolutions.get(resource) {
            Ok(module.clone())
        } else {
            let source = resolver.resolve_module(resource)?;
            let module = Module::new(source, resource.clone());
            // let const_asserts =
            //     module
            //         .source
            //         .global_declarations
            //         .iter()
            //         .filter_map(|decl| match decl {
            //             syntax::GlobalDeclaration::ConstAssert(const_assert) => Some(const_assert),
            //             _ => None,
            //         }).flat_map(|ca| Visit::<TypeExpression>::visit(ca))
            let module = Rc::new(RefCell::new(module));
            resolutions.insert(resource.clone(), module.clone());
            Ok(module)
        }
    }

    fn resolve_ident(
        module: &mut Module,
        ident: &Ident,
        local_idents: &mut HashSet<Ident>,
        extern_idents: &mut Idents,
        resolver: &impl Resolver,
        resolutions: &mut Modules,
    ) -> Result<(), E> {
        if !module.treated_idents.insert(ident.clone()) {
            // was already treated
            return Ok(());
        }

        let decl = module
            .source
            .global_declarations
            .iter_mut()
            .find(|decl| decl.ident().is_some_and(|id| id == ident))
            .ok_or_else(|| E::MissingDecl(module.resource.clone(), ident.name().to_string()))?;

        for ty in Visit::<TypeExpression>::visit_mut(decl) {
            if module.treated_idents.contains(&ty.ident) {
                continue;
            }

            // get the the resource associated with the type, if it points at a decl in another module.
            let ext_resource = if let Some(path) = &ty.path {
                resolve_inline_resource(path, &module.resource, &module.imports)
            } else if let Some(resource) = module.imports.get(&ty.ident) {
                resource.clone()
            } else {
                if module.idents.contains(&ty.ident) {
                    local_idents.insert(ty.ident.clone());
                }
                continue;
            };

            // if the import path point at the current module
            if ext_resource == module.resource {
                if module.idents.contains(&ty.ident) {
                    local_idents.insert(ty.ident.clone());
                    continue;
                } else {
                    return Err(E::MissingDecl(ext_resource, ty.ident.name().to_string()));
                }
            }

            // get or load the external module
            let ext_mod = load_module(&ext_resource, resolutions, resolver)?;
            let mut ext_mod = ext_mod
                .try_borrow_mut()
                .map_err(|_| E::CircularDependency(module.resource.clone()))?;
            let ext_mod = ext_mod.deref_mut();

            // get the ident of the external declaration pointed to by the type
            let ext_id = ext_mod
                .idents
                .iter()
                .find(|id| id.name().as_str() == ty.ident.name().as_str())
                .cloned();

            if let Some(ext_id) = ext_id {
                if !ext_mod.treated_idents.contains(&ext_id) {
                    extern_idents
                        .entry(ext_resource)
                        .or_insert(Default::default())
                        .insert(ext_id.clone());
                }

                ty.path = None;
                ty.ident = ext_id;
            } else {
                return Err(E::MissingDecl(ext_resource, ty.ident.name().to_string()));
            }
        }

        Ok(())
    }

    fn resolve_idents(
        resource: &Resource,
        idents: &mut HashSet<Ident>,
        extern_idents: &mut Idents,
        resolver: &impl Resolver,
        resolutions: &mut Modules,
    ) -> Result<(), E> {
        let module = load_module(&resource, resolutions, resolver)?;
        let mut module = module
            .try_borrow_mut()
            .map_err(|_| E::CircularDependency(resource.clone()))?;
        let module = module.deref_mut();

        let mut next_idents = HashSet::new();

        while !idents.is_empty() {
            for ident in idents.iter() {
                resolve_ident(
                    module,
                    ident,
                    &mut next_idents,
                    extern_idents,
                    resolver,
                    resolutions,
                )?;
            }
            std::mem::swap(idents, &mut next_idents);
            next_idents.clear();
        }

        Ok(())
    }

    let mut resolutions = Modules::new();
    // let module = load_module(&resource, &mut resolutions, resolver)?;
    let module = Module::new(root, resource.clone());
    let module = Rc::new(RefCell::new(module));
    resolutions.insert(resource.clone(), module.clone());

    let mut idents = Idents::new();
    idents.insert(resource.clone(), keep);
    let mut next_idents = Idents::new();

    while !idents.is_empty() {
        for (resource, idents) in &mut idents {
            resolve_idents(
                resource,
                idents,
                &mut next_idents,
                resolver,
                &mut resolutions,
            )?;
        }
        std::mem::swap(&mut idents, &mut next_idents);
        next_idents.clear();
    }

    Ok(Resolutions(resolutions, resource.clone()))
}

fn clean_path(path: impl AsRef<Path>) -> PathBuf {
    let mut res = PathBuf::new();
    for comp in path.as_ref().components() {
        match comp {
            Component::Prefix(_) => {}
            Component::RootDir => {
                res.push(comp);
            }
            Component::CurDir => {}
            Component::ParentDir => {
                res.pop();
            }
            Component::Normal(_) => res.push(comp),
        }
    }
    res
}

pub(crate) fn canonical_resource(
    import_path: &Path,
    parent_resource: Option<&Resource>,
) -> Resource {
    let path = if import_path.starts_with(".") {
        if let Some(parent) = parent_resource {
            // SAFETY: parent_path must be a file, therefore must have a containing directory
            let mut path = parent.path().parent().unwrap().to_path_buf();
            path.extend(import_path);
            clean_path(path)
        } else {
            clean_path(import_path)
        }
    } else {
        clean_path(import_path)
    };

    Resource::new(path)
}

/// Flatten imports to a list of resources to import.
pub(crate) fn imported_resources(imports: &[syntax::Import], parent_res: &Resource) -> Imports {
    let mut res = Imports::new();

    for import in imports {
        match &import.content {
            syntax::ImportContent::Item(item) => {
                let resource = canonical_resource(&import.path, Some(parent_res));
                let ident = item.rename.as_ref().unwrap_or(&item.ident).clone();
                res.insert(ident, resource);
            }
            syntax::ImportContent::Collection(imports) => {
                // prepend the parent import path to the children in the collection
                let imports = imports
                    .clone()
                    .into_iter()
                    .map(|mut child| {
                        let mut path = import.path.clone();
                        path.extend(child.path.iter());
                        child.path = path;
                        child
                    })
                    .collect::<Vec<_>>();

                res.extend(imported_resources(&imports, parent_res));
            }
        }
    }

    res
}

fn mangle_decls<'a>(
    wgsl: &'a mut TranslationUnit,
    resource: &'a Resource,
    mangler: &'a impl Mangler,
) {
    wgsl.global_declarations
        .iter_mut()
        .filter_map(|decl| decl.ident_mut())
        .for_each(|ident| {
            let new_name = mangler.mangle(resource, ident.name().as_str());
            ident.rename(new_name.clone());
        })
}

impl Resolutions {
    pub fn mangle(&mut self, mangler: &impl Mangler) -> Result<(), E> {
        let root_resource = self.root_resource().clone();
        for (resource, module) in self.0.iter_mut() {
            if resource != &root_resource {
                let mut module = module.borrow_mut();
                mangle_decls(&mut module.source, resource, mangler);
            }
        }
        Ok(())
    }

    pub fn assemble(&self, strip: bool) -> TranslationUnit {
        let mut wesl = TranslationUnit::default();
        for module in self.0.values() {
            let module = module.borrow();
            let source = module.source.clone();
            if strip {
                wesl.global_declarations
                    .extend(source.global_declarations.into_iter().filter(|decl| {
                        decl.is_const_assert()
                            || decl
                                .ident()
                                .is_some_and(|id| module.treated_idents.contains(id))
                    }));
            } else {
                wesl.global_declarations.extend(source.global_declarations);
            }
        }
        wesl
    }
}
