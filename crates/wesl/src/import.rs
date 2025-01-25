use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    ops::DerefMut,
    path::{Path, PathBuf},
    rc::Rc,
};

use itertools::Itertools;
use wgsl_parse::syntax::{self, Ident, TranslationUnit, TypeExpression};

use crate::{visit::Visit, Mangler, ResolveError, Resolver, Resource};

type Imports = HashMap<Ident, Resource>;
type Decls = HashMap<Resource, HashSet<usize>>;
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
    idents: HashMap<Ident, usize>,  // lookup (ident, decl_index)
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
    // let suffix = PathBuf::from_iter(path.iter().skip(1));

    let resource = imports
        .iter()
        .find_map(|(ident, res)| {
            if &*ident.name() == prefix {
                Some(res.join(PathBuf::from_iter(path.iter().skip(1))))
            } else {
                None
            }
        })
        .unwrap_or_else(|| Resource::new(path));

    canonical_resource(resource.path(), Some(parent_resource))
}

impl Module {
    fn new(source: TranslationUnit, resource: Resource) -> Self {
        let idents = source
            .global_declarations
            .iter()
            .enumerate()
            .filter_map(|(i, decl)| decl.ident().map(|id| (id.clone(), i)))
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
pub fn resolve(
    root: TranslationUnit,
    resource: &Resource,
    keep: HashSet<Ident>,
    resolver: &impl Resolver,
) -> Result<Resolutions, E> {
    fn load_module(
        resource: &Resource,
        local_decls: &mut HashSet<usize>,
        resolutions: &mut Modules,
        resolver: &impl Resolver,
    ) -> Result<Rc<RefCell<Module>>, E> {
        if let Some(module) = resolutions.get(resource) {
            Ok(module.clone())
        } else {
            let source = resolver.resolve_module(resource)?;
            let module = Module::new(source, resource.clone());

            // const_asserts of used modules must be included.
            // https://github.com/wgsl-tooling-wg/wesl-spec/issues/66
            let const_asserts = module
                .source
                .global_declarations
                .iter()
                .enumerate()
                .filter_map(|(i, decl)| decl.is_const_assert().then_some(i));
            local_decls.extend(const_asserts);

            let module = Rc::new(RefCell::new(module));
            resolutions.insert(resource.clone(), module.clone());

            Ok(module)
        }
    }

    fn resolve_decl(
        module: &mut Module,
        decl: usize,
        local_decls: &mut HashSet<usize>,
        extern_decls: &mut Decls,
        resolutions: &mut Modules,
        resolver: &impl Resolver,
    ) -> Result<(), E> {
        let decl = module.source.global_declarations.get_mut(decl).unwrap();

        if let Some(id) = decl.ident() {
            if !module.treated_idents.insert(id.clone()) {
                return Ok(());
            }
        }

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
                if let Some(decl) = module.idents.get(&ty.ident) {
                    local_decls.insert(*decl);
                }
                continue;
            };

            // if the import path point at the current module, it must be a local decl
            if ext_resource == module.resource {
                if let Some(decl) = module.idents.get(&ty.ident) {
                    local_decls.insert(*decl);
                    continue;
                } else {
                    return Err(E::MissingDecl(ext_resource, ty.ident.name().to_string()));
                }
            }

            // get or load the external module
            let ext_mod = load_module(&ext_resource, &mut HashSet::new(), resolutions, resolver)?;
            let mut ext_mod = ext_mod
                .try_borrow_mut()
                .map_err(|_| E::CircularDependency(module.resource.clone()))?;
            let ext_mod = ext_mod.deref_mut();

            // get the ident of the external declaration pointed to by the type
            let ext_id = ext_mod
                .idents
                .keys()
                .find(|id| *id.name() == *ty.ident.name())
                .cloned();

            if let Some(ext_id) = ext_id {
                if !ext_mod.treated_idents.contains(&ext_id) {
                    let decl = *ext_mod
                        .idents
                        .get(&ext_id)
                        .ok_or_else(|| E::MissingDecl(ext_resource.clone(), ext_id.to_string()))?;
                    extern_decls
                        .entry(ext_resource)
                        .or_insert(Default::default())
                        .insert(decl);
                }

                ty.path = None;
                ty.ident = ext_id;
            } else {
                return Err(E::MissingDecl(ext_resource, ty.ident.to_string()));
            }
        }

        Ok(())
    }

    fn resolve_decls(
        resource: &Resource,
        local_decls: &mut HashSet<usize>,
        extern_decls: &mut Decls,
        resolver: &impl Resolver,
        resolutions: &mut Modules,
    ) -> Result<(), E> {
        let module = load_module(&resource, &mut HashSet::new(), resolutions, resolver)?;
        let mut module = module
            .try_borrow_mut()
            .map_err(|_| E::CircularDependency(resource.clone()))?;
        let module = module.deref_mut();

        let mut next_decls = HashSet::new();

        while !local_decls.is_empty() {
            for decl in local_decls.iter() {
                resolve_decl(
                    module,
                    *decl,
                    &mut next_decls,
                    extern_decls,
                    resolutions,
                    resolver,
                )?;
            }

            std::mem::swap(local_decls, &mut next_decls);
            next_decls.clear();
        }

        Ok(())
    }

    let mut resolutions = Modules::new();
    let module = Module::new(root, resource.clone());

    let mut keep_decls: HashSet<usize> = keep
        .iter()
        .map(|id| {
            module
                .idents
                .get(id)
                .copied()
                .ok_or_else(|| E::MissingDecl(resource.clone(), id.to_string()))
        })
        .try_collect()?;

    // const_asserts of used modules must be included.
    // https://github.com/wgsl-tooling-wg/wesl-spec/issues/66
    let const_asserts = module
        .source
        .global_declarations
        .iter()
        .enumerate()
        .filter_map(|(i, decl)| decl.is_const_assert().then_some(i));
    keep_decls.extend(const_asserts);

    let mut decls = Decls::new();
    let mut next_decls = Decls::new();
    decls.insert(resource.clone(), keep_decls);

    let module = Rc::new(RefCell::new(module));
    resolutions.insert(resource.clone(), module.clone());

    while !decls.is_empty() {
        for (resource, decls) in &mut decls {
            resolve_decls(resource, decls, &mut next_decls, resolver, &mut resolutions)?;
        }
        std::mem::swap(&mut decls, &mut next_decls);
        next_decls.clear();
    }

    Ok(Resolutions(resolutions, resource.clone()))
}

pub(crate) fn canonical_resource(
    import_path: &Path,
    parent_resource: Option<&Resource>,
) -> Resource {
    if import_path.starts_with(".") || import_path.starts_with("..") {
        if let Some(parent) = parent_resource {
            parent.join(import_path)
        } else {
            Resource::new(import_path)
        }
    } else {
        Resource::new(import_path)
    }
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
            let new_name = mangler.mangle(resource, &*ident.name());
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
