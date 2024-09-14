use std::collections::HashMap;
use std::collections::HashSet;
use std::hash::DefaultHasher;
use std::hash::Hash;
use std::hash::Hasher;
use std::marker::PhantomData;

use itertools::Itertools;
use wgsl_parse::syntax::*;
use wgsl_parse_macros::query_mut;

use super::resolve::FileResource;
use super::resolve::Resource;
use super::resolve::{ImportError, Module};

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

/// keep track of declarations in a scope.
type Scope = HashSet<String>;

fn iter_replaceable_names(module: &mut TranslationUnit) -> impl Iterator<Item = &mut String> {
    fn expr_names(expr: &mut Expression) -> impl Iterator<Item = &mut String> {
        query_mut!(expr.{
            Expression::Parenthesized.expression.(x => expr_names(&mut **x)),
            Expression::NamedComponent.base.(x => expr_names(&mut **x)),
            Expression::Indexing.{ base, index }.(x => expr_names(&mut **x)),
            Expression::Unary.operand.(x => expr_names(&mut **x)),
            Expression::Binary.{ left, right }.(x =>  expr_names(&mut **x)),
            Expression::FunctionCall.{
                name,
                template_args.[].[].(expr_names),
                arguments.[].(expr_names)
            },
            Expression::Identifier.name,
            Expression::Type.{ name, template_args.[].[].(expr_names) }
        })
    }

    fn type_names(ty: &mut TypeExpression) -> impl Iterator<Item = &mut String> {
        query_mut!(ty.{
            name,
            template_args.[].[].(expr_names),
        })
    }

    // this one keeps track of scope, because local declarations introduce a new scope and may
    // shadow names declared at the global level.
    // we just ignore names that refer to local variables.
    fn body_names_scoped(statements: &mut Vec<Statement>) -> impl Iterator<Item = &mut String> {
        fn rec<'a>(
            statements: impl IntoIterator<Item = &'a mut Statement>,
        ) -> (Vec<&'a mut String>, Scope) {
            let mut names = Vec::new();
            let mut scope = Scope::new();
            for stat in statements {
                match stat {
                    Statement::Compound(stat) => {
                        let it = rec(&mut stat.statements).0.into_iter();
                        names.extend(it.filter(|name| !scope.contains(*name)));
                    }
                    Statement::Assignment(stat) => {
                        let it = query_mut!(stat.{ lhs, rhs }.(expr_names));
                        names.extend(it.filter(|name| !scope.contains(*name)));
                    }
                    Statement::Increment(stat) => {
                        let it = query_mut!(stat.expression.(expr_names));
                        names.extend(it.filter(|name| !scope.contains(*name)));
                    }
                    Statement::Decrement(stat) => {
                        let it = query_mut!(stat.expression.(expr_names));
                        names.extend(it.filter(|name| !scope.contains(*name)));
                    }
                    Statement::If(stat) => {
                        let it = query_mut!(stat.{
                            attributes.[].arguments.[].[].(expr_names),
                            if_clause.{
                                expression.(expr_names),
                                body.{
                                    attributes.[].arguments.[].[].(expr_names),
                                    statements.(x => rec(x).0)
                                }
                            },
                            else_if_clauses.[].{
                                expression.(expr_names),
                                body.{
                                    attributes.[].arguments.[].[].(expr_names),
                                    statements.(x => rec(x).0)
                                }
                            },
                            else_clause.[].body.{
                                attributes.[].arguments.[].[].(expr_names),
                                statements.(x => rec(x).0)
                            },
                        });
                        names.extend(it.filter(|name| !scope.contains(*name)));
                    }
                    Statement::Switch(stat) => {
                        let it = query_mut!(stat.{
                            attributes.[].arguments.[].[].(expr_names),
                            expression.(expr_names),
                            body_attributes.[].arguments.[].[].(expr_names),
                            clauses.[].{
                                case_selectors.[].CaseSelector::Expression.(expr_names),
                                body.{
                                    attributes.[].arguments.[].[].(expr_names),
                                    statements.(x => rec(x).0)
                                }
                            },
                        });
                        names.extend(it.filter(|name| !scope.contains(*name)));
                    }
                    Statement::Loop(stat) => {
                        let it = query_mut!(stat.attributes.[].arguments.[].[].(expr_names));
                        names.extend(it.filter(|name| !scope.contains(*name)));

                        let it = query_mut!(stat.body.attributes.[].arguments.[].[].(expr_names));
                        names.extend(it.filter(|name| !scope.contains(*name)));

                        // these ones have to be handled separatly, because the continuing statement
                        // is separated from the rest of the statements (same for the break-if)
                        let it = rec(&mut stat.body.statements).0;
                        names.extend(it.into_iter().filter(|name| !scope.contains(*name)));

                        if let Some(stat) = &mut stat.continuing {
                            let it =
                                query_mut!(stat.body.attributes.[].arguments.[].[].(expr_names));
                            names.extend(it.filter(|name| !scope.contains(*name)));

                            let (it, cont_scope) = rec(&mut stat.body.statements);
                            names.extend(it.into_iter().filter(|name| !scope.contains(*name)));

                            if let Some(stat) = &mut stat.break_if {
                                let it = expr_names(&mut stat.expression);
                                names.extend(it.filter(|name| !cont_scope.contains(*name)));
                            }
                        }
                    }
                    Statement::For(stat) => {
                        let it = query_mut!(stat.attributes.[].arguments.[].[].(expr_names));
                        names.extend(it.filter(|name| !scope.contains(*name)));

                        // these ones have to be handled separatly, because the for initializer
                        // statement is the parent scope of the body
                        let body_scope = if let Some(init) = &mut stat.initializer {
                            let (it, scope) = rec(std::iter::once(&mut **init));
                            names.extend(it.into_iter().filter(|name| !scope.contains(*name)));
                            scope
                        } else {
                            Scope::new()
                        };
                        let it = query_mut!(stat.condition.[].(expr_names));
                        names.extend(it.filter(|name| !body_scope.contains(*name)));

                        if let Some(update) = &mut stat.update {
                            let it = rec(std::iter::once(&mut **update)).0.into_iter();
                            names.extend(it.filter(|name| !body_scope.contains(*name)));
                        }

                        let it = query_mut!(stat.body.{
                            attributes.[].arguments.[].[].(expr_names),
                            statements.(x => rec(x).0)
                        });
                        names.extend(it.filter(|name| !body_scope.contains(*name)));
                    }
                    Statement::While(stat) => {
                        let it = query_mut!(stat.{
                            attributes.[].arguments.[].[].(expr_names),
                            condition.(expr_names),
                            body.{
                                attributes.[].arguments.[].[].(expr_names),
                                statements.(x => rec(x).0)
                            }
                        });
                        names.extend(it.filter(|name| !scope.contains(*name)));
                    }
                    Statement::Break(_) => (),
                    Statement::Continue(_) => (),
                    Statement::Return(stat) => {
                        let it = query_mut!(stat.expression.[].(expr_names));
                        names.extend(it.filter(|name| !scope.contains(*name)));
                    }
                    Statement::Discard(_) => (),
                    Statement::FunctionCall(stat) => {
                        let it = query_mut!(stat.call.{
                            name,
                            template_args.[].[].(expr_names),
                            arguments.[].(expr_names),
                        });
                        names.extend(it.filter(|name| !scope.contains(*name)));
                    }
                    Statement::ConstAssert(stat) => {
                        let it = query_mut!(stat.expression.(expr_names));
                        names.extend(it.filter(|name| !scope.contains(*name)));
                    }
                    Statement::Declaration(stat) => {
                        scope.insert(stat.name.clone());
                        let it = query_mut!(stat.{
                            attributes.[].arguments.[].[].(expr_names),
                            template_args.[].[].(expr_names),
                            ty.[].(type_names),
                            initializer.[].(expr_names),
                        });
                        names.extend(it.filter(|name| !scope.contains(*name)));
                    }
                    _ => (),
                }
            }
            (names, scope)
        }
        rec(statements).0.into_iter()
    }

    let replaceable_names = query_mut!(module.global_declarations.[].{
        GlobalDeclaration::Declaration.{
            attributes.[].arguments.[].[].(expr_names),
            template_args.[].[].(expr_names),
            ty.[].(type_names),
            initializer.[].(expr_names),
        },
        GlobalDeclaration::TypeAlias.ty.(type_names),
        GlobalDeclaration::Struct.members.[].{
            attributes.[].arguments.[].[].(expr_names),
            ty.(type_names),
        },
        GlobalDeclaration::Function.{
            attributes.[].arguments.[].[].(expr_names),
            parameters.[].{
                attributes.[].arguments.[].[].(expr_names),
                ty.(type_names),
            },
            return_attributes.[].arguments.[].[].(expr_names),
            return_type.[].(type_names),
            body.{
                attributes.[].arguments.[].[].(expr_names),
                statements.(body_names_scoped)
            }
        },
        GlobalDeclaration::ConstAssert.expression.(expr_names),
    });

    replaceable_names
}

impl<R: Resource> Module<R> {
    pub fn mangle(&mut self, mangler: &(impl Mangler<R> + ?Sized)) -> Result<(), ImportError> {
        // delared idents
        let mut replace: HashMap<String, String> = query_mut!(self.source.global_declarations.[].{
            GlobalDeclaration::Declaration.name,
            GlobalDeclaration::TypeAlias.name,
            GlobalDeclaration::Struct.name,
            GlobalDeclaration::Function.name,
        })
        .map(|decl_ident| {
            let old_ident = decl_ident.to_string();
            let new_ident = mangler.mangle(&self.resource, &old_ident);
            *decl_ident = new_ident.clone();
            (old_ident, new_ident)
        })
        .collect();

        // imported idents
        for (resource, items) in &self.imports {
            for item in items {
                let old_ident = item.rename.as_ref().unwrap_or(&item.name).clone();
                let new_ident = mangler.mangle(&resource, &item.name);
                replace.insert(old_ident, new_ident);
            }
        }

        for name in iter_replaceable_names(&mut self.source) {
            if let Some(new_ident) = replace.get(name) {
                *name = new_ident.clone();
            }
        }

        Ok(())
    }
}
