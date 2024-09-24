use std::collections::HashSet;

use itertools::Itertools;
use wgsl_parse::syntax::*;
use wgsl_parse_macros::query_mut;

/// keep track of declarations in a scope.
type Scope = HashSet<String>;

pub trait IterUses {
    fn uses_mut(&mut self) -> impl Iterator<Item = &mut String>;
}

impl IterUses for TranslationUnit {
    fn uses_mut(&mut self) -> impl Iterator<Item = &mut String> {
        query_mut!(self.global_declarations.[].(IterUses::uses_mut))
    }
}

impl IterUses for GlobalDeclaration {
    fn uses_mut(&mut self) -> impl Iterator<Item = &mut String> {
        query_mut!(self.{
            GlobalDeclaration::Declaration.{
                attributes.[].arguments.[].[].(IterUses::uses_mut),
                template_args.[].[].(IterUses::uses_mut),
                ty.[].(IterUses::uses_mut),
                initializer.[].(IterUses::uses_mut),
            },
            GlobalDeclaration::TypeAlias.ty.(IterUses::uses_mut),
            GlobalDeclaration::Struct.members.[].{
                attributes.[].arguments.[].[].(IterUses::uses_mut),
                ty.(IterUses::uses_mut),
            },
            GlobalDeclaration::Function.{
                attributes.[].arguments.[].[].(IterUses::uses_mut),
                parameters.[].{
                    attributes.[].arguments.[].[].(IterUses::uses_mut),
                    ty.(IterUses::uses_mut),
                },
                return_attributes.[].arguments.[].[].(IterUses::uses_mut),
                return_type.[].(IterUses::uses_mut),
                body.{
                    attributes.[].arguments.[].[].(IterUses::uses_mut),
                    statements.(IterUses::uses_mut)
                }
            },
            GlobalDeclaration::ConstAssert.expression.(IterUses::uses_mut),
        })
    }
}

impl IterUses for Expression {
    fn uses_mut(&mut self) -> impl Iterator<Item = &mut String> {
        query_mut!(self.{
            Expression::Parenthesized.expression.(x => x.uses_mut()),
            Expression::NamedComponent.base.(x => x.uses_mut()),
            Expression::Indexing.{ base, index }.(x => x.uses_mut()),
            Expression::Unary.operand.(x => x.uses_mut()),
            Expression::Binary.{ left, right }.(x => x.uses_mut()),
            Expression::FunctionCall.{
                name,
                template_args.[].[].(IterUses::uses_mut),
                arguments.[].(IterUses::uses_mut)
            },
            Expression::Identifier.name,
            Expression::Type.{ name, template_args.[].[].(IterUses::uses_mut) }
        })
    }
}

impl IterUses for TypeExpression {
    fn uses_mut(&mut self) -> impl Iterator<Item = &mut String> {
        query_mut!(self.{
            name,
            template_args.[].[].(IterUses::uses_mut),
        })
    }
}

impl IterUses for Vec<Statement> {
    // this one keeps track of scope, because local declarations introduce a new scope and may
    // shadow names declared at the global level.
    // we just ignore names that refer to local variables.
    fn uses_mut(&mut self) -> impl Iterator<Item = &mut String> {
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
                        let it = query_mut!(stat.{ lhs, rhs }.(IterUses::uses_mut));
                        names.extend(it.filter(|name| !scope.contains(*name)));
                    }
                    Statement::Increment(stat) => {
                        let it = query_mut!(stat.expression.(IterUses::uses_mut));
                        names.extend(it.filter(|name| !scope.contains(*name)));
                    }
                    Statement::Decrement(stat) => {
                        let it = query_mut!(stat.expression.(IterUses::uses_mut));
                        names.extend(it.filter(|name| !scope.contains(*name)));
                    }
                    Statement::If(stat) => {
                        let it = query_mut!(stat.{
                            attributes.[].arguments.[].[].(IterUses::uses_mut),
                            if_clause.{
                                expression.(IterUses::uses_mut),
                                body.{
                                    attributes.[].arguments.[].[].(IterUses::uses_mut),
                                    statements.(x => rec(x).0)
                                }
                            },
                            else_if_clauses.[].{
                                expression.(IterUses::uses_mut),
                                body.{
                                    attributes.[].arguments.[].[].(IterUses::uses_mut),
                                    statements.(x => rec(x).0)
                                }
                            },
                            else_clause.[].body.{
                                attributes.[].arguments.[].[].(IterUses::uses_mut),
                                statements.(x => rec(x).0)
                            },
                        });
                        names.extend(it.filter(|name| !scope.contains(*name)));
                    }
                    Statement::Switch(stat) => {
                        let it = query_mut!(stat.{
                            attributes.[].arguments.[].[].(IterUses::uses_mut),
                            expression.(IterUses::uses_mut),
                            body_attributes.[].arguments.[].[].(IterUses::uses_mut),
                            clauses.[].{
                                case_selectors.[].CaseSelector::Expression.(IterUses::uses_mut),
                                body.{
                                    attributes.[].arguments.[].[].(IterUses::uses_mut),
                                    statements.(x => rec(x).0)
                                }
                            },
                        });
                        names.extend(it.filter(|name| !scope.contains(*name)));
                    }
                    Statement::Loop(stat) => {
                        let it =
                            query_mut!(stat.attributes.[].arguments.[].[].(IterUses::uses_mut));
                        names.extend(it.filter(|name| !scope.contains(*name)));

                        let it = query_mut!(stat.body.attributes.[].arguments.[].[].(IterUses::uses_mut));
                        names.extend(it.filter(|name| !scope.contains(*name)));

                        // these ones have to be handled separatly, because the continuing statement
                        // is separated from the rest of the statements (same for the break-if)
                        let it = rec(&mut stat.body.statements).0;
                        names.extend(it.into_iter().filter(|name| !scope.contains(*name)));

                        if let Some(stat) = &mut stat.continuing {
                            let it = query_mut!(stat.body.attributes.[].arguments.[].[].(IterUses::uses_mut));
                            names.extend(it.filter(|name| !scope.contains(*name)));

                            let (it, cont_scope) = rec(&mut stat.body.statements);
                            names.extend(it.into_iter().filter(|name| !scope.contains(*name)));

                            if let Some(stat) = &mut stat.break_if {
                                let it = IterUses::uses_mut(&mut stat.expression);
                                names.extend(it.filter(|name| !cont_scope.contains(*name)));
                            }
                        }
                    }
                    Statement::For(stat) => {
                        let it =
                            query_mut!(stat.attributes.[].arguments.[].[].(IterUses::uses_mut));
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
                        let it = query_mut!(stat.condition.[].(IterUses::uses_mut));
                        names.extend(it.filter(|name| !body_scope.contains(*name)));

                        if let Some(update) = &mut stat.update {
                            let it = rec(std::iter::once(&mut **update)).0.into_iter();
                            names.extend(it.filter(|name| !body_scope.contains(*name)));
                        }

                        let it = query_mut!(stat.body.{
                            attributes.[].arguments.[].[].(IterUses::uses_mut),
                            statements.(x => rec(x).0)
                        });
                        names.extend(it.filter(|name| !body_scope.contains(*name)));
                    }
                    Statement::While(stat) => {
                        let it = query_mut!(stat.{
                            attributes.[].arguments.[].[].(IterUses::uses_mut),
                            condition.(IterUses::uses_mut),
                            body.{
                                attributes.[].arguments.[].[].(IterUses::uses_mut),
                                statements.(x => rec(x).0)
                            }
                        });
                        names.extend(it.filter(|name| !scope.contains(*name)));
                    }
                    Statement::Break(_) => (),
                    Statement::Continue(_) => (),
                    Statement::Return(stat) => {
                        let it = query_mut!(stat.expression.[].(IterUses::uses_mut));
                        names.extend(it.filter(|name| !scope.contains(*name)));
                    }
                    Statement::Discard(_) => (),
                    Statement::FunctionCall(stat) => {
                        let it = query_mut!(stat.call.{
                            name,
                            template_args.[].[].(IterUses::uses_mut),
                            arguments.[].(IterUses::uses_mut),
                        });
                        names.extend(it.filter(|name| !scope.contains(*name)));
                    }
                    Statement::ConstAssert(stat) => {
                        let it = query_mut!(stat.expression.(IterUses::uses_mut));
                        names.extend(it.filter(|name| !scope.contains(*name)));
                    }
                    Statement::Declaration(stat) => {
                        scope.insert(stat.name.clone());
                        let it = query_mut!(stat.{
                            attributes.[].arguments.[].[].(IterUses::uses_mut),
                            template_args.[].[].(IterUses::uses_mut),
                            ty.[].(IterUses::uses_mut),
                            initializer.[].(IterUses::uses_mut),
                        });
                        names.extend(it.filter(|name| !scope.contains(*name)));
                    }
                    _ => (),
                }
            }
            (names, scope)
        }
        rec(self).0.into_iter()
    }
}

pub fn decl_name(decl: &GlobalDeclaration) -> Option<&str> {
    match decl {
        wgsl_parse::syntax::GlobalDeclaration::Void => None,
        wgsl_parse::syntax::GlobalDeclaration::Declaration(d) => Some(&d.name),
        wgsl_parse::syntax::GlobalDeclaration::TypeAlias(d) => Some(&d.name),
        wgsl_parse::syntax::GlobalDeclaration::Struct(d) => Some(&d.name),
        wgsl_parse::syntax::GlobalDeclaration::Function(d) => Some(&d.name),
        wgsl_parse::syntax::GlobalDeclaration::ConstAssert(_) => None,
    }
}

pub fn decl_name_mut(decl: &mut GlobalDeclaration) -> Option<&mut String> {
    match decl {
        wgsl_parse::syntax::GlobalDeclaration::Void => None,
        wgsl_parse::syntax::GlobalDeclaration::Declaration(d) => Some(&mut d.name),
        wgsl_parse::syntax::GlobalDeclaration::TypeAlias(d) => Some(&mut d.name),
        wgsl_parse::syntax::GlobalDeclaration::Struct(d) => Some(&mut d.name),
        wgsl_parse::syntax::GlobalDeclaration::Function(d) => Some(&mut d.name),
        wgsl_parse::syntax::GlobalDeclaration::ConstAssert(_) => None,
    }
}

pub fn rename_decl(wesl: &mut TranslationUnit, old_name: &str, new_name: &str) {
    for name in wesl.uses_mut() {
        if name == old_name {
            *name = new_name.to_string();
        }
    }

    for decl in &mut wesl.global_declarations {
        if let Some(name) = decl_name_mut(decl) {
            if name == old_name {
                *name = new_name.to_string();
            }
        }
    }
}

pub fn entry_points(wesl: &TranslationUnit) -> impl Iterator<Item = &str> {
    wesl.global_declarations
        .iter()
        .filter_map(|decl| match decl {
            GlobalDeclaration::Function(decl) => decl
                .attributes
                .iter()
                .find(|attr| {
                    attr.name == "vertex" || attr.name == "fragment" || attr.name == "compute"
                })
                .is_some()
                .then_some(decl.name.as_str()),
            _ => None,
        })
}
