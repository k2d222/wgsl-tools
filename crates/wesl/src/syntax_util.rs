use std::{borrow::Cow, collections::HashSet, iter::Iterator};

use wgsl_parse::syntax::*;
use wgsl_parse_macros::query_mut;

/// was that not in the std at some point???
type BoxedIterator<'a, T> = Box<dyn Iterator<Item = T> + 'a>;

#[allow(dead_code)]
trait IteratorExt: Iterator {
    fn boxed<'a>(self) -> BoxedIterator<'a, Self::Item>
    where
        Self: Sized + 'a;
}

impl<T: Iterator> IteratorExt for T {
    fn boxed<'a>(self) -> BoxedIterator<'a, Self::Item>
    where
        Self: Sized + 'a,
    {
        Box::new(self)
    }
}

/// A trait that iterates over identifiers used in a syntax node. (reference identifiers)
pub trait IterIdents {
    fn iter_idents(&mut self) -> impl Iterator<Item = &mut TypeExpression>;
}

impl IterIdents for GlobalDeclaration {
    fn iter_idents(&mut self) -> impl Iterator<Item = &mut TypeExpression> {
        query_mut!(self.{
            GlobalDeclaration::Declaration.(x => x.iter_idents()),
            GlobalDeclaration::TypeAlias.(x => x.iter_idents()),
            GlobalDeclaration::Struct.(x => x.iter_idents()),
            GlobalDeclaration::Function.(x => x.iter_idents()),
            GlobalDeclaration::ConstAssert.(x => x.iter_idents())
        })
    }
}

impl IterIdents for Declaration {
    fn iter_idents(&mut self) -> impl Iterator<Item = &mut TypeExpression> {
        query_mut!(self.{
            attributes.[].(x => x.iter_idents()),
            ty.[],
            initializer.[].(x => x.iter_idents()),
        })
    }
}

impl IterIdents for TypeAlias {
    fn iter_idents(&mut self) -> impl Iterator<Item = &mut TypeExpression> {
        query_mut!(self.{
            #[cfg(feature = "attributes")]
            attributes.[].(x => x.iter_idents()),
            ty,
        })
    }
}

impl IterIdents for Struct {
    fn iter_idents(&mut self) -> impl Iterator<Item = &mut TypeExpression> {
        query_mut!(self.{
            #[cfg(feature = "attributes")]
            attributes.[].(x => x.iter_idents()),
            members.[].{
                attributes.[].(x => x.iter_idents()),
                ty,
            },
        })
    }
}

impl IterIdents for Function {
    fn iter_idents(&mut self) -> impl Iterator<Item = &mut TypeExpression> {
        query_mut!(self.{
            attributes.[].(x => x.iter_idents()),
            parameters.[].{
                attributes.[].(x => x.iter_idents()),
                ty,
            },
            return_attributes.[].(x => x.iter_idents()),
            return_type.[],
            body.{
                attributes.[].(x => x.iter_idents()),
                statements.[].(x => x.iter_idents()),
            }
        })
    }
}

impl IterIdents for ConstAssert {
    fn iter_idents(&mut self) -> impl Iterator<Item = &mut TypeExpression> {
        query_mut!(self.{
            #[cfg(feature = "attributes")]
            attributes.[].(x => x.iter_idents()),
            expression.(x => x.iter_idents()),
        })
    }
}

impl IterIdents for Attribute {
    fn iter_idents(&mut self) -> impl Iterator<Item = &mut TypeExpression> {
        #[cfg(feature = "generics")]
        {
            query_mut!(self.{
                Attribute::Align.(x => x.iter_idents()),
                Attribute::Binding.(x => x.iter_idents()),
                Attribute::BlendSrc.(x => x.iter_idents()),
                Attribute::Group.(x => x.iter_idents()),
                Attribute::Id.(x => x.iter_idents()),
                Attribute::Location.(x => x.iter_idents()),
                Attribute::Size.(x => x.iter_idents()),
                Attribute::WorkgroupSize.{
                    x.(x => x.iter_idents()),
                    y.[].(x => x.iter_idents()),
                    z.[].(x => x.iter_idents()),
                },
                Attribute::Type.variants.[],
                Attribute::Custom.arguments.[].[].(x => x.iter_idents())
            })
        }
        #[cfg(not(feature = "generics"))]
        {
            query_mut!(self.{
                Attribute::Align.(x => x.iter_idents()),
                Attribute::Binding.(x => x.iter_idents()),
                Attribute::BlendSrc.(x => x.iter_idents()),
                Attribute::Group.(x => x.iter_idents()),
                Attribute::Id.(x => x.iter_idents()),
                Attribute::Location.(x => x.iter_idents()),
                Attribute::Size.(x => x.iter_idents()),
                Attribute::WorkgroupSize.{
                    x.(x => x.iter_idents()),
                    y.[].(x => x.iter_idents()),
                    z.[].(x => x.iter_idents()),
                },
                Attribute::Custom.arguments.[].[].(x => x.iter_idents())
            })
        }
    }
}

impl IterIdents for Expression {
    fn iter_idents(&mut self) -> impl Iterator<Item = &mut TypeExpression> {
        query_mut!(self.{
            Expression::Parenthesized.expression.(x => x.iter_idents()),
            Expression::NamedComponent.base.(x => x.iter_idents()),
            Expression::Indexing.{ base, index }.(x => x.iter_idents()),
            Expression::Unary.operand.(x => x.iter_idents()),
            Expression::Binary.{ left, right }.(x => x.iter_idents()),
            Expression::FunctionCall.{
                ty,
                arguments.[].(x => x.iter_idents())
            },
            Expression::TypeOrIdentifier
        })
    }
}

impl IterIdents for ExpressionNode {
    fn iter_idents(&mut self) -> impl Iterator<Item = &mut TypeExpression> {
        self.node_mut().iter_idents()
    }
}

impl IterIdents for TypeExpression {
    fn iter_idents(&mut self) -> impl Iterator<Item = &mut TypeExpression> {
        query_mut!(self.template_args.[].[].(x => x.iter_idents()))
    }
}

impl IterIdents for Statement {
    fn iter_idents(&mut self) -> impl Iterator<Item = &mut TypeExpression> {
        query_mut!(self.{
            Statement::Compound.{
                attributes.[].(x => x.iter_idents()),
                statements.[].(x => x.iter_idents()),
            },
            Statement::Assignment.{
                #[cfg(feature = "attributes")]
                attributes.[].(x => x.iter_idents()),
                lhs.(x => x.iter_idents()),
                rhs.(x => x.iter_idents()),
            },
            Statement::Increment.{
                #[cfg(feature = "attributes")]
                attributes.[].(x => x.iter_idents()),
                expression.(x => x.iter_idents()),
            },
            Statement::Decrement.{
                #[cfg(feature = "attributes")]
                attributes.[].(x => x.iter_idents()),
                expression.(x => x.iter_idents()),
            },
            Statement::If.{
                attributes.[].(x => x.iter_idents()),
                if_clause.{
                    expression.(x => x.iter_idents()),
                    body.{
                        attributes.[].(x => x.iter_idents()),
                        statements.[].(x => x.iter_idents()),
                    }
                },
                else_if_clauses.[].{
                    #[cfg(feature = "attributes")]
                    attributes.[].(x => x.iter_idents()),
                    expression.(x => x.iter_idents()),
                    body.{
                        attributes.[].(x => x.iter_idents()),
                        statements.[].(x => x.iter_idents()),
                    }
                },
                else_clause.[].{
                    #[cfg(feature = "attributes")]
                    attributes.[].(x => x.iter_idents()),
                    body.{
                        attributes.[].(x => x.iter_idents()),
                        statements.[].(x => x.iter_idents()),
                    }
                },
            },
            Statement::Switch.{
                attributes.[].(x => x.iter_idents()),
                expression.(x => x.iter_idents()),
                body_attributes.[].(x => x.iter_idents()),
                clauses.[].{
                    attributes.[].(x => x.iter_idents()),
                    case_selectors.[].CaseSelector::Expression.(x => x.iter_idents()),
                    body.{
                        attributes.[].(x => x.iter_idents()),
                        statements.[].(x => x.iter_idents()),
                    }
                }
            },
            Statement::Loop.{
                attributes.[].(x => x.iter_idents()),
                body.{
                    attributes.[].(x => x.iter_idents()),
                    statements.[].(x => x.iter_idents()),
                },
                continuing.[].{
                    #[cfg(feature = "attributes")]
                    attributes.[].(x => x.iter_idents()),
                    body.{
                        attributes.[].(x => x.iter_idents()),
                        statements.[].(x => x.iter_idents()),
                    },
                    break_if.[].{
                        #[cfg(feature = "attributes")]
                        attributes.[].(x => x.iter_idents()),
                        expression.(x => x.iter_idents()),
                    }
                }
            },
            Statement::For.{
                attributes.[].(x => x.iter_idents()),
                initializer.[].(x => x.iter_idents()),
                condition.[].(x => x.iter_idents()),
                update.[].(x => x.iter_idents()),
                body.{
                    attributes.[].(x => x.iter_idents()),
                    statements.[].(x => x.iter_idents()),
                },
            },
            Statement::While.{
                attributes.[].(x => x.iter_idents()),
                condition.(x => x.iter_idents()),
                body.{
                    attributes.[].(x => x.iter_idents()),
                    statements.[].(x => x.iter_idents()),
                },
            },
            #[cfg(feature = "attributes")]
            Statement::Break.{
                attributes.[].(x => x.iter_idents()),
            },
            #[cfg(feature = "attributes")]
            Statement::Continue.{
                attributes.[].(x => x.iter_idents()),
            },
            Statement::Return.{
                #[cfg(feature = "attributes")]
                attributes.[].(x => x.iter_idents()),
                expression.[].(x => x.iter_idents()),
            },
            #[cfg(feature = "attributes")]
            Statement::Discard.{
                attributes.[].(x => x.iter_idents()),
            },
            Statement::FunctionCall.{
                #[cfg(feature = "attributes")]
                attributes.[].(x => x.iter_idents()),
                call.{
                    ty,
                    arguments.[].(x => x.iter_idents()),
                }
            },
            Statement::ConstAssert.{
                #[cfg(feature = "attributes")]
                attributes.[].(x => x.iter_idents()),
                expression.(x => x.iter_idents()),
            },
            Statement::Declaration.{
                attributes.[].(x => x.iter_idents()),
                ty.[],
                initializer.[].(x => x.iter_idents()),
            },
        })
    }
}

impl IterIdents for TemplateArg {
    fn iter_idents(&mut self) -> impl Iterator<Item = &mut TypeExpression> {
        query_mut!(self.expression.(x => x.iter_idents()))
    }
}

#[allow(unused)]
pub fn decl_ident(decl: &GlobalDeclaration) -> Option<&Ident> {
    match decl {
        wgsl_parse::syntax::GlobalDeclaration::Void => None,
        wgsl_parse::syntax::GlobalDeclaration::Declaration(d) => Some(&d.ident),
        wgsl_parse::syntax::GlobalDeclaration::TypeAlias(d) => Some(&d.ident),
        wgsl_parse::syntax::GlobalDeclaration::Struct(d) => Some(&d.ident),
        wgsl_parse::syntax::GlobalDeclaration::Function(d) => Some(&d.ident),
        wgsl_parse::syntax::GlobalDeclaration::ConstAssert(_) => None,
    }
}

#[allow(unused)]
pub fn entry_points(wesl: &TranslationUnit) -> impl Iterator<Item = &Ident> {
    wesl.global_declarations
        .iter()
        .filter_map(|decl| match decl {
            GlobalDeclaration::Function(decl) => decl
                .attributes
                .iter()
                .find(|attr| {
                    matches!(
                        attr,
                        Attribute::Vertex | Attribute::Fragment | Attribute::Compute
                    )
                })
                .is_some()
                .then_some(&decl.ident),
            _ => None,
        })
}

/// make all identifiers that point to the same declaration refer to the same string.
///
/// retarget local references to the local declaration ident and global
/// references to the global declaration ident. It does this by keeping track of the
/// local declarations scope.
pub fn retarget_idents(wesl: &mut TranslationUnit) {
    // keep track of declarations in a scope.
    type Scope<'a> = Cow<'a, HashSet<Ident>>;

    let scope: Scope = Cow::Owned(
        wesl.global_declarations
            .iter()
            .filter_map(|decl| decl.ident().cloned())
            .collect::<HashSet<_>>(),
    );

    fn retarget_ty(ty: &mut TypeExpression, scope: &HashSet<Ident>) {
        if let Some(id) = scope
            .iter()
            .find(|ident| &*ident.name() == &*ty.ident.name())
        {
            ty.ident = id.clone();
        }
        query_mut!(ty.template_args.[].[].(x => x.iter_idents()))
            .for_each(|ty| retarget_ty(ty, scope));
    }

    // retarget local references to the local declaration ident and global
    // references to the global declaration ident. It does this by keeping track of the
    // local declarations scope.
    fn retarget_stats<'a>(
        stats: impl IntoIterator<Item = &'a mut StatementNode>,
        mut scope: Scope<'a>,
    ) -> Scope<'a> {
        stats.into_iter().for_each(|stat| match stat.node_mut() {
            Statement::Void => (),
            Statement::Compound(s) => {
                query_mut!(s.attributes.[].(x => x.iter_idents()))
                    .for_each(|ty| retarget_ty(ty, &scope));
                retarget_stats(&mut s.statements, scope.clone());
            }
            Statement::Assignment(s) => {
                query_mut!(s.{
                    #[cfg(feature = "attributes")]
                    attributes.[].(x => x.iter_idents()),
                    lhs.(x => x.iter_idents()),
                    rhs.(x => x.iter_idents()),
                })
                .for_each(|ty| retarget_ty(ty, &scope));
            }
            Statement::Increment(s) => {
                query_mut!(s.{
                    #[cfg(feature = "attributes")]
                    attributes.[].(x => x.iter_idents()),
                    expression.(x => x.iter_idents()),
                })
                .for_each(|ty| retarget_ty(ty, &scope));
            }
            Statement::Decrement(s) => {
                query_mut!(s.{
                    #[cfg(feature = "attributes")]
                    attributes.[].(x => x.iter_idents()),
                    expression.(x => x.iter_idents()),
                })
                .for_each(|ty| retarget_ty(ty, &scope));
            }
            Statement::If(s) => {
                let s2 = &mut *s; // COMBAK: not sure why this is needed?
                query_mut!(s2.{
                    attributes.[].(x => x.iter_idents()),
                    if_clause.{
                        expression.(x => x.iter_idents()),
                        body.{
                            attributes.[].(x => x.iter_idents()),
                        }
                    },
                    else_if_clauses.[].{
                        #[cfg(feature = "attributes")]
                        attributes.[].(x => x.iter_idents()),
                        expression.(x => x.iter_idents()),
                        body.{
                            attributes.[].(x => x.iter_idents()),
                        }
                    },
                    else_clause.[].{
                        #[cfg(feature = "attributes")]
                        attributes.[].(x => x.iter_idents()),
                        body.{
                            attributes.[].(x => x.iter_idents()),
                        },
                    },
                })
                .for_each(|ty| retarget_ty(ty, &scope));
                retarget_stats(&mut s.if_clause.body.statements, scope.clone());
                for clause in &mut s.else_if_clauses {
                    retarget_stats(&mut clause.body.statements, scope.clone());
                }
                if let Some(clause) = &mut s.else_clause {
                    retarget_stats(&mut clause.body.statements, scope.clone());
                }
            }
            Statement::Switch(s) => {
                let s2 = &mut *s; // COMBAK: not sure why this is needed?
                query_mut!(s2.{
                    attributes.[].(x => x.iter_idents()),
                    expression.(x => x.iter_idents()),
                    body_attributes.[].(x => x.iter_idents()),
                    clauses.[].{
                        #[cfg(feature = "attributes")]
                        attributes.[].(x => x.iter_idents()),
                        case_selectors.[].CaseSelector::Expression.(x => x.iter_idents()),
                        body.{
                            attributes.[].(x => x.iter_idents()),
                        }
                    },

                })
                .for_each(|ty| retarget_ty(ty, &scope));
                for clause in &mut s.clauses {
                    retarget_stats(&mut clause.body.statements, scope.clone());
                }
            }
            Statement::Loop(s) => {
                let s2 = &mut *s; // COMBAK: not sure why this is needed?
                query_mut!(s2.{
                    attributes.[].(x => x.iter_idents()),
                    body.attributes.[].(x => x.iter_idents()),
                })
                .for_each(|ty| retarget_ty(ty, &scope));
                let scope = retarget_stats(&mut s.body.statements, scope.clone());
                // continuing, if present, must be the last statement of the loop body
                // and therefore has access to the scope at the end of the body.
                if let Some(s) = &mut s.continuing {
                    let s2 = &mut *s; // COMBAK: not sure why this is needed?
                    query_mut!(s2.{
                        attributes.[].(x => x.iter_idents()),
                        body.attributes.[].(x => x.iter_idents()),
                    })
                    .for_each(|ty| retarget_ty(ty, &scope));
                    let scope = retarget_stats(&mut s.body.statements, scope.clone());
                    // break-if, if present, must be the last statement of the continuing body
                    // and therefore has access to the scope at the end of the body.
                    if let Some(s) = &mut s.break_if {
                        let s2 = &mut *s; // COMBAK: not sure why this is needed?
                        query_mut!(s2.{
                            attributes.[].(x => x.iter_idents()),
                            expression.(x => x.iter_idents()),
                        })
                        .for_each(|ty| retarget_ty(ty, &scope));
                    }
                }
            }
            Statement::For(s) => {
                query_mut!(s.attributes.[].(x => x.iter_idents()))
                    .for_each(|ty| retarget_ty(ty, &scope));
                let scope = if let Some(init) = &mut s.initializer {
                    retarget_stats([init], scope.clone())
                } else {
                    scope.clone()
                };
                query_mut!(s.condition.[].(x => x.iter_idents()))
                    .for_each(|ty| retarget_ty(ty, &scope));
                query_mut!(s.body.attributes.[].(x => x.iter_idents()))
                    .for_each(|ty| retarget_ty(ty, &scope));
                if let Some(update) = &mut s.update {
                    retarget_stats([update], scope.clone());
                }
                retarget_stats(&mut s.body.statements, scope);
            }
            Statement::While(s) => {
                let s2 = &mut *s; // COMBAK: not sure why this is needed?
                query_mut!(s2.{
                    attributes.[].(x => x.iter_idents()),
                    condition.(x => x.iter_idents()),
                    body.attributes.[].(x => x.iter_idents()),
                })
                .for_each(|ty| retarget_ty(ty, &scope));
                retarget_stats(&mut s.body.statements, scope.clone());
            }
            Statement::Break(s) => {
                #[cfg(feature = "attributes")]
                query_mut!(s.attributes.[].(x => x.iter_idents()))
                    .for_each(|ty| retarget_ty(ty, &scope));
            }
            Statement::Continue(s) => {
                #[cfg(feature = "attributes")]
                query_mut!(s.attributes.[].(x => x.iter_idents()))
                    .for_each(|ty| retarget_ty(ty, &scope));
            }
            Statement::Return(s) => {
                query_mut!(s.expression.[].(x => x.iter_idents()))
                    .for_each(|ty| retarget_ty(ty, &scope));
            }
            Statement::Discard(s) => {
                #[cfg(feature = "attributes")]
                query_mut!(s.attributes.[].(x => x.iter_idents()))
                    .for_each(|ty| retarget_ty(ty, &scope));
            }
            Statement::FunctionCall(s) => {
                query_mut!(s.{
                   #[cfg(feature = "attributes")]
                    attributes.[].(x => x.iter_idents()),
                    call.{
                        ty,
                        arguments.[].(x => x.iter_idents()),
                    }
                })
                .for_each(|ty| retarget_ty(ty, &scope));
            }
            Statement::ConstAssert(s) => {
                query_mut!(s.{
                    expression.(x => x.iter_idents())
                })
                .for_each(|ty| retarget_ty(ty, &scope));
            }
            Statement::Declaration(s) => {
                let s2 = &mut *s; // COMBAK: not sure why this is needed?
                query_mut!(s2.{
                    attributes.[].(x => x.iter_idents()),
                    ty.[],
                    initializer.[].(x => x.iter_idents()),
                })
                .for_each(|ty| retarget_ty(ty, &scope));
                scope.to_mut().replace(s.ident.clone());
            }
        });
        scope
    }

    for decl in &mut wesl.global_declarations {
        match decl {
            GlobalDeclaration::Void => (),
            GlobalDeclaration::Declaration(d) => {
                d.iter_idents().for_each(|ty| retarget_ty(ty, &scope))
            }
            GlobalDeclaration::TypeAlias(d) => {
                d.iter_idents().for_each(|ty| retarget_ty(ty, &scope))
            }
            GlobalDeclaration::Struct(d) => d.iter_idents().for_each(|ty| retarget_ty(ty, &scope)),
            GlobalDeclaration::Function(d) => {
                #[cfg(feature = "generics")]
                let scope = {
                    let mut scope = scope.clone();
                    scope
                        .to_mut()
                        .extend(d.attributes.iter().filter_map(|attr| match attr {
                            Attribute::Type(attr) => Some(attr.ident.clone()),
                            _ => None,
                        }));
                    scope
                };

                let d2 = &mut *d; // COMBAK: not sure why this is needed?
                query_mut!(d2.{
                    attributes.[].(x => x.iter_idents()),
                    parameters.[].{
                        attributes.[].(x => x.iter_idents()),
                        ty,
                    },
                    return_attributes.[].(x => x.iter_idents()),
                    return_type.[],
                    body.{
                        attributes.[].(x => x.iter_idents()),
                    }
                })
                .for_each(|ty| retarget_ty(ty, &scope));
                retarget_stats(&mut d.body.statements, scope.clone());
            }
            GlobalDeclaration::ConstAssert(d) => {
                d.iter_idents().for_each(|ty| retarget_ty(ty, &scope))
            }
        }
    }
}
