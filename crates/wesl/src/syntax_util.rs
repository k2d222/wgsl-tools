use std::{
    collections::HashSet,
    iter::{empty, once, Iterator},
};

use itertools::{chain, Itertools};
use wgsl_parse::syntax::*;
use wgsl_parse_macros::query_mut;

/// was that not in the std at some point???
type BoxedIterator<'a, T> = Box<dyn Iterator<Item = T> + 'a>;
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

/// keep track of declarations in a scope.
type Scope = HashSet<Ident>;

/// A trait that iterates over identifiers that refer to a declaration in the global scope.
/// It does that by keeping track of local declarations in function scope.
pub trait IterUses {
    fn uses_mut(&mut self) -> impl Iterator<Item = &mut TypeExpression>;
}

impl IterUses for TranslationUnit {
    fn uses_mut(&mut self) -> impl Iterator<Item = &mut TypeExpression> {
        query_mut!(self.{
            #[cfg(feature = "attributes")]
            global_directives.[].{
                GlobalDirective::Diagnostic.attributes.[].(IterUses::uses_mut),
                GlobalDirective::Enable.attributes.[].(IterUses::uses_mut),
                GlobalDirective::Requires.attributes.[].(IterUses::uses_mut),
            },
            global_declarations.[].(IterUses::uses_mut)
        })
    }
}

impl IterUses for GlobalDeclaration {
    fn uses_mut(&mut self) -> impl Iterator<Item = &mut TypeExpression> {
        query_mut!(self.{
            GlobalDeclaration::Declaration.{
                attributes.[].(IterUses::uses_mut),
                ty.[],
                initializer.[].(IterUses::uses_mut),
            },
            GlobalDeclaration::TypeAlias.{
                #[cfg(feature = "attributes")]
                attributes.[].(IterUses::uses_mut),
                ty,
            },
            GlobalDeclaration::Struct.{
                #[cfg(feature = "attributes")]
                attributes.[].(IterUses::uses_mut),
                members.[].{
                    attributes.[].(IterUses::uses_mut),
                    ty,
                },
            },
            GlobalDeclaration::Function.(IterUses::uses_mut),
            GlobalDeclaration::ConstAssert.{
                #[cfg(feature = "attributes")]
                attributes.[].(IterUses::uses_mut),
                expression.(IterUses::uses_mut),
            }
        })
    }
}

impl IterUses for Function {
    fn uses_mut(&mut self) -> impl Iterator<Item = &mut TypeExpression> {
        query_mut!(self.{
            attributes.[].(IterUses::uses_mut),
            parameters.[].{
                attributes.[].(IterUses::uses_mut),
                ty,
            },
            return_attributes.[].(IterUses::uses_mut),
            return_type.[],
            body.{
                attributes.[].(IterUses::uses_mut),
                statements.(IterUses::uses_mut)
            }
        })
    }
}

impl IterUses for Attribute {
    fn uses_mut(&mut self) -> impl Iterator<Item = &mut TypeExpression> {
        #[cfg(feature = "generics")]
        {
            query_mut!(self.{
                Attribute::Align.(IterUses::uses_mut),
                Attribute::Binding.(IterUses::uses_mut),
                Attribute::BlendSrc.(IterUses::uses_mut),
                Attribute::Group.(IterUses::uses_mut),
                Attribute::Id.(IterUses::uses_mut),
                Attribute::Location.(IterUses::uses_mut),
                Attribute::Size.(IterUses::uses_mut),
                Attribute::WorkgroupSize.{
                    x.(IterUses::uses_mut),
                    y.[].(IterUses::uses_mut),
                    z.[].(IterUses::uses_mut),
                },
                Attribute::Type.variants.[],
                Attribute::Custom.arguments.[].[].(IterUses::uses_mut)
            })
        }
        #[cfg(not(feature = "generics"))]
        {
            query_mut!(self.{
                Attribute::Align.(IterUses::uses_mut),
                Attribute::Binding.(IterUses::uses_mut),
                Attribute::BlendSrc.(IterUses::uses_mut),
                Attribute::Group.(IterUses::uses_mut),
                Attribute::Id.(IterUses::uses_mut),
                Attribute::Location.(IterUses::uses_mut),
                Attribute::Size.(IterUses::uses_mut),
                Attribute::WorkgroupSize.{
                    x.(IterUses::uses_mut),
                    y.[].(IterUses::uses_mut),
                    z.[].(IterUses::uses_mut),
                },
                Attribute::Custom.arguments.[].[].(IterUses::uses_mut)
            })
        }
    }
}

impl IterUses for Expression {
    fn uses_mut(&mut self) -> impl Iterator<Item = &mut TypeExpression> {
        query_mut!(self.{
            Expression::Parenthesized.expression.(x => x.uses_mut()),
            Expression::NamedComponent.base.(x => x.uses_mut()),
            Expression::Indexing.{ base, index }.(x => x.uses_mut()),
            Expression::Unary.operand.(x => x.uses_mut()),
            Expression::Binary.{ left, right }.(x => x.uses_mut()),
            Expression::FunctionCall.{
                ty,
                arguments.[].(IterUses::uses_mut)
            },
            Expression::TypeOrIdentifier
        })
    }
}

impl IterUses for ExpressionNode {
    fn uses_mut(&mut self) -> impl Iterator<Item = &mut TypeExpression> {
        self.node_mut().uses_mut()
    }
}

impl IterUses for TypeExpression {
    fn uses_mut(&mut self) -> impl Iterator<Item = &mut TypeExpression> {
        query_mut!(self.template_args.[].[].(IterUses::uses_mut))
    }
}

impl IterUses for TemplateArg {
    fn uses_mut(&mut self) -> impl Iterator<Item = &mut TypeExpression> {
        query_mut!(self.expression.(IterUses::uses_mut))
    }
}

impl IterUses for Vec<StatementNode> {
    // this one keeps track of scope, because local declarations introduce a new scope and may
    // shadow names declared at the global level.
    // we just ignore names that refer to local variables.
    fn uses_mut(&mut self) -> impl Iterator<Item = &mut TypeExpression> {
        fn rec<'a>(
            statements: impl IntoIterator<Item = &'a mut StatementNode>,
        ) -> (Vec<&'a mut TypeExpression>, Scope) {
            let mut scope = Scope::new();
            let names = statements
                .into_iter()
                .flat_map(|stat| {
                    let it: BoxedIterator<'a, &mut TypeExpression> = match stat.node_mut() {
                        Statement::Compound(stat) => chain!(
                            query_mut!(stat.attributes.[].(IterUses::uses_mut)),
                            rec(&mut stat.statements).0.into_iter(),
                        )
                        .boxed(),
                        Statement::Assignment(stat) => query_mut!(stat.{
                            #[cfg(feature = "attributes")]
                            attributes.[].(IterUses::uses_mut),
                            lhs.(IterUses::uses_mut),
                            rhs.(IterUses::uses_mut),
                        })
                        .boxed(),
                        Statement::Increment(stat) => query_mut!(stat.{
                            #[cfg(feature = "attributes")]
                            attributes.[].(IterUses::uses_mut),
                            expression.(IterUses::uses_mut),
                        })
                        .boxed(),
                        Statement::Decrement(stat) => query_mut!(stat.{
                            #[cfg(feature = "attributes")]
                            attributes.[].(IterUses::uses_mut),
                            expression.(IterUses::uses_mut),
                        })
                        .boxed(),
                        Statement::If(stat) => query_mut!(stat.{
                            attributes.[].(IterUses::uses_mut),
                            if_clause.{
                                expression.(IterUses::uses_mut),
                                body.{
                                    attributes.[].(IterUses::uses_mut),
                                    statements.(x => rec(x).0)
                                }
                            },
                            else_if_clauses.[].{
                                #[cfg(feature = "attributes")]
                                attributes.[].(IterUses::uses_mut),
                                expression.(IterUses::uses_mut),
                                body.{
                                    attributes.[].(IterUses::uses_mut),
                                    statements.(x => rec(x).0)
                                }
                            },
                            else_clause.[].{
                                #[cfg(feature = "attributes")]
                                attributes.[].(IterUses::uses_mut),
                                body.{
                                    attributes.[].(IterUses::uses_mut),
                                    statements.(x => rec(x).0)
                                },
                            },
                        })
                        .boxed(),
                        Statement::Switch(stat) => query_mut!(stat.{
                            attributes.[].(IterUses::uses_mut),
                            expression.(IterUses::uses_mut),
                            body_attributes.[].(IterUses::uses_mut),
                            clauses.[].{
                                #[cfg(feature = "attributes")]
                                attributes.[].(IterUses::uses_mut),
                                case_selectors.[].CaseSelector::Expression.(IterUses::uses_mut),
                                body.{
                                    attributes.[].(IterUses::uses_mut),
                                    statements.(x => rec(x).0)
                                }
                            },
                        })
                        .boxed(),
                        Statement::Loop(stat) => {
                            let it1 = query_mut!(stat.attributes.[].(IterUses::uses_mut));
                            let it2 = query_mut!(stat.body.attributes.[].(IterUses::uses_mut));

                            // these ones have to be handled separatly, because the continuing statement
                            // is separated from the rest of the statements (same for the break-if)
                            let (it3, cont_scope) = rec(&mut stat.body.statements);

                            let it4 =
                                {
                                    let cont_scope = cont_scope.clone();
                                    stat.continuing.iter_mut().flat_map(move |stat| {
                                    #[cfg(feature = "attributes")]
                                    let it1 =
                                        query_mut!(stat.body.attributes.[].(IterUses::uses_mut));
                                    #[cfg(not(feature = "attributes"))]
                                    let it1 = empty();

                                    let (it2, break_scope) = rec(&mut stat.body.statements);
                                    let it3 = stat
                                        .break_if
                                        .iter_mut()
                                        .flat_map(|stat| query_mut!(stat.{
                                            #[cfg(feature = "attributes")]
                                            attributes.[].(IterUses::uses_mut),
                                            expression.(IterUses::uses_mut),
                                        }))
                                        .filter(move |ty| !break_scope.contains(&ty.ident));
                                    let cont_scope = cont_scope.clone();
                                    chain!(it1, it2, it3)
                                        .filter(move |ty| !cont_scope.contains(&ty.ident))
                                })
                                };
                            chain!(it1, it2, it3, it4).boxed()
                        }
                        Statement::For(stat) => {
                            let it1 = query_mut!(stat.attributes.[].(IterUses::uses_mut));

                            // these ones have to be handled separatly, because the for initializer
                            // statement is the parent scope of the body
                            let (it2, body_scope) = stat
                                .initializer
                                .as_mut()
                                .map(|init| {
                                    let (iter, scope) = rec(once(init));
                                    (iter, scope)
                                })
                                .unwrap_or_else(|| (Vec::new(), Scope::new()));

                            let it3 = query_mut!(stat.condition.[].(IterUses::uses_mut));

                            let it4 = stat
                                .update
                                .iter_mut()
                                .flat_map(|update| rec(once(update)).0);

                            let it5 = query_mut!(stat.body.{
                                attributes.[].(IterUses::uses_mut),
                                statements.(x => rec(x).0)
                            });
                            chain!(
                                it1,
                                it2,
                                chain!(it3, it4, it5)
                                    .filter(move |ty| !body_scope.contains(&ty.ident))
                            )
                            .boxed()
                        }
                        Statement::While(stat) => query_mut!(stat.{
                            attributes.[].(IterUses::uses_mut),
                            condition.(IterUses::uses_mut),
                            body.{
                                attributes.[].(IterUses::uses_mut),
                                statements.(x => rec(x).0)
                            }
                        })
                        .boxed(),
                        #[cfg(feature = "attributes")]
                        Statement::Break(stat) => {
                            query_mut!(stat.attributes.[].(IterUses::uses_mut)).boxed()
                        }
                        #[cfg(feature = "attributes")]
                        Statement::Continue(stat) => {
                            query_mut!(stat.attributes.[].(IterUses::uses_mut)).boxed()
                        }
                        Statement::Return(stat) => {
                            query_mut!(stat.expression.[].(IterUses::uses_mut)).boxed()
                        }
                        #[cfg(feature = "attributes")]
                        Statement::Discard(stat) => {
                            query_mut!(stat.attributes.[].(IterUses::uses_mut)).boxed()
                        }
                        Statement::FunctionCall(stat) => Box::new(query_mut!(stat.{
                            #[cfg(feature = "attributes")]
                            attributes.[].(IterUses::uses_mut),
                            call.{
                                ty,
                                arguments.[].(IterUses::uses_mut),
                            }
                        })),
                        Statement::ConstAssert(stat) => {
                            query_mut!(stat.expression.(IterUses::uses_mut)).boxed()
                        }
                        Statement::Declaration(stat) => {
                            scope.insert(stat.ident.clone());
                            query_mut!(stat.{
                                attributes.[].(IterUses::uses_mut),
                                ty.[],
                                initializer.[].(IterUses::uses_mut),
                            })
                            .boxed()
                        }
                        _ => empty().boxed(),
                    };
                    let scope = scope.clone(); // this clone is unfortunate
                    it.filter(move |ty| !scope.contains(&ty.ident))
                })
                .collect_vec();
            (names, scope)
        }
        rec(self).0.into_iter()
    }
}

#[allow(unused)]
pub fn decl_name(decl: &GlobalDeclaration) -> Option<&Ident> {
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
