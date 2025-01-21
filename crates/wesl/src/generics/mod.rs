mod mangle;

use itertools::Itertools;
use thiserror::Error;
use wgsl_parse::{syntax::*, Decorated};

use crate::attributes::stat_query_attrs;
use crate::visit::Visit;

#[derive(Clone, Debug, Error)]
pub enum GenericsError {
    #[error("template not allowed on a generic parameter")]
    DisallowedTemplate,
}

type E = GenericsError;

pub fn generate_variants(wesl: &mut TranslationUnit) -> Result<(), E> {
    let mut new_decls = Vec::new();
    for decl in &wesl.global_declarations {
        if let GlobalDeclaration::Function(decl) = decl {
            let ty_constrs = decl.attributes.iter().rev().filter_map(|attr| match attr {
                Attribute::Type(t) => Some(t),
                _ => None,
            });

            let variants = ty_constrs
                .map(|t| t.variants.iter().map(|v| (&t.ident, v)))
                .multi_cartesian_product();

            for variant in variants {
                if variant.is_empty() {
                    break;
                }
                let mut decl = decl.clone();
                decl.attributes
                    .retain(|attr| !matches!(attr, Attribute::Type(_)));

                // rename uses of the generic args with the concrete variant
                for (old_id, new_ty) in &variant {
                    let new_id = Ident::new(format!("{new_ty}"));
                    for ty in Visit::<TypeExpression>::visit_mut(&mut decl) {
                        if &ty.ident == *old_id {
                            if ty.template_args.is_some() {
                                return Err(E::DisallowedTemplate);
                            }
                            ty.ident = new_id.clone();
                        }
                    }
                }

                let constraints = variant.iter().map(|&(name, ty)| TypeConstraint {
                    ident: name.clone(),
                    variants: vec![ty.clone()],
                });

                // evaluate type attributes
                for ty in constraints {
                    // this is future-proofing. We'll want to generate fewer variants in
                    // the future by grouping them.
                    eval_ty_attrs(&mut decl.parameters, &ty)?;
                    stat_eval_ty_attrs(&mut decl.body.statements, &ty)?;
                }

                // remove evaluated type attributes
                for stat in &mut decl.body.statements {
                    for attrs in stat_query_attrs(stat) {
                        attrs.retain(|attr| match attr {
                            Attribute::Type(c) => !c.variants.is_empty(),
                            _ => true,
                        })
                    }
                }

                let signature = decl.parameters.iter().map(|p| p.ty.clone()).collect_vec();

                let new_name = mangle::mangle(decl.ident.name().as_str(), &signature);
                decl.ident = Ident::new(new_name);
                new_decls.push(decl.into());
            }
        }
    }

    // remove generic function declarations
    wesl.global_declarations.retain(|decl| !matches!(decl, GlobalDeclaration::Function(f) if f.attributes.iter().any(|attr| attr.is_type())));

    // add generic variants
    wesl.global_declarations.extend(new_decls);

    Ok(())
}

pub fn replace_calls(wesl: &mut TranslationUnit) -> Result<(), E> {
    let idents = wesl
        .global_declarations
        .iter()
        .filter_map(|decl| decl.ident())
        .cloned()
        .collect_vec();
    for expr in Visit::<ExpressionNode>::visit_mut(wesl) {
        if let Expression::FunctionCall(f) = expr.node_mut() {
            if let Some(args) = &f.ty.template_args {
                let signature = args
                    .iter()
                    .map(|arg| match arg.expression.node() {
                        Expression::Literal(_) => todo!("literal generics"),
                        Expression::TypeOrIdentifier(ty) => ty.clone(),
                        _ => panic!("invalid template arg"),
                    })
                    .collect_vec();

                let new_name = mangle::mangle(f.ty.ident.name().as_str(), &signature);
                f.ty.ident = idents
                    .iter()
                    .find(|ident| *ident.name() == new_name)
                    .unwrap()
                    .clone();
                f.ty.template_args = None;
            }
        }
        // TODO recursive
        // expr.visit_mut()
    }
    Ok(())
}

fn eval_ty_attr(opt_node: &mut Option<impl Decorated>, ty: &TypeConstraint) -> Result<(), E> {
    if let Some(node) = opt_node {
        let vars = node
            .attributes_mut()
            .iter_mut()
            .find_map(|attr| match attr {
                Attribute::Type(TypeConstraint {
                    ident: name,
                    variants,
                }) if name == &ty.ident => Some(variants),
                _ => None,
            });

        if let Some(vars) = vars {
            let keep = ty.variants.iter().all(|ty| vars.contains(ty));
            if !keep {
                *opt_node = None;
            }
        }
    }
    Ok(())
}

fn eval_ty_attrs(nodes: &mut Vec<impl Decorated>, ty: &TypeConstraint) -> Result<(), E> {
    let retains = nodes
        .iter()
        .map(|node| {
            let vars = node.attributes().iter().find_map(|attr| match attr {
                Attribute::Type(TypeConstraint {
                    ident: name,
                    variants,
                }) if name == &ty.ident => Some(variants),
                _ => None,
            });

            if let Some(vars) = vars {
                ty.variants.iter().all(|ty| vars.contains(ty))
            } else {
                true
            }
        })
        .collect_vec();

    let retains = nodes
        .iter_mut()
        .zip(retains)
        .map(|(node, keep)| {
            let ty_attr = node
                .attributes_mut()
                .iter_mut()
                .find_map(|attr| match attr {
                    Attribute::Type(TypeConstraint {
                        ident: name,
                        variants,
                    }) if name == &ty.ident => Some(variants),
                    _ => None,
                });
            if let Some(ty_attr) = ty_attr {
                ty_attr.clear();
                keep
            } else {
                true
            }
        })
        .collect_vec();

    let mut it = retains.iter();
    nodes.retain(|_| *it.next().unwrap());
    Ok(())
}

fn stat_eval_ty_attrs(statements: &mut Vec<StatementNode>, ty: &TypeConstraint) -> Result<(), E> {
    fn rec_one(stat: &mut StatementNode, ty: &TypeConstraint) -> Result<(), GenericsError> {
        match stat.node_mut() {
            Statement::Compound(stat) => rec(&mut stat.statements, ty)?,
            Statement::If(stat) => {
                rec(&mut stat.if_clause.body.statements, ty)?;
                for elif in &mut stat.else_if_clauses {
                    rec(&mut elif.body.statements, ty)?;
                }
                if let Some(el) = &mut stat.else_clause {
                    rec(&mut el.body.statements, ty)?;
                }
            }
            Statement::Switch(stat) => {
                eval_ty_attrs(&mut stat.clauses, ty)?;
                for clause in &mut stat.clauses {
                    rec(&mut clause.body.statements, ty)?;
                }
            }
            Statement::Loop(stat) => {
                rec(&mut stat.body.statements, ty)?;
                eval_ty_attr(&mut stat.continuing, ty)?;
                if let Some(cont) = &mut stat.continuing {
                    rec(&mut cont.body.statements, ty)?;
                    eval_ty_attr(&mut cont.break_if, ty)?;
                }
                rec(&mut stat.body.statements, ty)?;
            }
            Statement::For(stat) => {
                if let Some(init) = &mut stat.initializer {
                    rec_one(&mut *init, ty)?
                }
                if let Some(updt) = &mut stat.update {
                    rec_one(&mut *updt, ty)?
                }
                rec(&mut stat.body.statements, ty)?
            }
            Statement::While(stat) => rec(&mut stat.body.statements, ty)?,
            _ => (),
        };
        Ok(())
    }
    fn rec(stats: &mut Vec<StatementNode>, ty: &TypeConstraint) -> Result<(), E> {
        eval_ty_attrs(stats, ty)?;
        for stat in stats {
            rec_one(stat, ty)?;
        }
        Ok(())
    }
    rec(statements, ty)
}
