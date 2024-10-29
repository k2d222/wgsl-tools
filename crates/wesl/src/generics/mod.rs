mod mangle;

use itertools::Itertools;
use thiserror::Error;
use wgsl_parse::{syntax::*, Decorated};

use crate::{attributes::statement_query_attributes, syntax_util::IterUses};

#[derive(Clone, Debug, Error)]
pub enum GenericsError {}

pub fn replace_ty(ty: &mut TypeExpression, old_name: &str, new_ty: &TypeExpression) {
    if &ty.name == old_name {
        *ty = new_ty.clone();
    }
    for ty in ty.uses_mut() {
        replace_ty(ty, old_name, new_ty);
    }
}

pub fn run(wesl: &mut TranslationUnit) -> Result<(), GenericsError> {
    let mut new_decls = Vec::new();
    for decl in &wesl.global_declarations {
        if let GlobalDeclaration::Function(decl) = decl {
            let ty_constrs = decl.attributes.iter().rev().filter_map(|attr| match attr {
                Attribute::Type(t) => Some(t),
                _ => None,
            });

            let variants = ty_constrs
                .map(|t| t.variants.iter().map(|v| (&t.name, v)))
                .multi_cartesian_product();

            for variant in variants {
                let mut decl = decl.clone();
                decl.attributes
                    .retain(|attr| !matches!(attr, Attribute::Type(_)));

                for (old_name, new_ty) in &variant {
                    for ty in decl.uses_mut() {
                        replace_ty(ty, old_name, new_ty);
                    }
                }

                let constraints = variant.iter().map(|&(name, ty)| {
                    let mut ty = ty.clone();
                    for (name2, ty2) in &variant {
                        replace_ty(&mut ty, name2, ty2);
                    }
                    TypeConstraint {
                        name: name.clone(),
                        variants: vec![ty],
                    }
                });

                for ty in constraints {
                    // this is future-proofing. We'll want to generate fewer variants in
                    // the future by grouping them.
                    eval_ty_attrs(&mut decl.parameters, &ty)?;
                    statement_eval_ty_attributes(&mut decl.body.statements, &ty)?;
                }

                for stat in &mut decl.body.statements {
                    for attrs in statement_query_attributes(stat) {
                        attrs.retain(|attr| match attr {
                            Attribute::Type(c) => !c.variants.is_empty(),
                            _ => true,
                        })
                    }
                }

                // let signature = decl.parameters.iter().map(|p| {
                //     let mut ty = p.ty.clone();
                //     for (old_name, new_name) in &variant {
                //         rename_ty(ty, old_name, new_name)
                //     }
                // })

                // decl.name = format!("{}{}", decl.name, signature);

                new_decls.push(decl.into());
            }
        }
    }

    // remove generic function declarations
    wesl.global_declarations.retain(|decl| matches!(decl, GlobalDeclaration::Function(f) if f.attributes.iter().find(|attr| attr.is_type()).is_some()));

    // add generic variants
    wesl.global_declarations.extend(new_decls);

    Ok(())
}

fn eval_ty_attr(
    opt_node: &mut Option<impl Decorated>,
    ty: &TypeConstraint,
) -> Result<(), GenericsError> {
    if let Some(node) = opt_node {
        let vars = node
            .attributes_mut()
            .iter_mut()
            .find_map(|attr| match attr {
                Attribute::Type(TypeConstraint { name, variants }) if name == &ty.name => {
                    Some(variants)
                }
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

fn eval_ty_attrs(
    nodes: &mut Vec<impl Decorated>,
    ty: &TypeConstraint,
) -> Result<(), GenericsError> {
    let retains = nodes
        .iter()
        .map(|node| {
            let vars = node.attributes().iter().find_map(|attr| match attr {
                Attribute::Type(TypeConstraint { name, variants }) if name == &ty.name => {
                    Some(variants)
                }
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
        .zip(retains.into_iter())
        .map(|(node, keep)| {
            let ty_attr = node
                .attributes_mut()
                .iter_mut()
                .find_map(|attr| match attr {
                    Attribute::Type(TypeConstraint { name, variants }) if name == &ty.name => {
                        Some(variants)
                    }
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

fn statement_eval_ty_attributes(
    statements: &mut Vec<StatementNode>,
    ty: &TypeConstraint,
) -> Result<(), GenericsError> {
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
    fn rec(stats: &mut Vec<StatementNode>, ty: &TypeConstraint) -> Result<(), GenericsError> {
        eval_ty_attrs(stats, ty)?;
        for stat in stats {
            rec_one(stat, ty)?;
        }
        Ok(())
    }
    rec(statements, ty)
}
