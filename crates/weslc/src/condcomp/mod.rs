use std::collections::HashMap;

use itertools::Itertools;
use thiserror::Error;
use wgsl_parse::{syntax::*, Decorated};
use wgsl_parse_macros::query_mut;

#[derive(Clone, Debug, Error)]
pub enum CondcompError {
    #[error("invalid feature flag: `{0}`")]
    InvalidFeatureFlag(String),
    #[error("missing feature flag: `{0}`")]
    MissingFeatureFlag(String),
    #[error("invalid if attribute expression: `{0}`")]
    InvalidExpression(Expression),
}

type Features = HashMap<String, bool>;

const EXPR_TRUE: Expression = Expression::Literal(LiteralExpression::True);
const EXPR_FALSE: Expression = Expression::Literal(LiteralExpression::False);

pub fn eval_attr(expr: &Expression, features: &Features) -> Result<Expression, CondcompError> {
    match expr {
        Expression::Literal(lit) => match lit {
            LiteralExpression::True => Ok(expr.clone()),
            LiteralExpression::False => Ok(expr.clone()),
            _ => Err(CondcompError::InvalidExpression(expr.clone())),
        },
        Expression::Parenthesized(paren) => eval_attr(&paren.expression, features),
        Expression::Binary(binary) => {
            let left = eval_attr(&binary.left, features)?;
            let right = eval_attr(&binary.right, features)?;
            match &binary.operator {
                BinaryOperator::ShortCircuitOr => {
                    let expr = if left == EXPR_TRUE || right == EXPR_TRUE {
                        left
                    } else if left == EXPR_FALSE {
                        right
                    } else if right == EXPR_FALSE {
                        left
                    } else {
                        expr.clone()
                    };
                    Ok(expr)
                }
                BinaryOperator::ShortCircuitAnd => {
                    let expr = if left == EXPR_TRUE && right == EXPR_TRUE {
                        left
                    } else if left == EXPR_FALSE || right == EXPR_FALSE {
                        left
                    } else {
                        expr.clone()
                    };
                    Ok(expr)
                }
                _ => Err(CondcompError::InvalidExpression(expr.clone())),
            }
        }
        Expression::Identifier(ident) => {
            let feat = features.get(&ident.name);
            let expr = match feat {
                Some(true) => EXPR_TRUE.clone(),
                Some(false) => EXPR_FALSE.clone(),
                None => expr.clone(),
            };
            Ok(expr)
        }
        _ => Err(CondcompError::InvalidExpression(expr.clone())),
    }
}

fn eval_if_attr(
    opt_node: &mut Option<impl Decorated>,
    features: &Features,
) -> Result<(), CondcompError> {
    if let Some(node) = opt_node {
        let if_attr = node
            .attributes_mut()
            .iter_mut()
            .find_map(|attr| (attr.name == "if").then_some(attr.arguments.as_mut()?.first_mut()?));

        if let Some(if_attr) = if_attr {
            let expr = eval_attr(if_attr, features)?;
            let keep = !(expr == EXPR_FALSE);
            if keep {
                *if_attr = expr;
            } else {
                *opt_node = None;
            }
        }
    }
    Ok(())
}

fn eval_if_attributes(
    nodes: &mut Vec<impl Decorated>,
    features: &Features,
) -> Result<(), CondcompError> {
    let retains = nodes
        .iter()
        .map(|node| {
            let if_attr = node
                .attributes()
                .iter()
                .find_map(|attr| (attr.name == "if").then_some(attr.arguments.as_ref()?.first()?));

            if let Some(expr) = if_attr {
                eval_attr(expr, features)
            } else {
                Ok(EXPR_TRUE.clone())
            }
        })
        .collect::<Result<Vec<Expression>, CondcompError>>()?;

    let retains = nodes
        .iter_mut()
        .zip(retains.into_iter())
        .map(|(node, expr)| {
            let if_attr = node.attributes_mut().iter_mut().find_map(|attr| {
                (attr.name == "if").then_some(attr.arguments.as_mut()?.first_mut()?)
            });
            if let Some(if_attr) = if_attr {
                let keep = !(expr == EXPR_FALSE);
                *if_attr = expr;
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

fn statement_eval_if_attributes(
    statements: &mut Vec<Statement>,
    features: &HashMap<String, bool>,
) -> Result<(), CondcompError> {
    fn rec_one(stat: &mut Statement, feats: &HashMap<String, bool>) -> Result<(), CondcompError> {
        match stat {
            Statement::Compound(stat) => rec(&mut stat.statements, feats)?,
            Statement::If(stat) => {
                rec(&mut stat.if_clause.body.statements, feats)?;
                for elif in &mut stat.else_if_clauses {
                    rec(&mut elif.body.statements, feats)?;
                }
                if let Some(el) = &mut stat.else_clause {
                    rec(&mut el.body.statements, feats)?;
                }
            }
            Statement::Switch(stat) => {
                eval_if_attributes(&mut stat.clauses, feats)?;
                for clause in &mut stat.clauses {
                    rec(&mut clause.body.statements, feats)?;
                }
            }
            Statement::Loop(stat) => {
                rec(&mut stat.body.statements, feats)?;
                eval_if_attr(&mut stat.continuing, feats)?;
                if let Some(cont) = &mut stat.continuing {
                    rec(&mut cont.body.statements, feats)?;
                    eval_if_attr(&mut cont.break_if, feats)?;
                }
                rec(&mut stat.body.statements, feats)?;
            }
            Statement::For(stat) => {
                if let Some(init) = &mut stat.initializer {
                    rec_one(&mut *init, feats)?
                }
                if let Some(updt) = &mut stat.update {
                    rec_one(&mut *updt, feats)?
                }
                rec(&mut stat.body.statements, feats)?
            }
            Statement::While(stat) => rec(&mut stat.body.statements, feats)?,
            _ => (),
        };
        Ok(())
    }
    fn rec(stats: &mut Vec<Statement>, feats: &HashMap<String, bool>) -> Result<(), CondcompError> {
        eval_if_attributes(stats, feats)?;
        for stat in stats {
            rec_one(stat, feats)?;
        }
        Ok(())
    }
    rec(statements, features)
}

fn query_attributes(wesl: &mut TranslationUnit) -> impl Iterator<Item = &mut Vec<Attribute>> {
    query_mut!(wesl.{
        imports.[].attributes,
        global_directives.[].{
            GlobalDirective::Diagnostic.attributes,
            GlobalDirective::Enable.attributes,
            GlobalDirective::Requires.attributes,
        },
        global_declarations.[].{
            GlobalDeclaration::Declaration.attributes,
            GlobalDeclaration::TypeAlias.attributes,
            GlobalDeclaration::Struct.{
                attributes,
                members.[].attributes,
            },
            GlobalDeclaration::Function.{
                attributes,
                parameters.[].attributes,
                body.{ attributes, statements.[].(statement_query_attributes) }
            },
            GlobalDeclaration::ConstAssert.attributes,
        }
    })
}

fn statement_query_attributes(stat: &mut Statement) -> impl Iterator<Item = &mut Vec<Attribute>> {
    query_mut!(stat.{
        Statement::Compound.{ attributes, statements.[].(statement_query_attributes) },
        Statement::If.{
            attributes,
            else_if_clauses.[].body.statements.[].(statement_query_attributes),
            else_clause.[].body.statements.[].(statement_query_attributes),
        },
        Statement::Switch.{
            attributes,
            clauses.[].{ attributes, body.statements.[].(statement_query_attributes) },
        },
        Statement::Loop.{
            attributes,
            body.statements.[].(statement_query_attributes),
            continuing.[].{
                attributes,
                body.statements.[].(statement_query_attributes),
                break_if.[].{ attributes }
            },
        },
        Statement::For.{
            attributes,
            body.statements.[].(statement_query_attributes),
        },
        Statement::While.{
            attributes,
            body.statements.[].(statement_query_attributes),
        },
        Statement::Break.attributes,
        Statement::Continue.attributes,
        Statement::Return.attributes,
        Statement::Discard.attributes,
        Statement::FunctionCall.attributes,
        Statement::ConstAssert.attributes,
        Statement::Declaration.attributes,
    })
}

pub fn run(wesl: &mut TranslationUnit, features: &Features) -> Result<(), CondcompError> {
    // 1. evaluate all if attributes

    if cfg!(feature = "imports") {
        eval_if_attributes(&mut wesl.imports, features)?;
    }

    eval_if_attributes(&mut wesl.global_directives, features)?;
    eval_if_attributes(&mut wesl.global_declarations, features)?;

    let structs = wesl
        .global_declarations
        .iter_mut()
        .filter_map(|decl| match decl {
            wgsl_parse::syntax::GlobalDeclaration::Struct(decl) => Some(decl),
            _ => None,
        });
    for strukt in structs {
        eval_if_attributes(&mut strukt.members, features)?;
    }

    let functions = wesl
        .global_declarations
        .iter_mut()
        .filter_map(|decl| match decl {
            wgsl_parse::syntax::GlobalDeclaration::Function(decl) => Some(decl),
            _ => None,
        });
    for func in functions {
        eval_if_attributes(&mut func.parameters, features)?;
        statement_eval_if_attributes(&mut func.body.statements, features)?;
    }

    // 2. remove attributes that evaluate to true

    for attrs in query_attributes(wesl) {
        attrs.retain(|attr| {
            attr.name != "if" || {
                attr.arguments
                    .as_ref()
                    .and_then(|args| args.first())
                    .map(|expr| *expr != EXPR_TRUE)
                    .unwrap_or(true)
            }
        })
    }
    Ok(())
}
