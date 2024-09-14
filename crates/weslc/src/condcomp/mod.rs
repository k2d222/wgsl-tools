use std::collections::HashMap;

use thiserror::Error;
use wgsl_parse::syntax::{BinaryOperator, Expression, LiteralExpression, TranslationUnit};

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

pub fn run(wesl: &mut TranslationUnit, features: &Features) -> Result<(), CondcompError> {
    if cfg!(feature = "imports") {
        let retains = wesl
            .imports
            .iter()
            .map(|import| {
                let if_attr = import.attributes.iter().find_map(|attr| {
                    (attr.name == "if").then_some(attr.arguments.as_ref()?.first()?)
                });

                if let Some(expr) = if_attr {
                    eval_attr(expr, features)
                } else {
                    Ok(EXPR_TRUE.clone())
                }
            })
            .collect::<Result<Vec<Expression>, CondcompError>>()?;

        let retains = wesl
            .imports
            .iter_mut()
            .zip(retains.into_iter())
            .map(|(import, expr)| {
                let if_attr = import.attributes.iter_mut().find_map(|attr| {
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
            .collect::<Vec<bool>>();

        let mut it = retains.iter();
        wesl.imports.retain(|_| *it.next().unwrap());
    }

    Ok(())
}
