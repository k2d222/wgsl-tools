use wgsl_parse::syntax::{Attribute, Expression};

use super::{with_stage, Context, Eval, EvalError, EvalStage, Instance, LiteralInstance, Ty, Type};

type E = EvalError;

fn eval_positive_integer(expr: &Expression, ctx: &mut Context) -> Result<u32, E> {
    let expr = with_stage!(ctx, EvalStage::Const, { expr.eval_value(ctx) })?;
    let expr = match expr {
        Instance::Literal(g) => match g {
            LiteralInstance::AbstractInt(g) => Ok(g as i64),
            LiteralInstance::I32(g) => Ok(g as i64),
            LiteralInstance::U32(g) => Ok(g as i64),
            _ => Err(E::Type(Type::U32, g.ty())),
        },
        _ => Err(E::Type(Type::U32, expr.ty())),
    }?;
    if expr < 0 {
        Err(E::BindNegative(expr))
    } else {
        Ok(expr as u32)
    }
}

pub fn eval_group_binding(attrs: &[Attribute], ctx: &mut Context) -> Result<(u32, u32), E> {
    let group = attrs.iter().find_map(|attr| match attr {
        Attribute::Group(g) => Some(g),
        _ => None,
    });
    let binding = attrs.iter().find_map(|attr| match attr {
        Attribute::Binding(b) => Some(b),
        _ => None,
    });

    let (group, binding) = match (group, binding) {
        (Some(g), Some(b)) => Ok((
            eval_positive_integer(g, ctx)?,
            eval_positive_integer(b, ctx)?,
        )),
        _ => Err(E::MissingBindAttr),
    }?;
    Ok((group, binding))
}
