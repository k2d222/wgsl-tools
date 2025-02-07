use wgsl_parse::{
    syntax::{Attribute, Expression},
    Decorated,
};

use super::{with_stage, Context, Eval, EvalError, EvalStage, Instance, LiteralInstance, Ty, Type};

type E = EvalError;

pub trait EvalAttrs: Decorated {
    fn attr_align(&self, ctx: &mut Context) -> Result<Option<u32>, E> {
        attr_align(self.attributes(), ctx).transpose()
    }
    fn attr_group_binding(&self, ctx: &mut Context) -> Result<(u32, u32), E> {
        attr_group_binding(self.attributes(), ctx)
    }
    fn attr_size(&self, ctx: &mut Context) -> Result<Option<u32>, E> {
        attr_size(self.attributes(), ctx).transpose()
    }
    fn attr_id(&self, ctx: &mut Context) -> Result<Option<u32>, E> {
        attr_id(self.attributes(), ctx).transpose()
    }
    fn attr_location(&self, ctx: &mut Context) -> Result<Option<u32>, E> {
        attr_location(self.attributes(), ctx).transpose()
    }
    fn attr_workgroup_size(&self, ctx: &mut Context) -> Result<(u32, Option<u32>, Option<u32>), E> {
        attr_workgroup_size(self.attributes(), ctx)
    }
    fn attr_blend_src(&self, ctx: &mut Context) -> Result<Option<bool>, E> {
        attr_blend_src(self.attributes(), ctx).transpose()
    }
}

impl<T: Decorated> EvalAttrs for T {}
fn eval_positive_integer(expr: &Expression, ctx: &mut Context) -> Result<u32, E> {
    let expr = with_stage!(ctx, EvalStage::Const, { expr.eval_value(ctx) })?;
    let expr = match expr {
        Instance::Literal(g) => match g {
            LiteralInstance::AbstractInt(g) => Ok(g),
            LiteralInstance::I32(g) => Ok(g as i64),
            LiteralInstance::U32(g) => Ok(g as i64),
            _ => Err(E::Type(Type::U32, g.ty())),
        },
        _ => Err(E::Type(Type::U32, expr.ty())),
    }?;
    if expr < 0 {
        Err(E::NegativeAttr(expr))
    } else {
        Ok(expr as u32)
    }
}

fn attr_group_binding(attrs: &[Attribute], ctx: &mut Context) -> Result<(u32, u32), E> {
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

fn attr_size(attrs: &[Attribute], ctx: &mut Context) -> Option<Result<u32, E>> {
    let expr = attrs.iter().find_map(|attr| match attr {
        Attribute::Size(e) => Some(e),
        _ => None,
    })?;

    Some(eval_positive_integer(expr, ctx))
}

fn attr_align(attrs: &[Attribute], ctx: &mut Context) -> Option<Result<u32, E>> {
    let expr = attrs.iter().find_map(|attr| match attr {
        Attribute::Align(e) => Some(e),
        _ => None,
    })?;

    Some(eval_positive_integer(expr, ctx))
}

fn attr_id(attrs: &[Attribute], ctx: &mut Context) -> Option<Result<u32, E>> {
    let expr = attrs.iter().find_map(|attr| match attr {
        Attribute::Id(e) => Some(e),
        _ => None,
    })?;

    Some(eval_positive_integer(expr, ctx))
}

fn attr_location(attrs: &[Attribute], ctx: &mut Context) -> Option<Result<u32, E>> {
    let expr = attrs.iter().find_map(|attr| match attr {
        Attribute::Location(e) => Some(e),
        _ => None,
    })?;

    Some(eval_positive_integer(expr, ctx))
}

fn attr_workgroup_size(
    attrs: &[Attribute],
    ctx: &mut Context,
) -> Result<(u32, Option<u32>, Option<u32>), E> {
    let attr = attrs
        .iter()
        .find_map(|attr| match attr {
            Attribute::WorkgroupSize(attr) => Some(attr),
            _ => None,
        })
        .ok_or(E::MissingWorkgroupSize)?;

    let x = eval_positive_integer(&attr.x, ctx)?;
    let y = attr
        .y
        .as_ref()
        .map(|y| eval_positive_integer(y, ctx))
        .transpose()?;
    let z = attr
        .z
        .as_ref()
        .map(|z| eval_positive_integer(z, ctx))
        .transpose()?;
    Ok((x, y, z))
}

fn attr_blend_src(attrs: &[Attribute], ctx: &mut Context) -> Option<Result<bool, E>> {
    let expr = attrs.iter().find_map(|attr| match attr {
        Attribute::BlendSrc(attr) => Some(attr),
        _ => None,
    })?;
    Some(eval_positive_integer(expr, ctx).and_then(|val| match val {
        0 => Ok(false),
        1 => Ok(true),
        _ => Err(E::InvalidBlendSrc(val)),
    }))
}
