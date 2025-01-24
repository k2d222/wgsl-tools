use super::{
    name_to_builtin_ident, ArrayInstance, LiteralInstance, MatInstance, StructInstance, SyntaxUtil,
    Ty, Type, VecInstance, IDENT_ARRAY, IDENT_ATOMIC, IDENT_BOOL, IDENT_F16, IDENT_F32, IDENT_I32,
    IDENT_PTR, IDENT_U32,
};
use crate::eval::{Context, EvalError, Instance};
use wgsl_parse::{span::Spanned, syntax::*};

type E = EvalError;

/// Convert and instance to an Expression.
pub trait ToExpr {
    fn to_expr(&self, ctx: &Context) -> Result<Expression, E>;
}

impl ToExpr for Instance {
    fn to_expr(&self, ctx: &Context) -> Result<Expression, E> {
        match self {
            Instance::Literal(inst) => inst.to_expr(ctx),
            Instance::Struct(inst) => inst.to_expr(ctx),
            Instance::Array(inst) => inst.to_expr(ctx),
            Instance::Vec(inst) => inst.to_expr(ctx),
            Instance::Mat(inst) => inst.to_expr(ctx),
            Instance::Type(inst) => inst.to_expr(ctx),
            Instance::Atomic(_) | Instance::Ptr(_) | Instance::Ref(_) | Instance::Void => {
                Err(E::NotConstructible(self.ty()))
            }
        }
    }
}

impl ToExpr for LiteralInstance {
    fn to_expr(&self, _ctx: &Context) -> Result<Expression, E> {
        Ok(match self {
            LiteralInstance::Bool(lit) => LiteralExpression::Bool(*lit),
            LiteralInstance::AbstractInt(lit) => LiteralExpression::AbstractInt(*lit),
            LiteralInstance::AbstractFloat(lit) => LiteralExpression::AbstractFloat(*lit),
            LiteralInstance::I32(lit) => LiteralExpression::I32(*lit),
            LiteralInstance::U32(lit) => LiteralExpression::U32(*lit),
            LiteralInstance::F32(lit) => LiteralExpression::F32(*lit),
            LiteralInstance::F16(lit) => LiteralExpression::F16(lit.to_f32()),
        }
        .into())
    }
}

impl ToExpr for StructInstance {
    fn to_expr(&self, ctx: &Context) -> Result<Expression, E> {
        let decl = ctx
            .source
            .decl_struct(self.ident())
            .expect("struct declaration not found");
        Ok(Expression::FunctionCall(FunctionCall {
            ty: TypeExpression::new(self.ident().clone()),
            arguments: decl
                .members
                .iter()
                .map(|m| {
                    self.member(&m.ident)
                        .expect("struct member not found")
                        .to_expr(ctx)
                        .map(Spanned::from)
                })
                .collect::<Result<Vec<_>, _>>()?,
        }))
    }
}

impl ToExpr for ArrayInstance {
    fn to_expr(&self, ctx: &Context) -> Result<Expression, E> {
        Ok(Expression::FunctionCall(FunctionCall {
            ty: TypeExpression::new(Ident::new("array".to_string())),
            arguments: self
                .iter()
                .map(|c| c.to_expr(ctx).map(Spanned::from))
                .collect::<Result<Vec<_>, _>>()?,
        }))
    }
}

impl ToExpr for VecInstance {
    fn to_expr(&self, ctx: &Context) -> Result<Expression, E> {
        Ok(Expression::FunctionCall(FunctionCall {
            ty: TypeExpression::new(Ident::new(format!("vec{}", self.n()))),
            arguments: self
                .iter()
                .map(|c| c.to_expr(ctx).map(Spanned::from))
                .collect::<Result<Vec<_>, _>>()?,
        }))
    }
}

impl ToExpr for MatInstance {
    fn to_expr(&self, ctx: &Context) -> Result<Expression, E> {
        Ok(Expression::FunctionCall(FunctionCall {
            ty: TypeExpression::new(Ident::new(format!("mat{}x{}", self.c(), self.r()))),
            arguments: self
                .iter_cols()
                .map(|c| c.to_expr(ctx).map(Spanned::from))
                .collect::<Result<Vec<_>, _>>()?,
        }))
    }
}

impl ToExpr for Type {
    fn to_expr(&self, ctx: &Context) -> Result<Expression, E> {
        match self {
            Type::Bool => Ok(TypeExpression::new(IDENT_BOOL.clone())),
            Type::AbstractInt => Err(E::NotConstructible(Type::AbstractInt)),
            Type::AbstractFloat => Err(E::NotConstructible(Type::AbstractFloat)),
            Type::I32 => Ok(TypeExpression::new(IDENT_I32.clone())),
            Type::U32 => Ok(TypeExpression::new(IDENT_U32.clone())),
            Type::F32 => Ok(TypeExpression::new(IDENT_F32.clone())),
            Type::F16 => Ok(TypeExpression::new(IDENT_F16.clone())),
            Type::Struct(s) => Ok(TypeExpression::new(s.clone())),
            Type::Array(Some(n), inner_ty) => {
                let mut ty = TypeExpression::new(IDENT_ARRAY.clone());
                ty.template_args = Some(vec![
                    TemplateArg {
                        expression: inner_ty.to_expr(ctx)?.into(),
                    },
                    TemplateArg {
                        expression: Expression::TypeOrIdentifier(Ident::new(n.to_string()).into())
                            .into(),
                    },
                ]);
                Ok(ty)
            }
            Type::Array(None, inner_ty) => {
                let mut ty = TypeExpression::new(IDENT_ARRAY.clone());
                ty.template_args = Some(vec![TemplateArg {
                    expression: inner_ty.to_expr(ctx)?.into(),
                }]);
                Ok(ty)
            }
            Type::Vec(n, inner_ty) => {
                let mut ty =
                    TypeExpression::new(name_to_builtin_ident(&format!("vec{n}")).unwrap());
                ty.template_args = Some(vec![TemplateArg {
                    expression: inner_ty.to_expr(ctx)?.into(),
                }]);
                Ok(ty)
            }
            Type::Mat(c, r, inner_ty) => {
                let mut ty =
                    TypeExpression::new(name_to_builtin_ident(&format!("mat{c}x{r}")).unwrap());
                ty.template_args = Some(vec![TemplateArg {
                    expression: inner_ty.to_expr(ctx)?.into(),
                }]);
                Ok(ty)
            }
            Type::Atomic(inner_ty) => {
                let mut ty = TypeExpression::new(IDENT_ATOMIC.clone());
                ty.template_args = Some(vec![TemplateArg {
                    expression: inner_ty.to_expr(ctx)?.into(),
                }]);
                Ok(ty)
            }
            Type::Ptr(space, inner_ty) => {
                let mut ty = TypeExpression::new(IDENT_PTR.clone());
                ty.template_args = Some(vec![
                    TemplateArg {
                        expression: space.to_expr(ctx)?.into(),
                    },
                    TemplateArg {
                        expression: inner_ty.to_expr(ctx)?.into(),
                    },
                ]);
                Ok(ty)
            }
            Type::Void => Err(E::NotConstructible(Type::Void)),
        }
        .map(Into::into)
    }
}

impl ToExpr for AddressSpace {
    fn to_expr(&self, _ctx: &Context) -> Result<Expression, E> {
        Ok(Expression::TypeOrIdentifier(TypeExpression::from(
            Ident::new(self.to_string()),
        )))
    }
}
