use super::{
    builtin_ident, ArrayInstance, LiteralInstance, MatInstance, SamplerType, StructInstance,
    SyntaxUtil, TextureType, Ty, Type, VecInstance,
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
            .decl_struct(self.name())
            .expect("struct declaration not found");
        Ok(Expression::FunctionCall(FunctionCall {
            ty: TypeExpression::new(Ident::new(self.name().to_string())),
            arguments: decl
                .members
                .iter()
                .map(|m| {
                    self.member(&*m.ident.name())
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
            Type::Bool => Ok(TypeExpression::new(builtin_ident("bool").unwrap().clone())),
            Type::AbstractInt => Err(E::NotConstructible(Type::AbstractInt)),
            Type::AbstractFloat => Err(E::NotConstructible(Type::AbstractFloat)),
            Type::I32 => Ok(TypeExpression::new(builtin_ident("i32").unwrap().clone())),
            Type::U32 => Ok(TypeExpression::new(builtin_ident("u32").unwrap().clone())),
            Type::F32 => Ok(TypeExpression::new(builtin_ident("f32").unwrap().clone())),
            Type::F16 => Ok(TypeExpression::new(builtin_ident("f16").unwrap().clone())),
            Type::Struct(s) => Ok(TypeExpression::new(Ident::new(s.clone()))),
            Type::Array(Some(n), inner_ty) => {
                let mut ty = TypeExpression::new(builtin_ident("array").unwrap().clone());
                ty.template_args = Some(vec![
                    TemplateArg {
                        expression: inner_ty.to_expr(ctx)?.into(),
                    },
                    TemplateArg {
                        expression: Expression::Literal(LiteralExpression::AbstractInt(*n as i64))
                            .into(),
                    },
                ]);
                Ok(ty)
            }
            Type::Array(None, inner_ty) => {
                let mut ty = TypeExpression::new(builtin_ident("f16").unwrap().clone());
                ty.template_args = Some(vec![TemplateArg {
                    expression: inner_ty.to_expr(ctx)?.into(),
                }]);
                Ok(ty)
            }
            Type::Vec(n, inner_ty) => {
                let mut ty =
                    TypeExpression::new(builtin_ident(&format!("vec{n}")).unwrap().clone());
                ty.template_args = Some(vec![TemplateArg {
                    expression: inner_ty.to_expr(ctx)?.into(),
                }]);
                Ok(ty)
            }
            Type::Mat(c, r, inner_ty) => {
                let mut ty =
                    TypeExpression::new(builtin_ident(&format!("mat{c}x{r}")).unwrap().clone());
                ty.template_args = Some(vec![TemplateArg {
                    expression: inner_ty.to_expr(ctx)?.into(),
                }]);
                Ok(ty)
            }
            Type::Atomic(inner_ty) => {
                let mut ty = TypeExpression::new(builtin_ident("atomic").unwrap().clone());
                ty.template_args = Some(vec![TemplateArg {
                    expression: inner_ty.to_expr(ctx)?.into(),
                }]);
                Ok(ty)
            }
            Type::Ptr(space, inner_ty) => {
                let mut ty = TypeExpression::new(builtin_ident("ptr").unwrap().clone());
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
            Type::Texture(tex) => {
                let name = match tex {
                    TextureType::Sampled1D(_) => "texture_1d",
                    TextureType::Sampled2D(_) => "texture_2d",
                    TextureType::Sampled2DArray(_) => "texture_2d_array",
                    TextureType::Sampled3D(_) => "texture_3d",
                    TextureType::SampledCube(_) => "texture_cube",
                    TextureType::SampledCubeArray(_) => "texture_cube_array",
                    TextureType::Multisampled2D(_) => "texture_multisampled_2d",
                    TextureType::DepthMultisampled2D => "texture_depth_multisampled_2d",
                    TextureType::External => "texture_external",
                    TextureType::Storage1D(_, _) => "texture_storage_1d",
                    TextureType::Storage2D(_, _) => "texture_storage_2d",
                    TextureType::Storage2DArray(_, _) => "texture_storage_2d_array",
                    TextureType::Storage3D(_, _) => "texture_storage_3d",
                    TextureType::Depth2D => "texture_depth_2d",
                    TextureType::Depth2DArray => "texture_depth_2d_array",
                    TextureType::DepthCube => "texture_depth_cube",
                    TextureType::DepthCubeArray => "texture_depth_cube_array",
                };
                let mut ty = TypeExpression::new(builtin_ident(name).unwrap().clone());
                ty.template_args = match tex {
                    TextureType::Sampled1D(sampled)
                    | TextureType::Sampled2D(sampled)
                    | TextureType::Sampled2DArray(sampled)
                    | TextureType::Sampled3D(sampled)
                    | TextureType::SampledCube(sampled)
                    | TextureType::SampledCubeArray(sampled)
                    | TextureType::Multisampled2D(sampled) => Some(vec![TemplateArg {
                        expression: Expression::TypeOrIdentifier(TypeExpression::new(
                            builtin_ident(&sampled.to_string()).unwrap().clone(),
                        ))
                        .into(),
                    }]),
                    TextureType::DepthMultisampled2D => None,
                    TextureType::External => None,
                    TextureType::Storage1D(texel, access)
                    | TextureType::Storage2D(texel, access)
                    | TextureType::Storage2DArray(texel, access)
                    | TextureType::Storage3D(texel, access) => Some(vec![
                        TemplateArg {
                            expression: Expression::TypeOrIdentifier(TypeExpression::new(
                                builtin_ident(&texel.to_string()).unwrap().clone(),
                            ))
                            .into(),
                        },
                        TemplateArg {
                            expression: Expression::TypeOrIdentifier(TypeExpression::new(
                                builtin_ident(&access.to_string()).unwrap().clone(),
                            ))
                            .into(),
                        },
                    ]),
                    TextureType::Depth2D => None,
                    TextureType::Depth2DArray => None,
                    TextureType::DepthCube => None,
                    TextureType::DepthCubeArray => None,
                };
                Ok(ty)
            }
            Type::Sampler(sampler) => match sampler {
                SamplerType::Sampler => Ok(TypeExpression::new(
                    builtin_ident("sampler").unwrap().clone(),
                )),
                SamplerType::SamplerComparison => Ok(TypeExpression::new(
                    builtin_ident("sampler_comparison").unwrap().clone(),
                )),
            },
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
