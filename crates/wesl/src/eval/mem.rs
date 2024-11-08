use std::collections::HashMap;

use half::f16;
use itertools::{ExactlyOneError, Itertools};
use wgsl_parse::syntax::TypeExpression;

use super::{
    ArrayInstance, Context, EvalTy, Instance, LiteralInstance, MatInstance, StructInstance,
    SyntaxUtil, Ty, Type, VecInstance,
};

pub trait HostShareable: Ty + Sized {
    /// Returns the memory for host-shareable types
    /// Returns None if the type is not host-shareable
    fn to_buffer(&self) -> Option<Vec<u8>>;
}

impl HostShareable for Instance {
    fn to_buffer(&self) -> Option<Vec<u8>> {
        match self {
            Instance::Literal(l) => l.to_buffer(),
            Instance::Struct(s) => s.to_buffer(),
            Instance::Array(a) => a.to_buffer(),
            Instance::Vec(v) => v.to_buffer(),
            Instance::Mat(m) => m.to_buffer(),
            Instance::Ptr(_) => None,
            Instance::Ref(_) => None,
            Instance::Type(_) => None,
            Instance::Void => None,
        }
    }
}

impl Instance {
    pub fn from_buffer(buf: &[u8], ty: &Type, ctx: &mut Context) -> Option<Self> {
        match ty {
            Type::Bool => None,
            Type::AbstractInt => None,
            Type::AbstractFloat => None,
            Type::I32 => buf
                .try_into()
                .ok()
                .map(|buf| LiteralInstance::I32(i32::from_le_bytes(buf)).into()),
            Type::U32 => buf
                .try_into()
                .ok()
                .map(|buf| LiteralInstance::U32(u32::from_le_bytes(buf)).into()),
            Type::F32 => buf
                .try_into()
                .ok()
                .map(|buf| LiteralInstance::F32(f32::from_le_bytes(buf)).into()),
            Type::F16 => buf
                .try_into()
                .ok()
                .map(|buf| LiteralInstance::F16(f16::from_le_bytes(buf)).into()),
            Type::Struct(s) => {
                let decl = ctx.source.decl_struct(s)?;
                let mut offset = 0;
                let members = decl
                    .members
                    .iter()
                    .map(|m| {
                        // handle the specific case of runtime-sized arrays.
                        // they can only be the last member of a struct.
                        let ty = m.ty.eval_ty(ctx).ok()?;
                        let inst = if let Type::Array(None, _) = ty {
                            let buf = buf.get(offset as usize..)?;
                            Instance::from_buffer(buf, &ty, ctx)?
                        } else {
                            let size = ty.size_of(ctx)?;
                            offset = round_up(ty.align_of(ctx)?, offset);
                            let buf = buf.get(offset as usize..(offset + size) as usize)?;
                            offset += size;
                            Instance::from_buffer(buf, &ty, ctx)?
                        };
                        Some((m.name.clone(), inst))
                    })
                    .collect::<Option<HashMap<_, _>>>()?;
                Some(StructInstance::new(s.to_string(), members).into())
            }
            Type::Array(Some(n), ty) => todo!(),
            Type::Array(None, ty) => {
                let mut offset = 0;
                let size = ty.size_of(ctx)?;
                let off = round_up(ty.align_of(ctx)?, size);
                let mut comps = Vec::new();
                while (offset as usize) < buf.len() {
                    let buf = buf.get(offset as usize..(offset + size) as usize)?;
                    offset += off;
                    let inst = Instance::from_buffer(buf, ty, ctx)?;
                    comps.push(inst);
                }
                Some(ArrayInstance::new(comps).into())
            }
            Type::Vec(n, ty) => {
                let mut offset = 0;
                let size = ty.size_of(ctx)?;
                let comps = (0..*n)
                    .map(|_| {
                        let buf = buf.get(offset as usize..(offset + size) as usize)?;
                        offset += size;
                        Instance::from_buffer(buf, ty, ctx)
                    })
                    .collect::<Option<Vec<_>>>()?;
                Some(VecInstance::new(comps).into())
            }
            Type::Mat(c, r, ty) => {
                let mut offset = 0;
                let col_ty = Type::Vec(*r, ty.clone().into());
                let col_size = col_ty.size_of(ctx)?;
                let col_off = round_up(col_ty.align_of(ctx)?, col_size);
                let cols = (0..*c)
                    .map(|_| {
                        let buf = buf.get(offset as usize..(offset + col_size) as usize)?;
                        offset += col_off;
                        Instance::from_buffer(buf, &col_ty, ctx)
                    })
                    .collect::<Option<Vec<_>>>()?;
                Some(MatInstance::from_cols(cols).into())
            }
            Type::Atomic(_) => todo!(),
            Type::Ptr(_, _) => None,
            Type::Void => None,
        }
    }
}

impl HostShareable for LiteralInstance {
    fn to_buffer(&self) -> Option<Vec<u8>> {
        match self {
            LiteralInstance::Bool(_) => None,
            LiteralInstance::AbstractInt(_) => None,
            LiteralInstance::AbstractFloat(_) => None,
            LiteralInstance::I32(n) => Some(n.to_le_bytes().to_vec()),
            LiteralInstance::U32(n) => Some(n.to_le_bytes().to_vec()),
            LiteralInstance::F32(n) => Some(n.to_le_bytes().to_vec()),
            LiteralInstance::F16(n) => Some(n.to_le_bytes().to_vec()),
        }
    }
}

// TODO: layout
impl HostShareable for StructInstance {
    fn to_buffer(&self) -> Option<Vec<u8>> {
        todo!()
    }
}

impl HostShareable for ArrayInstance {
    fn to_buffer(&self) -> Option<Vec<u8>> {
        todo!()
    }
}

impl HostShareable for VecInstance {
    fn to_buffer(&self) -> Option<Vec<u8>> {
        Some(
            self.iter()
                .flat_map(|v| v.to_buffer().unwrap(/* SAFETY: vector elements must be host-shareable */).into_iter())
                .collect_vec(),
        )
    }
}

impl HostShareable for MatInstance {
    fn to_buffer(&self) -> Option<Vec<u8>> {
        Some(
            self.iter()
                .flat_map(|v| v.to_buffer().unwrap(/* SAFETY: matrix elements must be host-shareable */).into_iter())
                .collect_vec(),
        )
    }
}

fn round_up(align: u32, size: u32) -> u32 {
    ((size + align - 1) / align) * align
}

impl Type {
    pub fn size_of(&self, ctx: &mut Context) -> Option<u32> {
        match self {
            Type::Bool => None,
            Type::AbstractInt => None,
            Type::AbstractFloat => None,
            Type::I32 => Some(4),
            Type::U32 => Some(4),
            Type::F32 => Some(4),
            Type::F16 => Some(2),
            Type::Struct(_s) => {
                todo!("size_of struct")
                // TODO implement size attribute
                // let decl = ctx.source.decl_struct(s)?;
                // Some(
                //     decl.members
                //         .iter()
                //         .filter_map(|m| m.ty.eval_ty(ctx).ok())
                //         .filter_map(|ty| Some((ty.size_of(ctx)?, ty.align_of(ctx)?)))
                //         .fold(0, |offset, (size, align)| round_up(align, offset + size)),
                // )
            }
            Type::Array(Some(n), ty) => {
                let (size, align) = (ty.size_of(ctx)?, ty.align_of(ctx)?);
                Some(*n as u32 * round_up(align, size))
            }
            Type::Array(None, _) => None,
            Type::Vec(n, ty) => {
                let size = ty.size_of(ctx)?;
                Some(*n as u32 * size)
            }
            Type::Mat(c, r, ty) => {
                let size = ty.size_of(ctx)?;
                Some(*c as u32 * *r as u32 * size)
            }
            Type::Atomic(_) => Some(4),
            Type::Ptr(_, _) => None,
            Type::Void => None,
        }
    }

    pub fn align_of(&self, ctx: &mut Context) -> Option<u32> {
        match self {
            Type::Bool => None,
            Type::AbstractInt => None,
            Type::AbstractFloat => None,
            Type::I32 => Some(4),
            Type::U32 => Some(4),
            Type::F32 => Some(4),
            Type::F16 => Some(2),
            Type::Struct(s) => {
                // TODO implement align attribute
                let decl = ctx.source.decl_struct(s)?;
                decl.members
                    .iter()
                    .map(|m| {
                        let ty = m.ty.eval_ty(ctx).ok()?;
                        ty.align_of(ctx)
                    })
                    .fold(Some(0), |a, b| Some(a?.max(b?)))
            }
            Type::Array(Some(n), ty) => {
                let (size, align) = (ty.size_of(ctx)?, ty.align_of(ctx)?);
                Some(*n as u32 * round_up(align, size))
            }
            Type::Array(None, _) => None,
            Type::Vec(n, ty) => {
                if *n == 3 {
                    match **ty {
                        Type::I32 | Type::U32 | Type::F32 => Some(16),
                        Type::F16 => Some(8),
                        _ => None,
                    }
                } else {
                    self.size_of(ctx)
                }
            }
            Type::Mat(c, r, ty) => Some(
                *c as u32
                    * *r as u32
                    * ty.size_of(ctx).unwrap(/* SAFETY: matrix elements must be host-shareable */),
            ),
            Type::Atomic(_) => Some(4),
            Type::Ptr(_, _) => None,
            Type::Void => None,
        }
    }
}
