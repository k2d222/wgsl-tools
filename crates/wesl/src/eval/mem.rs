use half::f16;
use itertools::Itertools;

use super::{
    ArrayInstance, AtomicInstance, Context, EvalAttrs, EvalTy, Instance, LiteralInstance,
    MatInstance, StructInstance, SyntaxUtil, Ty, Type, VecInstance,
};

pub trait HostShareable: Ty + Sized {
    /// Returns the memory for host-shareable types
    /// Returns None if the type is not host-shareable
    fn to_buffer(&self, ctx: &mut Context) -> Option<Vec<u8>>;
}

impl HostShareable for Instance {
    fn to_buffer(&self, ctx: &mut Context) -> Option<Vec<u8>> {
        match self {
            Instance::Literal(l) => l.to_buffer(ctx),
            Instance::Struct(s) => s.to_buffer(ctx),
            Instance::Array(a) => a.to_buffer(ctx),
            Instance::Vec(v) => v.to_buffer(ctx),
            Instance::Mat(m) => m.to_buffer(ctx),
            Instance::Ptr(_) => None,
            Instance::Ref(_) => None,
            Instance::Atomic(a) => a.inner().to_buffer(ctx),
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
                .get(..4)?
                .try_into()
                .ok()
                .map(|buf| LiteralInstance::I32(i32::from_le_bytes(buf)).into()),
            Type::U32 => buf
                .get(..4)?
                .try_into()
                .ok()
                .map(|buf| LiteralInstance::U32(u32::from_le_bytes(buf)).into()),
            Type::F32 => buf
                .get(..4)?
                .try_into()
                .ok()
                .map(|buf| LiteralInstance::F32(f32::from_le_bytes(buf)).into()),
            Type::F16 => buf
                .get(..2)?
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
                        let ty = m.ty.eval_ty(ctx).ok()?;
                        // handle the specific case of runtime-sized arrays.
                        // they can only be the last member of a struct.
                        let inst = if let Type::Array(None, _) = ty {
                            let buf = buf.get(offset as usize..)?;
                            Instance::from_buffer(buf, &ty, ctx)?
                        } else {
                            // TODO: handle errors, check valid size...
                            let size = m
                                .eval_size(ctx)
                                .ok()
                                .flatten()
                                .or_else(|| ty.size_of(ctx))?;
                            let align = m
                                .eval_align(ctx)
                                .ok()
                                .flatten()
                                .or_else(|| ty.align_of(ctx))?;
                            offset = round_up(align, offset);
                            let buf = buf.get(offset as usize..(offset + size) as usize)?;
                            offset += size;
                            Instance::from_buffer(buf, &ty, ctx)?
                        };
                        Some((m.ident.to_string(), inst))
                    })
                    .collect::<Option<Vec<_>>>()?;
                Some(StructInstance::new(s.clone(), members).into())
            }
            Type::Array(Some(n), ty) => {
                let mut offset = 0;
                let size = ty.size_of(ctx)?;
                let stride = round_up(ty.align_of(ctx)?, size);
                let mut comps = Vec::new();
                while comps.len() != *n {
                    let buf = buf.get(offset as usize..(offset + size) as usize)?;
                    offset += stride;
                    let inst = Instance::from_buffer(buf, ty, ctx)?;
                    comps.push(inst);
                }
                Some(ArrayInstance::new(comps, false).into())
            }
            Type::Array(None, ty) => {
                let mut offset = 0;
                let size = ty.size_of(ctx)?;
                let stride = round_up(ty.align_of(ctx)?, size);
                let n = buf.len() as u32 / stride;
                let comps = (0..n)
                    .map(|_| {
                        let buf = buf.get(offset as usize..(offset + size) as usize)?;
                        offset += stride;
                        Instance::from_buffer(buf, ty, ctx)
                    })
                    .collect::<Option<_>>()?;
                Some(ArrayInstance::new(comps, true).into())
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
                let col_ty = Type::Vec(*r, ty.clone());
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
            Type::Atomic(ty) => {
                let buf = buf.get(..4)?.try_into().ok()?;
                let inst = match &**ty {
                    Type::I32 => LiteralInstance::I32(i32::from_le_bytes(buf)).into(),
                    Type::U32 => LiteralInstance::U32(u32::from_le_bytes(buf)).into(),
                    _ => unreachable!("atomic type must be u32 or i32"),
                };
                Some(AtomicInstance::new(inst).into())
            }
            Type::Ptr(_, _) | Type::Texture(_) | Type::Void => None,
        }
    }
}

impl HostShareable for LiteralInstance {
    fn to_buffer(&self, _ctx: &mut Context) -> Option<Vec<u8>> {
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
    fn to_buffer(&self, ctx: &mut Context) -> Option<Vec<u8>> {
        let mut buf = Vec::new();
        let decl = ctx.source.decl_struct(self.name())?;
        for (i, (name, inst)) in self.iter_members().enumerate() {
            let ty = inst.ty();
            let len = buf.len() as u32;
            let m = decl.members.iter().find(|m| &*m.ident.name() == name)?;
            let size = m
                .eval_size(ctx)
                .ok()
                .flatten()
                .or_else(|| ty.min_size_of(ctx))?;

            // handle runtime-size arrays as last struct member
            let size = match inst {
                Instance::Array(a) if a.runtime_sized => {
                    (i == decl.members.len() - 1).then(|| a.n() as u32 * size)
                }
                _ => Some(size),
            }?;

            let align = m
                .eval_align(ctx)
                .ok()
                .flatten()
                .or_else(|| ty.align_of(ctx))?;
            let off = round_up(align, len);
            if off > len {
                buf.extend((len..off).map(|_| 0));
            }
            let mut bytes = inst.to_buffer(ctx)?;
            let bytes_len = bytes.len() as u32;
            if size > bytes_len {
                bytes.extend((bytes_len..size).map(|_| 0));
            }
            buf.extend(bytes);
        }
        Some(buf)
    }
}

impl HostShareable for ArrayInstance {
    fn to_buffer(&self, ctx: &mut Context) -> Option<Vec<u8>> {
        let mut buf = Vec::new();
        let ty = self.inner_ty();
        let size = ty.size_of(ctx)?;
        let stride = round_up(ty.align_of(ctx)?, size);
        for c in self.iter() {
            buf.extend(c.to_buffer(ctx)?);
            if stride > size {
                buf.extend((size..stride).map(|_| 0))
            }
        }
        Some(buf)
    }
}

impl HostShareable for VecInstance {
    fn to_buffer(&self, ctx: &mut Context) -> Option<Vec<u8>> {
        Some(
            self.iter()
                .flat_map(|v| v.to_buffer(ctx).unwrap(/* SAFETY: vector elements must be host-shareable */).into_iter())
                .collect_vec(),
        )
    }
}

impl HostShareable for MatInstance {
    fn to_buffer(&self, ctx: &mut Context) -> Option<Vec<u8>> {
        Some(
            self.iter_cols()
                .flat_map(|v| {
                    // SAFETY: vector elements must be host-shareable
                    let mut v_buf = v.to_buffer(ctx).unwrap();
                    let len = v_buf.len() as u32;
                    let align = v.ty().align_of(ctx).unwrap();
                    if len < align {
                        v_buf.extend((len..align).map(|_| 0));
                    }
                    v_buf.into_iter()
                })
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
            Type::Struct(s) => {
                let decl = ctx.source.decl_struct(s)?;
                let past_last_mem = decl
                    .members
                    .iter()
                    .map(|m| {
                        let ty = m.ty.eval_ty(ctx).ok()?;
                        // TODO: handle errors, check valid size...
                        let size = m
                            .eval_size(ctx)
                            .ok()
                            .flatten()
                            .or_else(|| ty.size_of(ctx))?;
                        let align = m
                            .eval_align(ctx)
                            .ok()
                            .flatten()
                            .or_else(|| ty.align_of(ctx))?;
                        Some((size, align))
                    })
                    .try_fold(0, |offset, mem| {
                        let (size, align) = mem?;
                        Some(round_up(align, offset) + size)
                    })?;
                Some(round_up(self.align_of(ctx)?, past_last_mem))
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
                let align = Type::Vec(*r, ty.clone()).align_of(ctx)?;
                Some(*c as u32 * align)
            }
            Type::Atomic(_) => Some(4),
            Type::Ptr(_, _) | Type::Texture(_) | Type::Void => None,
        }
    }

    pub fn min_size_of(&self, ctx: &mut Context) -> Option<u32> {
        match self {
            Type::Array(None, ty) => Some(round_up(ty.align_of(ctx)?, ty.size_of(ctx)?)),
            _ => self.size_of(ctx),
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
                let decl = ctx.source.decl_struct(s)?;
                decl.members
                    .iter()
                    .map(|m| {
                        let ty = m.ty.eval_ty(ctx).ok()?;
                        m.eval_align(ctx)
                            .ok()
                            .flatten()
                            .or_else(|| ty.align_of(ctx))
                    })
                    .try_fold(0, |a, b| Some(a.max(b?)))
            }
            Type::Array(_, ty) => ty.align_of(ctx),
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
            Type::Mat(_, r, ty) => Type::Vec(*r, ty.clone()).align_of(ctx),
            Type::Atomic(_) => Some(4),
            Type::Ptr(_, _) | Type::Texture(_) | Type::Void => None,
        }
    }
}
