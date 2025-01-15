use std::fmt::Display;

use itertools::Itertools;

use crate::eval::Ty;

use super::{
    ArrayInstance, AtomicInstance, Instance, LiteralInstance, MatInstance, MemView, PtrInstance,
    RefInstance, StructInstance, Type, VecInstance,
};

impl Display for Instance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instance::Literal(inst) => write!(f, "{inst}"),
            Instance::Struct(inst) => write!(f, "{inst}"),
            Instance::Array(inst) => write!(f, "{inst}"),
            Instance::Vec(inst) => write!(f, "{inst}"),
            Instance::Mat(inst) => write!(f, "{inst}"),
            Instance::Ptr(inst) => write!(f, "{inst}"),
            Instance::Ref(inst) => write!(f, "{inst}"),
            Instance::Atomic(inst) => write!(f, "{inst}"),
            Instance::Type(ty) => write!(f, "{ty}"),
            Instance::Void => write!(f, ""),
        }
    }
}

impl Display for LiteralInstance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LiteralInstance::Bool(lit) => write!(f, "{lit}"),
            LiteralInstance::AbstractInt(lit) => write!(f, "{lit}"),
            LiteralInstance::AbstractFloat(lit) => write!(f, "{lit:?}"), // using the Debug formatter to print the trailing .0 in floats representing integers. because format!("{}", 3.0f32) == "3"
            LiteralInstance::I32(lit) => write!(f, "{lit}i"),
            LiteralInstance::U32(lit) => write!(f, "{lit}u"),
            LiteralInstance::F32(lit) => write!(f, "{lit}f"),
            LiteralInstance::F16(lit) => write!(f, "{lit}h"),
        }
    }
}

impl Display for StructInstance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = self.ident();
        let comps = self
            .iter_members()
            .map(|(_, v)| format!("{v}"))
            .format(", ");
        write!(f, "{name}({comps})")
    }
}

impl Display for ArrayInstance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let comps = self.iter().format(", ");
        write!(f, "array({comps})")
    }
}

impl Display for VecInstance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let n = self.n();
        let comps = self.iter().format(", ");
        write!(f, "vec{n}({comps})")
    }
}

impl Display for MatInstance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let c = self.c();
        let r = self.r();
        let comps = (0..c).map(|i| self.col(i as usize).unwrap()).format(", ");
        write!(f, "mat{c}x{r}({comps})")
    }
}

impl Display for PtrInstance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let space = &self.ptr.space;
        let ty = &self.ptr.ty;
        let access = &self.ptr.access;
        let val = self.ptr.read().expect("invalid reference");
        write!(f, "ptr<{space}, {ty}, {access}>({val})")
    }
}

impl Display for RefInstance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let space = &self.space;
        let ty = &self.ty;
        let access = &self.access;
        let val = self.read().expect("invalid reference");
        write!(f, "ref<{space}, {ty}, {access}>({val})")
    }
}

impl Display for MemView {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn fmt_view(f: &mut std::fmt::Formatter<'_>, view: &MemView) -> std::fmt::Result {
            match view {
                MemView::Whole => Ok(()),
                MemView::Member(m, v) => {
                    write!(f, ":{m}")?;
                    fmt_view(f, v)
                }
                MemView::Index(i, v) => {
                    write!(f, ":{i}")?;
                    fmt_view(f, v)
                }
            }
        }

        fmt_view(f, &self)
    }
}

impl Display for AtomicInstance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ty = &self.inner_ty();
        let val = self.inner();
        write!(f, "atomic<{ty}>({val})")
    }
}

impl<'a> Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Bool => write!(f, "bool"),
            Type::AbstractInt => write!(f, "AbstractInt"),
            Type::AbstractFloat => write!(f, "AbstractFloat"),
            Type::I32 => write!(f, "i32"),
            Type::U32 => write!(f, "u32"),
            Type::F32 => write!(f, "f32"),
            Type::F16 => write!(f, "f16"),
            Type::Struct(name) => write!(f, "{name}"),
            Type::Array(Some(n), ty) => write!(f, "array<{ty}, {n}>"),
            Type::Array(None, ty) => write!(f, "array<{ty}>"),
            Type::Vec(n, ty) => write!(f, "vec{n}<{ty}>"),
            Type::Mat(m, n, ty) => write!(f, "mat{m}x{n}<{ty}>"),
            Type::Atomic(ty) => write!(f, "atomic<{ty}>"),
            Type::Ptr(a_s, ty) => write!(f, "ptr<{a_s}, {ty}>"),
            Type::Void => write!(f, ""),
        }
    }
}
