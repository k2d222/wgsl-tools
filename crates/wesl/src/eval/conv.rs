use half::f16;
use itertools::Itertools;
use num_traits::{FromPrimitive, ToPrimitive};

use super::{
    ArrayInstance, Instance, LiteralInstance, MatInstance, SyntaxUtil, Ty, Type, VecInstance,
    PRELUDE,
};

pub trait Convert: Sized + Clone + Ty {
    /// convert an instance to another type, if a feasible conversion exists.
    /// reference: https://www.w3.org/TR/WGSL/#conversion-rank
    /// TODO: check that the `as` cast conversions are correct.
    fn convert_to(&self, ty: &Type) -> Option<Self>;

    /// convert an instance by changing its inner type to another.
    /// see [`Ty::inner_ty`]
    /// see [`Convert::convert_to`]
    fn convert_inner_to(&self, ty: &Type) -> Option<Self> {
        self.convert_to(ty)
    }

    /// convert an abstract instance to a concrete type.
    ///
    /// e.g. `array<vec<AbstractInt>>` becomes `array<vec<i32>>`
    fn concretize(&self) -> Option<Self> {
        self.convert_to(&self.ty().concretize())
    }
}

impl Type {
    pub fn concretize(&self) -> Type {
        match self {
            Type::AbstractInt => Type::I32,
            Type::AbstractFloat => Type::F32,
            Type::Array(n, ty) => Type::Array(*n, ty.concretize().into()),
            Type::Vec(n, ty) => Type::Vec(*n, ty.concretize().into()),
            Type::Mat(c, r, ty) => Type::Mat(*c, *r, ty.concretize().into()),
            _ => self.clone(),
        }
    }

    pub fn is_convertible_to(&self, ty: &Type) -> bool {
        conversion_rank(self, ty).is_some()
    }
}

impl LiteralInstance {
    fn is_infinite(&self) -> bool {
        match self {
            LiteralInstance::Bool(_) => false,
            LiteralInstance::AbstractInt(_) => false,
            LiteralInstance::AbstractFloat(n) => n.is_infinite(),
            LiteralInstance::I32(_) => false,
            LiteralInstance::U32(_) => false,
            LiteralInstance::F32(n) => n.is_infinite(),
            LiteralInstance::F16(n) => n.is_infinite(),
        }
    }
    fn is_finite(&self) -> bool {
        !self.is_infinite()
    }
}

impl Convert for LiteralInstance {
    fn convert_to(&self, ty: &Type) -> Option<Self> {
        if ty == &self.ty() {
            return Some(self.clone());
        }

        // TODO: check that these conversions are correctly implemented.
        // I think they are correct.
        // reference: https://www.w3.org/TR/WGSL/#floating-point-conversion
        match (self, ty) {
            (Self::AbstractInt(n), Type::AbstractFloat) => n.to_f64().map(Self::AbstractFloat),
            (Self::AbstractInt(n), Type::I32) => n.to_i32().map(Self::I32),
            (Self::AbstractInt(n), Type::U32) => n.to_u32().map(Self::U32),
            (Self::AbstractInt(n), Type::F32) => n.to_f32().map(Self::F32),
            (Self::AbstractInt(n), Type::F16) => f16::from_i64(*n).map(Self::F16),
            (Self::AbstractFloat(n), Type::F32) => n.to_f32().map(Self::F32),
            (Self::AbstractFloat(n), Type::F16) => Some(f16::from_f64(*n)).map(Self::F16),
            _ => None,
        }
        .and_then(|n| n.is_finite().then_some(n))
    }
}

impl Convert for ArrayInstance {
    fn convert_to(&self, ty: &Type) -> Option<Self> {
        if let Type::Array(n, c_ty) = ty {
            if *n == self.n() {
                self.convert_inner_to(c_ty)
            } else {
                None
            }
        } else {
            None
        }
    }
    fn convert_inner_to(&self, ty: &Type) -> Option<Self> {
        let components = self
            .iter()
            .map(|c| c.convert_to(ty))
            .collect::<Option<Vec<_>>>()?;
        Some(ArrayInstance::new(components))
    }
}

impl Convert for VecInstance {
    fn convert_to(&self, ty: &Type) -> Option<Self> {
        if let Type::Vec(n, c_ty) = ty {
            if *n as usize == self.n() {
                self.convert_inner_to(c_ty)
            } else {
                None
            }
        } else {
            None
        }
    }
    fn convert_inner_to(&self, ty: &Type) -> Option<Self> {
        let components = self
            .iter()
            .map(|c| c.convert_to(ty))
            .collect::<Option<Vec<_>>>()?;
        Some(VecInstance::new(components))
    }
}

impl Convert for MatInstance {
    fn convert_to(&self, ty: &Type) -> Option<Self> {
        if let Type::Mat(c, r, c_ty) = ty {
            if *c as usize == self.c() && *r as usize == self.r() {
                self.convert_inner_to(c_ty)
            } else {
                None
            }
        } else {
            None
        }
    }
    fn convert_inner_to(&self, ty: &Type) -> Option<Self> {
        let components = self
            .iter()
            .map(|c| c.convert_inner_to(ty))
            .collect::<Option<Vec<_>>>()?;
        Some(MatInstance::new(components))
    }
}

impl Convert for Instance {
    fn convert_to(&self, ty: &Type) -> Option<Self> {
        if &self.ty() == ty {
            return Some(self.clone());
        }
        match self {
            Self::Literal(l) => Some(Self::Literal(l.convert_to(ty)?)),
            Self::Struct(_) => None,
            Self::Array(a) => a.convert_to(ty).map(Self::Array),
            Self::Vec(v) => v.convert_to(ty).map(Self::Vec),
            Self::Mat(m) => m.convert_to(ty).map(Self::Mat),
            Self::Ptr(_) => None,
            Self::Ref(_) => None, // conversion from Ref<T> to T exists, but is not handled here.
            Self::Type(_) => None, // TODO: should it be converted?
            Self::Void => None,
        }
    }
}

pub fn conversion_rank(ty1: &Type, ty2: &Type) -> Option<u32> {
    // reference: https://www.w3.org/TR/WGSL/#conversion-rank
    match (ty1, ty2) {
        (_, _) if ty1 == ty2 => Some(0),
        (Type::AbstractInt, Type::AbstractFloat) => Some(5),
        (Type::AbstractInt, Type::I32) => Some(3),
        (Type::AbstractInt, Type::U32) => Some(4),
        (Type::AbstractInt, Type::F32) => Some(6),
        (Type::AbstractInt, Type::F16) => Some(7),
        (Type::AbstractFloat, Type::F32) => Some(1),
        (Type::AbstractFloat, Type::F16) => Some(2),
        (Type::Struct(s1), Type::Struct(s2)) => {
            if PRELUDE.decl_struct(&s1).is_some()
                && PRELUDE.decl_struct(&s2).is_some()
                && s1.ends_with("abstract")
            {
                if s2.ends_with("f32") {
                    Some(1)
                } else if s2.ends_with("f16") {
                    Some(2)
                } else {
                    None
                }
            } else {
                None
            }
        }
        (Type::Array(n1, ty1), Type::Array(n2, ty2)) if n1 == n2 => conversion_rank(ty1, ty2),
        (Type::Vec(n1, ty1), Type::Vec(n2, ty2)) if n1 == n2 => conversion_rank(ty1, ty2),
        (Type::Mat(c1, r1, ty1), Type::Mat(c2, r2, ty2)) if c1 == c2 && r1 == r2 => {
            conversion_rank(ty1, ty2)
        }
        _ => None,
    }
}

/// performs overload resolution when two instances of T are involved (which is the most common).
/// it just makes sure that the two types are the same, by lowering one of the instances.
/// this is sufficient in most cases.
/// TODO: check that it is sufficient
pub fn convert<T: Convert + Ty + Clone>(i1: &T, i2: &T) -> Option<(T, T)> {
    let (ty1, ty2) = (i1.ty(), i2.ty());
    let ty = convert_ty(&ty1, &ty2)?;
    let i1 = i1.convert_to(&ty)?;
    let i2 = i2.convert_to(&ty)?;
    Some((i1, i2))
}

pub fn convert_inner<T1: Convert + Ty + Clone, T2: Convert + Ty + Clone>(
    i1: &T1,
    i2: &T2,
) -> Option<(T1, T2)> {
    let (ty1, ty2) = (i1.ty(), i2.ty());
    let ty = convert_ty(&ty1, &ty2)?;
    let i1 = i1.convert_inner_to(&ty)?;
    let i2 = i2.convert_inner_to(&ty)?;
    Some((i1, i2))
}

/// See [`convert`]
pub fn convert_all<'a, T: Convert + Ty + Clone + 'a>(insts: &[T]) -> Option<Vec<T>> {
    let tys = insts.iter().map(|i| i.ty()).collect_vec();
    let ty = convert_ty_all(&tys)?;
    insts
        .into_iter()
        .map(|inst| inst.convert_to(ty))
        .collect::<Option<Vec<_>>>()
}

// performs overload resolution when two instances of T are involved (which is the most common).
// it just makes sure that the two types are the same.
// this is sufficient in most cases.
// TODO: check that it is sufficient
// TODO: find a better fn name
pub fn convert_ty<'a>(ty1: &'a Type, ty2: &'a Type) -> Option<&'a Type> {
    conversion_rank(ty1, ty2)
        .map(|_rank| ty2)
        .or_else(|| conversion_rank(ty2, ty1).map(|_rank| ty1))
}

// performs overload resolution when two instances of T are involved (which is the most common).
// it just makes sure that the two types are the same.
// this is sufficient in most cases.
// TODO: check that it is sufficient
// TODO: find a better fn name
pub fn convert_ty_all<'a>(tys: impl IntoIterator<Item = &'a Type> + 'a) -> Option<&'a Type> {
    tys.into_iter()
        .map(Option::Some)
        .reduce(|ty1, ty2| convert_ty(ty1?, ty2?))
        .flatten()
}
