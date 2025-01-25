use std::{
    cell::{Ref, RefCell, RefMut},
    ops::Index,
    rc::Rc,
};

use derive_more::derive::{From, IsVariant, Unwrap};
use half::f16;
use itertools::Itertools;
use wgsl_parse::syntax::{AccessMode, AddressSpace};

use crate::eval::Ty;

use super::{EvalError, Type};

type E = EvalError;

#[derive(Clone, Debug, From, PartialEq, IsVariant, Unwrap)]
#[unwrap(ref, ref_mut)]
pub enum Instance {
    Literal(LiteralInstance),
    Struct(StructInstance),
    Array(ArrayInstance),
    Vec(VecInstance),
    Mat(MatInstance),
    Ptr(PtrInstance),
    Ref(RefInstance),
    Atomic(AtomicInstance),
    Type(Type),
    Void,
}

impl Instance {
    pub fn view(&self, view: &MemView) -> Result<&Instance, E> {
        match view {
            MemView::Whole => Ok(self),
            MemView::Member(m, v) => match self {
                Instance::Struct(s) => {
                    let inst = s.member(m).ok_or_else(|| E::Component(s.ty(), m.clone()))?;
                    inst.view(v)
                }
                _ => Err(E::Component(self.ty(), m.clone())),
            },
            MemView::Index(i, view) => match self {
                Instance::Array(a) => {
                    let inst = a
                        .components
                        .get(*i)
                        .ok_or_else(|| E::OutOfBounds(*i, a.ty(), a.n()))?;
                    inst.view(view)
                }
                Instance::Vec(v) => {
                    let inst = v
                        .components
                        .get(*i)
                        .ok_or_else(|| E::OutOfBounds(*i, v.ty(), v.n()))?;
                    inst.view(view)
                }
                Instance::Mat(m) => {
                    let inst = m
                        .components
                        .get(*i)
                        .ok_or_else(|| E::OutOfBounds(*i, m.ty(), m.c()))?;
                    inst.view(view)
                }
                _ => Err(E::NotIndexable(self.ty())),
            },
        }
    }
    pub fn view_mut(&mut self, view: &MemView) -> Result<&mut Instance, E> {
        let ty = self.ty();
        match view {
            MemView::Whole => Ok(self),
            MemView::Member(m, v) => match self {
                Instance::Struct(s) => {
                    let inst = s.member_mut(m).ok_or_else(|| E::Component(ty, m.clone()))?;
                    inst.view_mut(v)
                }
                _ => Err(E::Component(ty, m.clone())),
            },
            MemView::Index(i, view) => match self {
                Instance::Array(a) => {
                    let n = a.n();
                    let inst = a
                        .components
                        .get_mut(*i)
                        .ok_or_else(|| E::OutOfBounds(*i, ty, n))?;
                    inst.view_mut(view)
                }
                Instance::Vec(v) => {
                    let n = v.n();
                    let inst = v
                        .components
                        .get_mut(*i)
                        .ok_or_else(|| E::OutOfBounds(*i, ty, n))?;
                    inst.view_mut(view)
                }
                Instance::Mat(m) => {
                    let c = m.c();
                    let inst = m
                        .components
                        .get_mut(*i)
                        .ok_or_else(|| E::OutOfBounds(*i, ty, c))?;
                    inst.view_mut(view)
                }
                _ => Err(E::NotIndexable(ty)),
            },
        }
    }
    pub fn write(&mut self, value: Instance) -> Result<Instance, E> {
        if value.ty() != self.ty() {
            return Err(E::WriteRefType(value.ty(), self.ty()));
        }
        let old = std::mem::replace(self, value);
        Ok(old)
    }
    pub fn len(&self) -> usize {
        match self {
            Instance::Array(a) => a.n(),
            Instance::Vec(v) => v.n(),
            Instance::Mat(m) => m.c(),
            _ => 0,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, From, Unwrap)]
pub enum LiteralInstance {
    Bool(bool),
    AbstractInt(i64),
    AbstractFloat(f64),
    I32(i32),
    U32(u32),
    F32(f32),
    F16(f16),
}

#[derive(Clone, Debug, PartialEq)]
pub struct StructInstance {
    name: String,
    members: Vec<(String, Instance)>,
}

impl StructInstance {
    pub fn new(name: String, members: Vec<(String, Instance)>) -> Self {
        Self { name, members }
    }
    pub fn name(&self) -> &str {
        &self.name
    }
    pub fn member(&self, name: &str) -> Option<&Instance> {
        self.members
            .iter()
            .find_map(|(n, inst)| (n == name).then_some(inst))
    }
    pub fn member_mut(&mut self, name: &str) -> Option<&mut Instance> {
        self.members
            .iter_mut()
            .find_map(|(n, inst)| (n == name).then_some(inst))
    }
    pub fn iter_members(&self) -> impl Iterator<Item = &(String, Instance)> {
        self.members.iter()
    }
}

#[derive(Clone, Debug, PartialEq, Default)]
pub struct ArrayInstance {
    components: Vec<Instance>,
    pub runtime_sized: bool,
}

impl ArrayInstance {
    ///
    /// # Panics
    /// * if the components is empty
    /// * if the components are not all the same type
    pub(crate) fn new(components: Vec<Instance>, runtime_sized: bool) -> Self {
        assert!(!components.is_empty());
        assert!(components.iter().map(|c| c.ty()).all_equal());
        Self {
            components,
            runtime_sized,
        }
    }
    pub fn n(&self) -> usize {
        self.components.len()
    }
    pub fn get(&self, i: usize) -> Option<&Instance> {
        self.components.get(i)
    }
    pub fn get_mut(&mut self, i: usize) -> Option<&mut Instance> {
        self.components.get_mut(i)
    }
    pub fn iter(&self) -> impl Iterator<Item = &Instance> {
        self.components.iter()
    }
    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut Instance> {
        self.components.iter_mut()
    }
    pub fn as_slice(&self) -> &[Instance] {
        self.components.as_slice()
    }
}
impl IntoIterator for ArrayInstance {
    type Item = Instance;
    type IntoIter = <Vec<Instance> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.components.into_iter()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct VecInstance {
    components: ArrayInstance,
}

impl VecInstance {
    /// # Panics
    /// * if the components length is not [2, 3, 4]
    /// * if the components are not all the same type
    /// * if the type is not a scalar
    pub(crate) fn new(components: Vec<Instance>) -> Self {
        assert!((2..=4).contains(&components.len()));
        let components = ArrayInstance::new(components, false);
        assert!(components.inner_ty().is_scalar());
        Self { components }
    }
    pub fn n(&self) -> usize {
        self.components.n()
    }
    pub fn get(&self, i: usize) -> Option<&Instance> {
        self.components.get(i)
    }
    pub fn get_mut(&mut self, i: usize) -> Option<&mut Instance> {
        self.components.get_mut(i)
    }
    pub fn iter(&self) -> impl Iterator<Item = &Instance> {
        self.components.iter()
    }
    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut Instance> {
        self.components.iter_mut()
    }
    pub fn as_slice(&self) -> &[Instance] {
        self.components.as_slice()
    }
}
impl IntoIterator for VecInstance {
    type Item = Instance;
    type IntoIter = <ArrayInstance as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.components.into_iter()
    }
}
impl Index<usize> for VecInstance {
    type Output = Instance;

    fn index(&self, index: usize) -> &Self::Output {
        self.get(index).unwrap()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct MatInstance {
    components: Vec<Instance>,
}

impl MatInstance {
    /// Constructor from column vectors.
    ///
    /// # Panics
    /// * if the number of columns is not [2, 3, 4]
    /// * if the colums don't have the same number of rows
    /// * if the number of rows is not [2, 3, 4]
    /// * if the elements don't have the same type
    /// * if the type is not a scalar
    pub(crate) fn from_cols(components: Vec<Instance>) -> Self {
        assert!((2..=4).contains(&components.len()));
        assert!(
            components
                .iter()
                .map(|c| c.unwrap_vec_ref().n())
                .all_equal(),
            "MatInstance columns must have the same number for rows"
        );
        assert!(
            components.iter().map(|c| c.ty()).all_equal(),
            "MatInstance columns must have the same type"
        );
        Self { components }
    }

    pub fn r(&self) -> usize {
        self.components.first().unwrap().unwrap_vec_ref().n()
    }
    pub fn c(&self) -> usize {
        self.components.len()
    }
    pub fn col(&self, i: usize) -> Option<&Instance> {
        self.components.get(i)
    }
    pub fn col_mut(&mut self, i: usize) -> Option<&mut Instance> {
        self.components.get_mut(i)
    }
    pub fn get(&self, i: usize, j: usize) -> Option<&Instance> {
        self.col(i).and_then(|v| v.unwrap_vec_ref().get(j))
    }
    pub fn get_mut(&mut self, i: usize, j: usize) -> Option<&mut Instance> {
        self.col_mut(i).and_then(|v| v.unwrap_vec_mut().get_mut(j))
    }
    pub fn iter_cols(&self) -> impl Iterator<Item = &Instance> {
        self.components.iter()
    }
    pub fn iter_cols_mut(&mut self) -> impl Iterator<Item = &mut Instance> {
        self.components.iter_mut()
    }
}
impl IntoIterator for MatInstance {
    type Item = Instance;
    type IntoIter = <Vec<Instance> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.components.into_iter()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct PtrInstance {
    pub ptr: RefInstance,
}

impl From<RefInstance> for PtrInstance {
    fn from(r: RefInstance) -> Self {
        Self { ptr: r }
    }
}

// #[derive(Clone, Debug, From, PartialEq)]
// pub struct Mem<T> {
//     bytes: Rc<RefCell<[u8]>>,
//     _ty: PhantomData<T>,
// }

// impl<T> Mem<T> {
//     pub fn get<'a>(&'a self) -> Ref<'a, T> {
//         Ref::<'_, [u8]>::map(self.bytes.borrow(), |bytes| unsafe {
//             std::ptr::read(bytes.as_ptr() as *const &T)
//         })
//     }

//     pub fn get_mut<'a>(&'a self) -> RefMut<'a, T> {
//         RefMut::<'_, [u8]>::map(self.bytes.borrow_mut(), |bytes| unsafe {
//             std::ptr::read(bytes.as_ptr() as *const &mut T)
//         })
//     }
// }

// impl<'a, T> Deref for Mem<T>
// where
//     Self: 'a,
// {
//     type Target = Ref<'a, T>;

//     fn deref(&'a self) -> &Self::Target {
//         &self.get()
//     }
// }

// impl<'a, T> AsRef<Ref<'a, T>> for Mem<T> {
//     fn as_ref(&self) -> &Ref<'a, T> {
//         todo!()
//     }
// }

// impl<T: Copy> Mem<T> {
//     pub fn val(&self) -> T {
//         *self.get()
//     }
// }

// #[derive(Clone, Debug, PartialEq)]
// pub enum RefData {
//     Internal {
//         ptr: Rc<RefCell<Instance>>,
//         view: MemView,
//     },
//     HostShared(Bytes),
// }

#[derive(Clone, Debug, PartialEq)]
pub struct RefInstance {
    pub ty: Type,
    pub space: AddressSpace,
    pub access: AccessMode,
    pub view: MemView,
    pub ptr: Rc<RefCell<Instance>>,
}

impl RefInstance {
    pub fn from_instance(inst: Instance, space: AddressSpace, access: AccessMode) -> Self {
        let ty = inst.ty();
        Self {
            ty,
            space,
            access,
            view: MemView::Whole,
            ptr: Rc::new(RefCell::new(inst)),
        }
    }
}

impl From<PtrInstance> for RefInstance {
    fn from(p: PtrInstance) -> Self {
        p.ptr
    }
}

impl RefInstance {
    /// get a reference to a struct or vec member
    pub fn view_member(&self, comp: String) -> Result<Self, E> {
        if !self.access.is_read() {
            return Err(E::NotRead);
        }
        let mut view = self.view.clone();
        view.append_member(comp);
        let ty = self.ptr.borrow().view(&view)?.ty();
        Ok(Self {
            ty,
            space: self.space,
            access: self.access,
            view,
            ptr: self.ptr.clone(),
        })
    }
    /// get a reference to an array, vec or mat component
    pub fn view_index(&self, index: usize) -> Result<Self, E> {
        if !self.access.is_read() {
            return Err(E::NotRead);
        }
        let mut view = self.view.clone();
        view.append_index(index);
        let ty = self.ptr.borrow().view(&view)?.ty();
        Ok(Self {
            ty,
            space: self.space,
            access: self.access,
            view,
            ptr: self.ptr.clone(),
        })
    }

    pub fn read<'a>(&'a self) -> Result<Ref<'a, Instance>, E> {
        if !self.access.is_read() {
            return Err(E::NotRead);
        }
        Ok(Ref::<'a, Instance>::map(self.ptr.borrow(), |r| {
            r.view(&self.view).expect("invalid reference")
        }))
    }

    pub fn write(&mut self, value: Instance) -> Result<(), E> {
        if !self.access.is_write() {
            return Err(E::NotWrite);
        }
        if value.ty() != self.ty() {
            return Err(E::WriteRefType(value.ty(), self.ty()));
        }
        let mut r = self.ptr.borrow_mut();
        let view = r.view_mut(&self.view).expect("invalid reference");
        assert!(view.ty() == value.ty());
        let _ = std::mem::replace(view, value);
        Ok(())
    }

    pub fn read_write<'a>(&'a mut self) -> Result<RefMut<'a, Instance>, E> {
        if !self.access.is_write() {
            return Err(E::NotReadWrite);
        }
        Ok(RefMut::<'a, Instance>::map(self.ptr.borrow_mut(), |r| {
            r.view_mut(&self.view).expect("invalid reference")
        }))
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum MemView {
    Whole,
    Member(String, Box<MemView>),
    Index(usize, Box<MemView>),
}

impl MemView {
    pub fn append_member(&mut self, comp: String) {
        match self {
            MemView::Whole => *self = MemView::Member(comp, Box::new(MemView::Whole)),
            MemView::Member(_, v) | MemView::Index(_, v) => v.append_member(comp),
        }
    }
    pub fn append_index(&mut self, index: usize) {
        match self {
            MemView::Whole => *self = MemView::Index(index, Box::new(MemView::Whole)),
            MemView::Member(_, v) | MemView::Index(_, v) => v.append_index(index),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct AtomicInstance {
    content: Box<Instance>,
}

impl AtomicInstance {
    /// # Panics
    /// * if the instance is not an i32 or u32
    pub fn new(inst: Instance) -> Self {
        assert!(matches!(inst.ty(), Type::I32 | Type::U32));
        Self {
            content: inst.into(),
        }
    }

    pub fn inner(&self) -> &Instance {
        &self.content
    }
}
