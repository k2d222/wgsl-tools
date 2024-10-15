use std::{
    cell::{Ref, RefCell, RefMut},
    collections::HashMap,
    ops::Deref,
    rc::Rc,
};

use derive_more::derive::{From, Unwrap};
use half::f16;
use itertools::Itertools;
use wgsl_parse::syntax::{AccessMode, AddressSpace};

use crate::eval::Ty;

use super::{EvalError, Type};

type E = EvalError;

#[derive(Clone, Debug, From, PartialEq, Unwrap)]
#[unwrap(ref, ref_mut)]
pub enum Instance {
    Literal(LiteralInstance),
    Struct(StructInstance),
    Array(ArrayInstance),
    Vec(VecInstance),
    Mat(MatInstance),
    Ptr(PtrInstance),
    Ref(RefInstance),
    Type(Type),
    Void,
}

impl Instance {
    pub fn view<'a>(&'a self, view: &'a MemView) -> Result<&Instance, E> {
        match view {
            MemView::Whole => Ok(self),
            MemView::Member(m, v) => match self {
                Instance::Struct(s) => {
                    let inst = s
                        .members
                        .get(m)
                        .ok_or_else(|| E::Component(s.ty(), m.clone()))?;
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
    pub fn view_mut<'a>(&'a mut self, view: &'a MemView) -> Result<&mut Instance, E> {
        let ty = self.ty();
        match view {
            MemView::Whole => Ok(self),
            MemView::Member(m, v) => match self {
                Instance::Struct(s) => {
                    let inst = s
                        .members
                        .get_mut(m)
                        .ok_or_else(|| E::Component(ty, m.clone()))?;
                    inst.view_mut(v)
                }
                _ => Err(E::Component(ty, m.clone())),
            },
            MemView::Index(i, v) => match self {
                Instance::Array(a) => {
                    let n = a.n();
                    let inst = a
                        .components
                        .get_mut(*i)
                        .ok_or_else(|| E::OutOfBounds(*i, ty, n))?;
                    inst.view_mut(v)
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
    pub fn len(&self) -> usize {
        match self {
            Instance::Array(a) => a.n(),
            Instance::Vec(v) => v.n() as usize,
            Instance::Mat(m) => m.c() as usize,
            _ => 0,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, From)]
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
    pub name: String,
    pub members: HashMap<String, Instance>,
}

impl StructInstance {
    pub fn name(&self) -> &str {
        &self.name
    }
}

#[derive(Clone, Debug, PartialEq, Default)]
pub struct ArrayInstance {
    components: Vec<Instance>,
}

impl ArrayInstance {
    ///
    /// # Panics
    /// * if the components is empty
    /// * if the components are not all the same type
    pub(crate) fn new(components: Vec<Instance>) -> Self {
        assert!(!components.is_empty());
        assert!(components.iter().map(|c| c.ty()).all_equal());
        Self { components }
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
        let components = ArrayInstance::new(components);
        assert!(components.ty().is_scalar());
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
}
impl IntoIterator for VecInstance {
    type Item = Instance;
    type IntoIter = <ArrayInstance as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.components.into_iter()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct MatInstance {
    components: Vec<Instance>,
}

impl MatInstance {
    /// # Panics
    /// * if the number of columns is not [2, 3, 4]
    /// * if the colums don't have the same number of rows
    /// * if the number of rows is not [2, 3, 4]
    /// * if the elements don't have the same type
    /// * if the type is not a scalar
    pub(crate) fn new(components: Vec<Instance>) -> Self {
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
        self.components.get(0).unwrap().unwrap_vec_ref().n()
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
    pub fn iter(&self) -> impl Iterator<Item = &Instance> {
        self.components.iter()
    }
    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut Instance> {
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
    pub ty: Type,
    pub access: AccessMode,
    pub space: AddressSpace,
    pub view: MemView,
    pub ptr: Rc<RefCell<Instance>>,
}

impl From<RefInstance> for PtrInstance {
    fn from(r: RefInstance) -> Self {
        Self {
            ty: r.ty.clone(),
            space: r.space,
            access: r.access,
            view: r.view.clone(),
            ptr: r.ptr.clone(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct RefInstance {
    pub ty: Type,
    pub space: AddressSpace,
    pub access: AccessMode,
    pub view: MemView,
    pub ptr: Rc<RefCell<Instance>>,
}

impl RefInstance {
    pub fn new(ptr: Rc<RefCell<Instance>>, space: AddressSpace, access: AccessMode) -> Self {
        let ty = ptr.borrow().ty();
        Self {
            ty,
            space,
            access,
            view: MemView::Whole,
            ptr,
        }
    }
}

impl From<PtrInstance> for RefInstance {
    fn from(p: PtrInstance) -> Self {
        Self {
            ty: p.ty.clone(),
            space: p.space,
            access: p.access,
            view: p.view.clone(),
            ptr: p.ptr.clone(),
        }
    }
}

pub struct RefView<'a> {
    r: Ref<'a, Instance>,
    v: &'a MemView,
}

impl<'a> Deref for RefView<'a> {
    type Target = Instance;

    fn deref(&self) -> &Self::Target {
        self.r.view(self.v).expect("invalid reference")
    }
}

pub struct RefViewMut<'a> {
    r: RefMut<'a, Instance>,
    v: &'a MemView,
}
impl<'a> RefViewMut<'a> {
    pub fn write(&mut self, value: Instance) -> Result<Instance, E> {
        let view = self.r.view_mut(self.v).expect("invalid reference");
        if value.ty() != view.ty() {
            return Err(E::WriteRefType(value.ty(), view.ty()));
        }
        let old = std::mem::replace(view, value);
        Ok(old)
    }
}
impl<'a> Deref for RefViewMut<'a> {
    type Target = Instance;

    fn deref(&self) -> &Self::Target {
        self.r.view(self.v).expect("invalid reference")
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
        let inst = self.ptr.borrow();
        let inst = inst.view(&self.view)?;
        Ok(Self {
            ty: inst.ty(),
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
        let inst = self.ptr.borrow();
        let inst = inst.view(&self.view)?;
        Ok(Self {
            ty: inst.ty(),
            space: self.space,
            access: self.access,
            view,
            ptr: self.ptr.clone(),
        })
    }

    pub fn read<'a>(&'a self) -> Result<RefView<'a>, E> {
        if !self.access.is_read() {
            Err(E::NotRead)
        } else {
            let r = self.ptr.borrow();
            Ok(RefView { r, v: &self.view })
        }
    }

    pub fn write(&mut self, value: Instance) -> Result<Instance, E> {
        if !self.access.is_write() {
            Err(E::NotWrite)
        } else {
            let mut r = self.ptr.borrow_mut();
            let view = r.view_mut(&self.view).expect("invalid reference");
            if value.ty() != view.ty() {
                return Err(E::WriteRefType(value.ty(), view.ty()));
            }
            let old = std::mem::replace(view, value);
            Ok(old)
        }
    }

    pub fn read_write<'a>(&'a mut self) -> Result<RefViewMut<'a>, E> {
        if !self.access.is_write() {
            Err(E::NotReadWrite)
        } else {
            let r = self.ptr.borrow_mut();
            Ok(RefViewMut { r, v: &self.view })
        }
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
