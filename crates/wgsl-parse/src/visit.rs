pub use wgsl_parse_macros::Visit;

pub trait Visit {
    fn visit(&self) -> impl Iterator<Item = &Self> {
        std::iter::once(self)
    }
    fn visit_mut(&mut self) -> impl Iterator<Item = &mut Self> {
        std::iter::once(self)
    }
}

impl<T> Visit for Vec<T> {}

pub trait VisitVec<'a, T>
where
    T: 'a,
{
    fn each(self) -> impl Iterator<Item = &'a T>;
}

impl<'a, T, I> VisitVec<'a, T> for I
where
    I: Iterator<Item = &'a Vec<T>>,
    T: 'a,
{
    fn each(self) -> impl Iterator<Item = &'a T> {
        self.map(|x| x.iter()).flatten()
    }
}

pub trait VisitMutVec<'a, T>
where
    T: 'a,
{
    fn each(self) -> impl Iterator<Item = &'a mut T>;
}

impl<'a, T, I> VisitMutVec<'a, T> for I
where
    I: Iterator<Item = &'a mut Vec<T>>,
    T: 'a,
{
    fn each(self) -> impl Iterator<Item = &'a mut T> {
        self.map(|x| x.iter_mut()).flatten()
    }
}

impl<T> Visit for Option<T> {}

pub trait VisitOption<'a, T>
where
    T: 'a,
{
    fn some(self) -> impl Iterator<Item = &'a T>;
}

impl<'a, T, I> VisitOption<'a, T> for I
where
    I: Iterator<Item = &'a Option<T>>,
    T: 'a,
{
    fn some(self) -> impl Iterator<Item = &'a T> {
        self.map(|x| x.iter()).flatten()
    }
}

pub trait VisitMutOption<'a, T>
where
    T: 'a,
{
    fn some(self) -> impl Iterator<Item = &'a mut T>;
}

impl<'a, T, I> VisitMutOption<'a, T> for I
where
    I: Iterator<Item = &'a mut Option<T>>,
    T: 'a,
{
    fn some(self) -> impl Iterator<Item = &'a mut T> {
        self.map(|x| x.iter_mut()).flatten()
    }
}

// #[macro_export]
// macro_rules! visit_variants {
//     ($m:ident { $($pat:pat => $expr:expr),* $(,)? }) => {
//         {
//             let x: Box<dyn Iterator<Item = _>> = match $m {
//                 $(
//                     $pat => Box::new($expr),
//                 )*
//                 _ => Box::new(std::iter::empty()),
//             };
//             x
//         }
//     };
// }

// #[macro_export]
// macro_rules! visit_fields {
//     ({ $($expr:expr),* $(,)? }) => {
//         itertools::chain!(
//             $( $expr, )*
//         )
//     };
// }

#[macro_export]
macro_rules! visit_variants {
    { $($pat:pat => $expr:expr),* $(,)? } => {
        |x: &mut _| {
            let x: Box<dyn Iterator<Item = _>> = match x {
                $(
                    $pat => Box::new($expr),
                )*
                _ => Box::new(std::iter::empty()),
            };
            x
        }
    };
    ($m:ident, { $($pat:pat => $expr:expr),* $(,)? }) => {
        {
            let x: Box<dyn Iterator<Item = _>> = match $m {
                $(
                    $pat => Box::new($expr),
                )*
                _ => Box::new(std::iter::empty()),
            };
            x
        }
    };
}

#[macro_export]
macro_rules! visit_fields {
    { $($field:ident => $expr:expr),* $(,)? } => {
        |x: &mut _| {
            itertools::chain!(
                $( { let $field = &mut x.$field; $expr }, )*
            )
        }
    };
    ($m:ident, { $($field:ident => $expr:expr),* $(,)? }) => {
        itertools::chain!(
            $( { let $field = &mut $m.$field; $expr }, )*
        )
    };
}
