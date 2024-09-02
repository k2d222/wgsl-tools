pub use wgsl_parse_macros::Visit;

pub trait VisitVec<'a, T>
where
    T: 'a,
{
    fn all(self) -> impl Iterator<Item = &'a T>;
}

impl<'a, T, I> VisitVec<'a, T> for I
where
    I: Iterator<Item = &'a Vec<T>>,
    T: 'a,
{
    fn all(self) -> impl Iterator<Item = &'a T> {
        self.map(|x| x.iter()).flatten()
    }
}
