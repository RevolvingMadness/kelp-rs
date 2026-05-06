use std::{iter::Map, slice::Iter};

type TakeSecondFn<A, B> = fn(&(A, B)) -> &B;

pub enum ParameterTypesIter<'a, P, T> {
    Regular(Map<Iter<'a, (P, T)>, TakeSecondFn<P, T>>),
    Builtin(Iter<'a, T>),
}

impl<'a, P, T> Iterator for ParameterTypesIter<'a, P, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::Regular(iterator) => iterator.next(),
            Self::Builtin(iterator) => iterator.next(),
        }
    }
}

#[inline]
pub const fn take_second<A, B>((_, second): &(A, B)) -> &B {
    second
}
