use std::{
    collections::{HashMap, hash_map::Values},
    slice::Iter,
};

use crate::low::data_type::resolved::ResolvedDataType;

pub enum FieldTypesIter<'a> {
    Struct(Values<'a, String, ResolvedDataType>),
    Tuple(Iter<'a, ResolvedDataType>),
}

impl<'a> Iterator for FieldTypesIter<'a> {
    type Item = &'a ResolvedDataType;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::Struct(iterator) => iterator.next(),
            Self::Tuple(iterator) => iterator.next(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StructId(pub usize);

impl From<StructStructId> for StructId {
    fn from(value: StructStructId) -> Self {
        Self(value.0)
    }
}

impl From<TupleStructId> for StructId {
    fn from(value: TupleStructId) -> Self {
        Self(value.0)
    }
}

#[derive(Debug, Clone)]
pub enum StructDeclaration {
    Struct(StructStructDeclaration),
    Tuple(TupleStructDeclaration),
}

impl StructDeclaration {
    #[must_use]
    pub fn name(&self) -> &str {
        match self {
            Self::Struct(declaration) => &declaration.name,
            Self::Tuple(declaration) => &declaration.name,
        }
    }

    #[must_use]
    pub fn generic_types(&self) -> &[ResolvedDataType] {
        match self {
            Self::Struct(declaration) => &declaration.generic_types,
            Self::Tuple(declaration) => &declaration.generic_types,
        }
    }

    #[must_use]
    pub fn field_types(&self) -> FieldTypesIter<'_> {
        match self {
            Self::Struct(declaration) => FieldTypesIter::Struct(declaration.field_types.values()),
            Self::Tuple(declaration) => FieldTypesIter::Tuple(declaration.field_types.iter()),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StructStructId(pub usize);

#[derive(Debug, Clone)]
pub struct StructStructDeclaration {
    pub name: String,
    pub generic_types: Vec<ResolvedDataType>,
    pub field_types: HashMap<String, ResolvedDataType>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TupleStructId(pub usize);

#[derive(Debug, Clone)]
pub struct TupleStructDeclaration {
    pub name: String,
    pub generic_types: Vec<ResolvedDataType>,
    pub field_types: Vec<ResolvedDataType>,
}
