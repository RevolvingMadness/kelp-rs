use std::{
    collections::{HashMap, hash_map::Values},
    slice::Iter,
};

use crate::typed::data_type::resolved::{FieldAccessType, DataType};

pub enum FieldTypesIter<'a> {
    Struct(Values<'a, String, DataType>),
    Tuple(Iter<'a, DataType>),
}

impl<'a> Iterator for FieldTypesIter<'a> {
    type Item = &'a DataType;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::Struct(iterator) => iterator.next(),
            Self::Tuple(iterator) => iterator.next(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StructId(pub u32);

impl From<RegularStructId> for StructId {
    fn from(value: RegularStructId) -> Self {
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
    Struct(RegularStructDeclaration),
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
    pub fn generic_types(&self) -> &[DataType] {
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

    #[must_use]
    pub const fn get_field_access_type(&self) -> FieldAccessType {
        match self {
            Self::Struct(_) => FieldAccessType::Name,
            Self::Tuple(_) => FieldAccessType::Index,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RegularStructId(pub u32);

#[derive(Debug, Clone)]
pub struct RegularStructDeclaration {
    pub name: String,
    pub generic_types: Vec<DataType>,
    pub field_types: HashMap<String, DataType>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TupleStructId(pub u32);

#[derive(Debug, Clone)]
pub struct TupleStructDeclaration {
    pub name: String,
    pub generic_types: Vec<DataType>,
    pub field_types: Vec<DataType>,
}
