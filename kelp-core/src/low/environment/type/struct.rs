use std::{
    collections::{HashMap, hash_map::Values},
    slice::Iter,
};

use crate::low::data_type::DataType;

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
    pub fn get_field(&self, field_name: &str) -> Option<&DataType> {
        match self {
            Self::Struct(declaration) => declaration.field_types.get(field_name),
            Self::Tuple(declaration) => {
                let field_index = field_name.parse::<usize>().ok()?;

                declaration.field_types.get(field_index)
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StructStructId(pub usize);

#[derive(Debug, Clone)]
pub struct StructStructDeclaration {
    pub name: String,
    pub generic_types: Vec<DataType>,
    pub field_types: HashMap<String, DataType>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TupleStructId(pub usize);

#[derive(Debug, Clone)]
pub struct TupleStructDeclaration {
    pub name: String,
    pub generic_types: Vec<DataType>,
    pub field_types: Vec<DataType>,
}
