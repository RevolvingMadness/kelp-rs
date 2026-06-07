use std::{
    collections::{HashMap, hash_map::Values},
    slice::Iter,
};

use crate::{
    low::data_type::{DataType, FieldAccessType},
    make_id,
};

pub enum FieldTypesIter<'a> {
    Struct(Values<'a, String, DataType>),
    Tuple(Iter<'a, DataType>),
    Unit,
}

impl<'a> Iterator for FieldTypesIter<'a> {
    type Item = &'a DataType;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::Struct(iterator) => iterator.next(),
            Self::Tuple(iterator) => iterator.next(),
            Self::Unit => None,
        }
    }
}

make_id!(StructId);

#[derive(Debug, Clone)]
pub enum StructDeclaration {
    Struct(RegularStructDeclaration),
    Tuple(TupleStructDeclaration),
    Unit(UnitStructDeclaration),
}

impl StructDeclaration {
    #[must_use]
    pub fn name(&self) -> &str {
        match self {
            Self::Struct(declaration) => &declaration.name,
            Self::Tuple(declaration) => &declaration.name,
            Self::Unit(declaration) => &declaration.name,
        }
    }

    #[must_use]
    pub fn generic_types(&self) -> &[DataType] {
        match self {
            Self::Struct(declaration) => &declaration.generic_types,
            Self::Tuple(declaration) => &declaration.generic_types,
            Self::Unit(declaration) => &[],
        }
    }

    #[must_use]
    pub fn field_types(&self) -> FieldTypesIter<'_> {
        match self {
            Self::Struct(declaration) => FieldTypesIter::Struct(declaration.field_types.values()),
            Self::Tuple(declaration) => FieldTypesIter::Tuple(declaration.field_types.iter()),
            Self::Unit(..) => FieldTypesIter::Unit,
        }
    }

    #[must_use]
    pub const fn get_field_access_type(&self) -> FieldAccessType {
        match self {
            Self::Struct(_) => FieldAccessType::Name,
            Self::Tuple(_) => FieldAccessType::Index,
            Self::Unit(_) => FieldAccessType::Name,
        }
    }
}

make_id!(RegularStructId);

impl From<RegularStructId> for StructId {
    fn from(value: RegularStructId) -> Self {
        Self(value.0)
    }
}

#[derive(Debug, Clone)]
pub struct RegularStructDeclaration {
    pub name: String,
    pub generic_types: Vec<DataType>,
    pub field_types: HashMap<String, DataType>,
}

make_id!(TupleStructId);

impl From<TupleStructId> for StructId {
    fn from(value: TupleStructId) -> Self {
        Self(value.0)
    }
}

#[derive(Debug, Clone)]
pub struct TupleStructDeclaration {
    pub name: String,
    pub generic_types: Vec<DataType>,
    pub field_types: Vec<DataType>,
}

make_id!(UnitStructId);

impl From<UnitStructId> for StructId {
    fn from(value: UnitStructId) -> Self {
        Self(value.0)
    }
}

#[derive(Debug, Clone)]
pub struct UnitStructDeclaration {
    pub name: String,
}
