use std::collections::HashMap;

use crate::high::data_type::resolved::PartiallyResolvedDataType;

#[derive(Debug, Clone)]
pub enum HighStructDeclaration {
    Struct(HighStructStructDeclaration),
    Tuple(HighTupleStructDeclaration),
}

impl HighStructDeclaration {
    #[must_use]
    pub fn name(&self) -> &str {
        match self {
            Self::Struct(declaration) => &declaration.name,
            Self::Tuple(declaration) => &declaration.name,
        }
    }

    #[must_use]
    pub fn generic_names(&self) -> &[String] {
        match self {
            Self::Struct(declaration) => &declaration.generic_names,
            Self::Tuple(declaration) => &declaration.generic_names,
        }
    }

    #[must_use]
    pub const fn generic_count(&self) -> usize {
        match self {
            Self::Struct(declaration) => declaration.generic_names.len(),
            Self::Tuple(declaration) => declaration.generic_names.len(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct HighStructStructDeclaration {
    pub name: String,
    pub generic_names: Vec<String>,
    pub field_types: HashMap<String, Option<PartiallyResolvedDataType>>,
}

#[derive(Debug, Clone)]
pub struct HighTupleStructDeclaration {
    pub name: String,
    pub generic_names: Vec<String>,
    pub field_types: Vec<Option<PartiallyResolvedDataType>>,
}
