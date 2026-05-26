use crate::semantic::data_type::SemanticDataType;
use crate::semantic::environment::r#type::HighTypeId;
use crate::semantic::environment::r#type::{
    HighGenericId,
    r#struct::{regular::SemanticRegularStructDeclaration, tuple::SemanticTupleStructDeclaration},
};

pub mod regular;
pub mod tuple;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HighStructId(pub u32);

impl From<HighStructId> for HighTypeId {
    fn from(value: HighStructId) -> Self {
        Self(value.0)
    }
}

#[derive(Debug, Clone)]
pub enum SemanticStructDeclaration {
    Struct(SemanticRegularStructDeclaration),
    Tuple(SemanticTupleStructDeclaration),
}

impl SemanticStructDeclaration {
    #[must_use]
    pub fn name(&self) -> &str {
        match self {
            Self::Struct(declaration) => &declaration.name,
            Self::Tuple(declaration) => &declaration.name,
        }
    }

    #[must_use]
    pub fn generic_ids(&self) -> &[HighGenericId] {
        match self {
            Self::Struct(declaration) => &declaration.generic_ids,
            Self::Tuple(declaration) => &declaration.generic_ids,
        }
    }

    #[must_use]
    pub const fn generic_count(&self) -> usize {
        match self {
            Self::Struct(declaration) => declaration.generic_ids.len(),
            Self::Tuple(declaration) => declaration.generic_ids.len(),
        }
    }

    #[must_use]
    pub fn get_field(&self, field_name: &str) -> Option<&SemanticDataType> {
        match self {
            Self::Struct(declaration) => declaration.field_types.get(field_name),
            Self::Tuple(declaration) => {
                let field_index = field_name.parse::<usize>().ok()?;

                declaration.field_types.get(field_index)
            }
        }
    }
}
