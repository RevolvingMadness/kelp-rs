use crate::high::environment::r#type::r#struct::{
    regular::{HighStructStructDeclaration, HighStructStructId},
    tuple::{HighTupleStructDeclaration, HighTupleStructId},
};

pub mod regular;
pub mod tuple;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HighStructId(pub usize);

impl From<HighStructStructId> for HighStructId {
    fn from(value: HighStructStructId) -> Self {
        Self(value.0)
    }
}

impl From<HighTupleStructId> for HighStructId {
    fn from(value: HighTupleStructId) -> Self {
        Self(value.0)
    }
}

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
