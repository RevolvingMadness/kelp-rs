use crate::parsed::environment::{
    resolved::r#type::r#struct::tuple::SemanticTupleStructDeclaration,
    unresolved::r#type::HighGenericId,
};

#[derive(Debug, Clone)]
pub struct ParsedTupleStructDeclaration {
    name: String,
    generic_ids: Vec<HighGenericId>,
}

impl From<SemanticTupleStructDeclaration> for ParsedTupleStructDeclaration {
    fn from(value: SemanticTupleStructDeclaration) -> Self {
        Self {
            name: value.name,
            generic_ids: value.generic_ids,
        }
    }
}

impl ParsedTupleStructDeclaration {
    #[inline]
    #[must_use]
    pub fn new(name: &str, generic_ids: Vec<HighGenericId>) -> Self {
        Self {
            name: name.to_owned(),
            generic_ids,
        }
    }

    #[inline]
    #[must_use]
    pub fn name(&self) -> &str {
        &self.name
    }

    #[inline]
    #[must_use]
    pub fn generic_ids(&self) -> &[HighGenericId] {
        &self.generic_ids
    }

    #[inline]
    #[must_use]
    pub const fn generic_count(&self) -> usize {
        self.generic_ids.len()
    }
}
