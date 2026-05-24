use crate::high::environment::{
    resolved::r#type::r#struct::tuple::ResolvedTupleStructDeclaration,
    unresolved::r#type::HighGenericId,
};

#[derive(Debug, Clone)]
pub struct UnresolvedTupleStructDeclaration {
    pub name: String,
    pub generic_ids: Vec<HighGenericId>,
}

impl From<ResolvedTupleStructDeclaration> for UnresolvedTupleStructDeclaration {
    fn from(value: ResolvedTupleStructDeclaration) -> Self {
        Self {
            name: value.name,
            generic_ids: value.generic_ids,
        }
    }
}
