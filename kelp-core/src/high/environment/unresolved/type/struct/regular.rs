use crate::high::environment::{
    resolved::r#type::r#struct::regular::ResolvedRegularStructDeclaration,
    unresolved::r#type::HighGenericId,
};

#[derive(Debug, Clone)]
pub struct UnresolvedRegularStructDeclaration {
    pub name: String,
    pub generic_ids: Vec<HighGenericId>,
}

impl From<ResolvedRegularStructDeclaration> for UnresolvedRegularStructDeclaration {
    fn from(value: ResolvedRegularStructDeclaration) -> Self {
        Self {
            name: value.name,
            generic_ids: value.generic_ids,
        }
    }
}
