use crate::parsed::environment::{
    resolved::r#type::alias::ResolvedTypeAliasDeclaration, unresolved::r#type::HighGenericId,
};

#[derive(Debug, Clone)]
pub struct UnresolvedTypeAliasDeclaration {
    pub name: String,
    pub generic_ids: Vec<HighGenericId>,
}

impl From<ResolvedTypeAliasDeclaration> for UnresolvedTypeAliasDeclaration {
    fn from(value: ResolvedTypeAliasDeclaration) -> Self {
        Self {
            name: value.name,
            generic_ids: value.generic_ids,
        }
    }
}
