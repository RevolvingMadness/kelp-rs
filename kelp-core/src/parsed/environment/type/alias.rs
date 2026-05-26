use crate::semantic::environment::r#type::alias::ResolvedTypeAliasDeclaration;
use crate::semantic::environment::r#type::HighGenericId;

#[derive(Debug, Clone)]
pub struct ParsedTypeAliasDeclaration {
    pub name: String,
    pub generic_ids: Vec<HighGenericId>,
}

impl From<ResolvedTypeAliasDeclaration> for ParsedTypeAliasDeclaration {
    fn from(value: ResolvedTypeAliasDeclaration) -> Self {
        Self {
            name: value.name,
            generic_ids: value.generic_ids,
        }
    }
}
