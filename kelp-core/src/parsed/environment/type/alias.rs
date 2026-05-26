use crate::semantic::environment::r#type::HighGenericId;
use crate::semantic::environment::r#type::alias::SemanticTypeAliasDeclaration;

#[derive(Debug, Clone)]
pub struct ParsedTypeAliasDeclaration {
    pub name: String,
    pub generic_ids: Vec<HighGenericId>,
}

impl From<SemanticTypeAliasDeclaration> for ParsedTypeAliasDeclaration {
    fn from(value: SemanticTypeAliasDeclaration) -> Self {
        Self {
            name: value.name,
            generic_ids: value.generic_ids,
        }
    }
}
