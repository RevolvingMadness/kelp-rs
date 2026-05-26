use crate::semantic::environment::r#type::r#struct::regular::SemanticRegularStructDeclaration;
use crate::semantic::environment::r#type::HighGenericId;

#[derive(Debug, Clone)]
pub struct ParsedRegularStructDeclaration {
    pub name: String,
    pub generic_ids: Vec<HighGenericId>,
}

impl From<SemanticRegularStructDeclaration> for ParsedRegularStructDeclaration {
    fn from(value: SemanticRegularStructDeclaration) -> Self {
        Self {
            name: value.name,
            generic_ids: value.generic_ids,
        }
    }
}
