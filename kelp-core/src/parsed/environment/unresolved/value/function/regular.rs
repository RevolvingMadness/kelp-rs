use crate::parsed::environment::resolved::{
    r#type::HighGenericId, value::function::regular::SemanticRegularFunctionDeclaration,
};

#[derive(Debug, Clone)]
pub struct ParsedRegularFunctionDeclaration {
    pub name: String,
    pub generic_ids: Vec<HighGenericId>,
}

impl From<SemanticRegularFunctionDeclaration> for ParsedRegularFunctionDeclaration {
    fn from(value: SemanticRegularFunctionDeclaration) -> Self {
        Self {
            name: value.name,
            generic_ids: value.generic_ids,
        }
    }
}
