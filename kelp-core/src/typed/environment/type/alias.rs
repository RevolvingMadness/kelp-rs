use crate::typed::{data_type::SemanticDataType, environment::r#type::HighGenericId};

#[derive(Debug, Clone)]
pub struct SemanticTypeAliasDeclaration {
    pub name: String,
    pub generic_ids: Vec<HighGenericId>,
    pub alias: SemanticDataType,
}
