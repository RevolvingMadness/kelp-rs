use crate::{
    parsed::environment::resolved::r#type::HighGenericId,
    typed::data_type::unresolved::SemanticDataType,
};

#[derive(Debug, Clone)]
pub struct SemanticTypeAliasDeclaration {
    pub name: String,
    pub generic_ids: Vec<HighGenericId>,
    pub alias: SemanticDataType,
}
