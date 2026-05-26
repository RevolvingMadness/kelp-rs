use crate::semantic::data_type::SemanticDataType;
use crate::semantic::environment::r#type::HighGenericId;

#[derive(Debug, Clone)]
pub struct ResolvedTypeAliasDeclaration {
    pub name: String,
    pub generic_ids: Vec<HighGenericId>,
    pub alias: SemanticDataType,
}
