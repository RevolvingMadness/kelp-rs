use crate::{
    high::environment::r#type::HighGenericId, low::data_type::unresolved::UnresolvedDataType,
};

#[derive(Debug, Clone)]
pub struct HighTypeAliasDeclaration {
    pub name: String,
    pub generic_ids: Vec<HighGenericId>,
    pub alias: UnresolvedDataType,
}
