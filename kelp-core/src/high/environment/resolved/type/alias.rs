use crate::{
    high::environment::resolved::r#type::HighGenericId,
    low::data_type::unresolved::UnresolvedDataType,
};

#[derive(Debug, Clone)]
pub struct ResolvedTypeAliasDeclaration {
    pub name: String,
    pub generic_ids: Vec<HighGenericId>,
    pub alias: UnresolvedDataType,
}
