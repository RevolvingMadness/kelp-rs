use crate::low::data_type::unresolved::UnresolvedDataType;

#[derive(Debug, Clone)]
pub struct HighAliasDeclaration {
    pub name: String,
    pub generic_names: Vec<String>,
    pub alias: UnresolvedDataType,
}
