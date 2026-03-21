use crate::high::data_type::resolved::PartiallyResolvedDataType;

#[derive(Debug, Clone)]
pub struct HighAliasDeclaration {
    pub name: String,
    pub generic_names: Vec<String>,
    pub alias: Option<PartiallyResolvedDataType>,
}
