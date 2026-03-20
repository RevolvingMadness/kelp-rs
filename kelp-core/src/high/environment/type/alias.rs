use crate::high::data_type::resolved::ResolvedDataType;

#[derive(Debug, Clone)]
pub struct HighAliasDeclaration {
    pub name: String,
    pub generic_names: Vec<String>,
    pub alias: Option<ResolvedDataType>,
}
