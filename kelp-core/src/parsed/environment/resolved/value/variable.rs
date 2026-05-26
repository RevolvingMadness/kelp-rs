use crate::typed::data_type::unresolved::SemanticDataType;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HighVariableId(pub u32);

#[derive(Debug, Clone)]
pub struct SemanticVariableDeclaration {
    pub name: String,
    pub data_type: SemanticDataType,
}
