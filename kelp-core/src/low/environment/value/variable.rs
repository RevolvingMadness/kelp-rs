use crate::low::data_type::resolved::ResolvedDataType;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VariableId(pub usize);

#[derive(Debug, Clone)]
pub struct VariableDeclaration {
    pub name: String,
    pub data_type: ResolvedDataType,
}
