use crate::low::data_type::unresolved::UnresolvedDataType;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HighVariableId(pub u32);

#[derive(Debug, Clone)]
pub struct HighVariableDeclaration {
    pub name: String,
    pub data_type: UnresolvedDataType,
}
