use crate::typed::{data_type::SemanticDataType, environment::value::HighValueId};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HighVariableId(pub u32);

impl From<HighVariableId> for HighValueId {
    fn from(value: HighVariableId) -> Self {
        Self(value.0)
    }
}

#[derive(Debug, Clone)]
pub struct SemanticVariableDeclaration {
    pub name: String,
    pub data_type: SemanticDataType,
}
