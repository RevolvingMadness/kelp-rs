use crate::{
    make_id,
    semantic::{data_type::SemanticDataType, environment::value::HighValueId},
};

make_id!(HighVariableId);

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
