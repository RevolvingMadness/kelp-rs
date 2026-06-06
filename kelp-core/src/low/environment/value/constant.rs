use crate::{
    low::{data_type::DataType, environment::value::ValueId},
    make_id,
};

make_id!(ConstantId);

impl From<ConstantId> for ValueId {
    fn from(value: ConstantId) -> Self {
        Self(value.0)
    }
}

#[derive(Debug, Clone)]
pub struct ConstantDeclaration {
    pub name: String,
    pub data_type: DataType,
}
