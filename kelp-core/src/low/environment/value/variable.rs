use crate::{low::data_type::DataType, make_id};

make_id!(VariableId);

#[derive(Debug, Clone)]
pub struct VariableDeclaration {
    pub name: String,
    pub data_type: DataType,
}
