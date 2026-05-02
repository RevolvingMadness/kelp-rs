use crate::low::data_type::DataType;

#[derive(Debug, Clone)]
pub struct HighVariableDeclaration {
    pub name: String,
    pub data_type: Option<DataType>,
}
