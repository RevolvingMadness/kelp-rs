use crate::low::data_type::DataType;

#[derive(Debug, Clone)]
pub struct HighFunctionDeclaration {
    pub name: String,
    pub generic_names: Vec<String>,
    pub parameter_types: Vec<DataType>,
    pub return_type: DataType,
}
