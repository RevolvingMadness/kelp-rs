use crate::low::data_type::DataType;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FunctionId(pub usize);

#[derive(Debug, Clone)]
pub struct FunctionDeclaration {
    pub name: String,
    pub generic_types: Vec<DataType>,
    pub parameter_types: Vec<DataType>,
    pub return_type: DataType,
}
