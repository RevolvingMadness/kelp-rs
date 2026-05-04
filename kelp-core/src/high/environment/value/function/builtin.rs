use crate::low::{data_type::DataType, environment::value::function::BuiltinFunctionKind};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HighBuiltinFunctionId(pub usize);

#[derive(Debug, Clone)]
pub struct HighBuiltinFunctionDeclaration {
    pub name: String,
    pub generic_names: Vec<String>,
    pub parameters: Vec<DataType>,
    pub return_type: DataType,
    pub kind: BuiltinFunctionKind,
}
