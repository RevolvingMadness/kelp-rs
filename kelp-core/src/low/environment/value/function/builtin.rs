use crate::low::{data_type::DataType, environment::value::function::BuiltinFunctionKind};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BuiltinFunctionId(pub usize);

#[derive(Debug, Clone)]
pub struct BuiltinFunctionDeclaration {
    pub module_path: Vec<String>,
    pub name: String,
    pub generic_types: Vec<DataType>,
    pub parameters: Vec<DataType>,
    pub return_type: DataType,
    pub kind: BuiltinFunctionKind,
}
