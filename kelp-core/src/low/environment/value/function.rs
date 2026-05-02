use crate::low::{data_type::DataType, expression::unresolved::UnresolvedExpression};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FunctionId(pub usize);

#[derive(Debug, Clone)]
pub struct FunctionDeclaration {
    pub module_path: Vec<String>,
    pub name: String,
    pub generic_types: Vec<DataType>,
    pub parameter_types: Vec<DataType>,
    pub return_type: DataType,
    pub body: Option<UnresolvedExpression>,
}
