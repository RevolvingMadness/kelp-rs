use crate::low::{data_type::DataType, expression::unresolved::UnresolvedExpression};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HighFunctionId(pub usize);

#[derive(Debug, Clone)]
pub struct HighFunctionDeclaration {
    pub name: String,
    pub generic_names: Vec<String>,
    pub parameter_types: Vec<Option<DataType>>,
    pub return_type: Option<DataType>,
    pub body: Option<UnresolvedExpression>,
}
