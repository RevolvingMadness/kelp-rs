use crate::low::{
    data_type::DataType, expression::unresolved::UnresolvedExpression, pattern::Pattern,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RegularFunctionId(pub usize);

#[derive(Debug, Clone)]
pub struct RegularFunctionDeclaration {
    pub module_path: Vec<String>,
    pub name: String,
    pub generic_types: Vec<DataType>,
    pub parameters: Vec<(Pattern, DataType)>,
    pub return_type: DataType,
    pub body: UnresolvedExpression,
}
