use crate::low::{
    data_type::DataType, expression::unresolved::UnresolvedExpression, pattern::Pattern,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HighFunctionId(pub usize);

#[derive(Debug, Clone)]
pub struct HighFunctionDeclaration {
    pub name: String,
    pub generic_names: Vec<String>,
    pub parameters: Vec<(Option<Pattern>, Option<DataType>)>,
    pub return_type: Option<DataType>,
    pub body: Option<UnresolvedExpression>,
}
