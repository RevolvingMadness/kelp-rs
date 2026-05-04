use crate::low::{
    data_type::DataType, expression::unresolved::UnresolvedExpression, pattern::Pattern,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HighRegularFunctionId(pub usize);

#[derive(Debug, Clone)]
pub struct HighRegularFunctionDeclaration {
    pub name: String,
    pub generic_names: Vec<String>,
    pub parameters: Vec<(Option<Pattern>, Option<DataType>)>,
    pub return_type: Option<DataType>,
    pub body: Option<UnresolvedExpression>,
}
