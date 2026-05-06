use crate::low::{
    data_type::unresolved::UnresolvedDataType, expression::unresolved::UnresolvedExpression,
    pattern::UnresolvedPattern,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HighRegularFunctionId(pub usize);

#[derive(Debug, Clone)]
pub struct HighRegularFunctionDeclaration {
    pub name: String,
    pub generic_names: Vec<String>,
    pub parameters: Vec<(Option<UnresolvedPattern>, UnresolvedDataType)>,
    pub return_type: UnresolvedDataType,
    pub body: Option<UnresolvedExpression>,
}
