use crate::low::{
    data_type::resolved::ResolvedDataType, expression::unresolved::UnresolvedExpression,
    pattern::UnresolvedPattern,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RegularFunctionId(pub usize);

#[derive(Debug, Clone)]
pub struct RegularFunctionDeclaration {
    pub module_path: Vec<String>,
    pub name: String,
    pub generic_types: Vec<ResolvedDataType>,
    pub parameters: Vec<(UnresolvedPattern, ResolvedDataType)>,
    pub return_type: ResolvedDataType,
    pub body: UnresolvedExpression,
}
