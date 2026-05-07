use crate::{
    low::{
        data_type::resolved::ResolvedDataType, expression::unresolved::UnresolvedExpression,
        pattern::UnresolvedPattern,
    },
    visibility::Visibility,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RegularFunctionId(pub u32);

#[derive(Debug, Clone)]
pub struct RegularFunctionDeclaration {
    pub module_paths: Vec<String>,
    pub visibility: Visibility,
    pub is_runtime: bool,
    pub name: String,
    pub generic_types: Vec<ResolvedDataType>,
    pub parameters: Vec<(UnresolvedPattern, ResolvedDataType)>,
    pub return_type: ResolvedDataType,
    pub body: UnresolvedExpression,
}
