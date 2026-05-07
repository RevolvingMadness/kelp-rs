use crate::{
    high::environment::value::function::builtin::BuiltinFunctionKind,
    low::data_type::resolved::ResolvedDataType,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BuiltinFunctionId(pub u32);

#[derive(Debug, Clone)]
pub struct BuiltinFunctionDeclaration {
    pub name: String,
    pub generic_types: Vec<ResolvedDataType>,
    pub parameters: Vec<ResolvedDataType>,
    pub return_type: ResolvedDataType,
    pub kind: BuiltinFunctionKind,
}
