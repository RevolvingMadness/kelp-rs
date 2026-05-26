use crate::{
    parsed::environment::resolved::value::function::builtin::BuiltinFunctionKind,
    typed::data_type::resolved::ResolvedDataType,
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
