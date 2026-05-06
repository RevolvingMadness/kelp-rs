use crate::low::{
    data_type::resolved::ResolvedDataType, environment::value::function::BuiltinFunctionKind,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BuiltinFunctionId(pub usize);

#[derive(Debug, Clone)]
pub struct BuiltinFunctionDeclaration {
    pub module_path: Vec<String>,
    pub name: String,
    pub generic_types: Vec<ResolvedDataType>,
    pub parameters: Vec<ResolvedDataType>,
    pub return_type: ResolvedDataType,
    pub kind: BuiltinFunctionKind,
}
