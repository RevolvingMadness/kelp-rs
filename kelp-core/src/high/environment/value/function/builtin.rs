use crate::low::{data_type::unresolved::UnresolvedDataType, environment::value::function::BuiltinFunctionKind};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HighBuiltinFunctionId(pub usize);

#[derive(Debug, Clone)]
pub struct HighBuiltinFunctionDeclaration {
    pub name: String,
    pub generic_names: Vec<String>,
    pub parameters: Vec<UnresolvedDataType>,
    pub return_type: UnresolvedDataType,
    pub kind: BuiltinFunctionKind,
}
