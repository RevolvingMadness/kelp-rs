use crate::{
    parsed::environment::resolved::value::function::builtin::BuiltinFunctionKind,
    typed::data_type::resolved::DataType,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BuiltinFunctionId(pub u32);

#[derive(Debug, Clone)]
pub struct BuiltinFunctionDeclaration {
    pub name: String,
    pub generic_types: Vec<DataType>,
    pub parameters: Vec<DataType>,
    pub return_type: DataType,
    pub kind: BuiltinFunctionKind,
}
