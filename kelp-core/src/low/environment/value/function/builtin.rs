use crate::low::data_type::DataType;
use crate::semantic::environment::value::function::builtin::BuiltinFunctionKind;

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
