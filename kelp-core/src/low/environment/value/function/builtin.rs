use crate::{
    compile_context::CompileContext,
    datapack::Datapack,
    low::{
        data_type::DataType,
        environment::value::function::{FunctionDeclaration, FunctionId},
        expression::Expression,
    },
    typed::{
        data_type::SemanticDataType, environment::value::function::builtin::BuiltinFunctionKind,
    },
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BuiltinFunctionId(pub u32);

impl From<BuiltinFunctionId> for FunctionId {
    fn from(value: BuiltinFunctionId) -> Self {
        Self(value.0)
    }
}

#[derive(Debug, Clone)]
pub struct BuiltinFunctionDeclaration {
    pub name: String,
    pub generic_types: Vec<DataType>,
    pub parameters: Vec<DataType>,
    pub return_type: DataType,
    pub kind: BuiltinFunctionKind,
}

impl From<BuiltinFunctionDeclaration> for FunctionDeclaration {
    fn from(value: BuiltinFunctionDeclaration) -> Self {
        Self::Builtin(value)
    }
}

impl BuiltinFunctionKind {
    pub fn call(
        self,
        datapack: &mut Datapack,
        _ctx: &mut CompileContext,
        mut arguments: Vec<Expression>,
    ) -> Expression {
        match self {
            Self::TupleConstructor(id, generic_types) => {
                let id = SemanticDataType::resolve_tuple_struct(datapack, id, generic_types);

                Expression::TupleStruct(id, arguments)
            }
            Self::StdAdd => {
                let Expression::Integer(a) = arguments.pop().unwrap() else {
                    unreachable!();
                };

                let Expression::Integer(b) = arguments.pop().unwrap() else {
                    unreachable!();
                };

                Expression::Integer(a + b)
            }
        }
    }
}
