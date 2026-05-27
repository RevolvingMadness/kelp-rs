use strum::EnumIter;

use crate::compile_context::CompileContext;
use crate::datapack::Datapack;
use crate::low::data_type::DataType;
use crate::low::environment::r#type::r#struct::TupleStructId;
use crate::low::expression::Expression;

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

#[derive(Debug, Clone, EnumIter)]
pub enum BuiltinFunctionKind {
    #[strum(disabled)]
    TupleStructConstructor(TupleStructId),

    StdAdd,
}

impl BuiltinFunctionKind {
    pub fn call(
        self,
        _datapack: &mut Datapack,
        _ctx: &mut CompileContext,
        mut arguments: Vec<Expression>,
    ) -> Expression {
        match self {
            Self::TupleStructConstructor(id) => Expression::TupleStruct(id, arguments),
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
