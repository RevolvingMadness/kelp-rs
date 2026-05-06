use strum::EnumIter;

use crate::{
    compile_context::CompileContext,
    datapack::Datapack,
    high::environment::r#type::r#struct::tuple::HighTupleStructId,
    low::{data_type::unresolved::UnresolvedDataType, expression::resolved::ResolvedExpression},
};

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

#[derive(Debug, Clone, EnumIter)]
pub enum BuiltinFunctionKind {
    #[strum(disabled)]
    TupleConstructor(HighTupleStructId, Vec<UnresolvedDataType>),

    StdAdd, // fn add(integer, integer) -> integer
}

impl BuiltinFunctionKind {
    #[must_use]
    pub fn declaration(self) -> HighBuiltinFunctionDeclaration {
        match self {
            Self::TupleConstructor(..) => unreachable!(),

            Self::StdAdd => HighBuiltinFunctionDeclaration {
                name: "add".to_owned(),
                generic_names: Vec::new(),
                parameters: vec![UnresolvedDataType::Integer, UnresolvedDataType::Integer],
                return_type: UnresolvedDataType::Integer,
                kind: self,
            },
        }
    }

    pub fn call(
        self,
        datapack: &mut Datapack,
        _ctx: &mut CompileContext,
        mut arguments: Vec<ResolvedExpression>,
    ) -> ResolvedExpression {
        match self {
            Self::TupleConstructor(id, generic_types) => {
                let id = UnresolvedDataType::resolve_tuple_struct(datapack, id, generic_types);

                ResolvedExpression::TupleStruct(id, arguments)
            }
            Self::StdAdd => {
                let ResolvedExpression::Integer(a) = arguments.pop().unwrap() else {
                    unreachable!();
                };

                let ResolvedExpression::Integer(b) = arguments.pop().unwrap() else {
                    unreachable!();
                };

                ResolvedExpression::Integer(a + b)
            }
        }
    }
}
