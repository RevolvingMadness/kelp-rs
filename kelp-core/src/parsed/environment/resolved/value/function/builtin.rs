use strum::EnumIter;

use crate::{
    compile_context::CompileContext,
    datapack::Datapack,
    parsed::environment::resolved::r#type::{HighGenericId, r#struct::tuple::HighTupleStructId},
    typed::{data_type::unresolved::UnresolvedDataType, expression::resolved::Expression},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HighBuiltinFunctionId(pub u32);

#[derive(Debug, Clone)]
pub struct ResolvedBuiltinFunctionDeclaration {
    pub name: String,
    pub generic_ids: Vec<HighGenericId>,
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
    pub fn declaration(self) -> ResolvedBuiltinFunctionDeclaration {
        macro_rules! declaration {
            (
                fn $name:ident ( $($parameters:expr),* $(,)? )
            ) => {
                ResolvedBuiltinFunctionDeclaration {
                    name: stringify!($name).to_owned(),
                    generic_names: Vec::new(),
                    parameters: vec![$($parameters),*],
                    return_type: UnresolvedDataType::Unit,
                    kind: self,
                }
            };

            (
                fn $name:ident ( $($parameters:expr),* $(,)? ) -> $return_type:expr
            ) => {
                ResolvedBuiltinFunctionDeclaration {
                    name: stringify!($name).to_owned(),
                    generic_ids: Vec::new(),
                    parameters: vec![$($parameters),*],
                    return_type: $return_type,
                    kind: self,
                }
            };

            (
                fn $name:ident < $($generic:ident),+ > ( $($parameters:expr),* $(,)? )
            ) => {
                declaration!(fn $name<$($generic),+>($($parameters),*) -> UnresolvedDataType::Unit)
            };

            (
                fn $name:ident < $($generic:ident),+ > ( $($parameters:expr),* $(,)? ) -> $return_type:expr
            ) => {
                ResolvedBuiltinFunctionDeclaration {
                    name: stringify!($name).to_owned(),
                    generic_names: vec![$(stringify!($generic).to_owned()),+],
                    parameters: vec![$($parameters),*],
                    return_type: $return_type,
                    kind: self,
                }
            };
        }

        match self {
            Self::TupleConstructor(..) => unreachable!(),

            Self::StdAdd => {
                declaration!(fn add(UnresolvedDataType::Integer, UnresolvedDataType::Integer) -> UnresolvedDataType::Integer)
            }
        }
    }

    pub fn call(
        self,
        datapack: &mut Datapack,
        _ctx: &mut CompileContext,
        mut arguments: Vec<Expression>,
    ) -> Expression {
        match self {
            Self::TupleConstructor(id, generic_types) => {
                let id = UnresolvedDataType::resolve_tuple_struct(datapack, id, generic_types);

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
