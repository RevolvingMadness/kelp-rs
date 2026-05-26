use strum::EnumIter;

use crate::low::expression::Expression;
use crate::semantic::data_type::SemanticDataType;
use crate::semantic::environment::r#type::{HighGenericId, r#struct::tuple::HighTupleStructId};
use crate::semantic::environment::value::HighValueId;
use crate::semantic::environment::value::function::HighFunctionId;
use crate::{compile_context::CompileContext, datapack::Datapack};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HighBuiltinFunctionId(pub u32);

impl From<HighBuiltinFunctionId> for HighFunctionId {
    fn from(value: HighBuiltinFunctionId) -> Self {
        Self(value.0)
    }
}

impl From<HighBuiltinFunctionId> for HighValueId {
    fn from(value: HighBuiltinFunctionId) -> Self {
        Self(value.0)
    }
}

#[derive(Debug, Clone)]
pub struct SemanticBuiltinFunctionDeclaration {
    pub name: String,
    pub generic_ids: Vec<HighGenericId>,
    pub parameters: Vec<SemanticDataType>,
    pub return_type: SemanticDataType,
    pub kind: BuiltinFunctionKind,
}

#[derive(Debug, Clone, EnumIter)]
pub enum BuiltinFunctionKind {
    #[strum(disabled)]
    TupleConstructor(HighTupleStructId, Vec<SemanticDataType>),

    StdAdd, // fn add(integer, integer) -> integer
}

impl BuiltinFunctionKind {
    #[must_use]
    pub fn declaration(self) -> SemanticBuiltinFunctionDeclaration {
        macro_rules! declaration {
            (
                fn $name:ident ( $($parameters:expr),* $(,)? )
            ) => {
                SemanticBuiltinFunctionDeclaration {
                    name: stringify!($name).to_owned(),
                    generic_names: Vec::new(),
                    parameters: vec![$($parameters),*],
                    return_type: SemanticDataType::Unit,
                    kind: self,
                }
            };

            (
                fn $name:ident ( $($parameters:expr),* $(,)? ) -> $return_type:expr
            ) => {
                SemanticBuiltinFunctionDeclaration {
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
                SemanticBuiltinFunctionDeclaration {
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
                declaration!(fn add(SemanticDataType::Integer, SemanticDataType::Integer) -> SemanticDataType::Integer)
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
