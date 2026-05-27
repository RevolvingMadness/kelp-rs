use strum::EnumIter;

use crate::datapack::Datapack;
use crate::low::environment::r#type::r#struct::TupleStructId;
use crate::low::environment::value::function::builtin::BuiltinFunctionKind;
use crate::semantic::data_type::SemanticDataType;
use crate::semantic::environment::r#type::{HighGenericId, r#struct::tuple::HighTupleStructId};
use crate::semantic::environment::value::HighValueId;
use crate::semantic::environment::value::function::HighFunctionId;

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
    pub kind: SemanticBuiltinFunctionKind,
}

#[derive(Debug, Clone, EnumIter)]
pub enum SemanticBuiltinFunctionKind {
    #[strum(disabled)]
    TupleStructConstructor(HighTupleStructId, Vec<HighGenericId>),

    StdAdd, // fn add(integer, integer) -> integer
}

impl SemanticBuiltinFunctionKind {
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
            Self::TupleStructConstructor(..) => unreachable!(),

            Self::StdAdd => {
                declaration!(fn add(SemanticDataType::Integer, SemanticDataType::Integer) -> SemanticDataType::Integer)
            }
        }
    }

    #[must_use]
    pub fn monomorphize(
        self,
        datapack: &mut Datapack,
        generic_types: &[SemanticDataType],
    ) -> BuiltinFunctionKind {
        match self {
            Self::TupleStructConstructor(id, generic_ids) => {
                let generic_types = generic_ids
                    .iter()
                    .copied()
                    .map(SemanticDataType::Generic)
                    .map(|data_type| data_type.substitute_generics(&generic_ids, generic_types))
                    .collect();

                let id = SemanticDataType::monomorphize_tuple_struct(datapack, id, generic_types);

                BuiltinFunctionKind::TupleStructConstructor(TupleStructId(id.0))
            }
            Self::StdAdd => BuiltinFunctionKind::StdAdd,
        }
    }
}
