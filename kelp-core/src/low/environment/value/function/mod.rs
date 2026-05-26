use la_arena::Idx;

use crate::{
    low::{
        data_type::DataType,
        environment::value::function::{
            builtin::BuiltinFunctionDeclaration, regular::RegularFunctionDeclaration,
        },
    },
    parameter_types_iter::{ParameterTypesIter, take_second},
    typed::pattern::TypedPattern,
};

pub mod builtin;
pub mod regular;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FunctionId(pub u32);

#[derive(Debug, Clone)]
pub enum FunctionDeclaration {
    Regular(RegularFunctionDeclaration),
    Builtin(BuiltinFunctionDeclaration),
}

impl FunctionDeclaration {
    #[must_use]
    pub fn name(&self) -> &str {
        match self {
            Self::Regular(declaration) => &declaration.name,
            Self::Builtin(declaration) => &declaration.name,
        }
    }

    #[must_use]
    pub fn generic_types(&self) -> &[DataType] {
        match self {
            Self::Regular(declaration) => &declaration.generic_types,
            Self::Builtin(declaration) => &declaration.generic_types,
        }
    }

    #[must_use]
    pub fn parameter_types(&self) -> ParameterTypesIter<'_, Idx<TypedPattern>, DataType> {
        match self {
            Self::Regular(declaration) => {
                ParameterTypesIter::Regular(declaration.parameters.iter().map(take_second))
            }
            Self::Builtin(declaration) => {
                ParameterTypesIter::Builtin(declaration.parameters.iter())
            }
        }
    }

    #[must_use]
    pub const fn return_type(&self) -> &DataType {
        match self {
            Self::Regular(declaration) => &declaration.return_type,
            Self::Builtin(declaration) => &declaration.return_type,
        }
    }
}
