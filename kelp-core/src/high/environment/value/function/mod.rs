pub mod builtin;
pub mod regular;

use crate::{
    high::environment::value::function::{
        builtin::{HighBuiltinFunctionDeclaration, HighBuiltinFunctionId},
        regular::{HighRegularFunctionDeclaration, HighRegularFunctionId},
    },
    low::{data_type::unresolved::UnresolvedDataType, pattern::UnresolvedPattern},
    parameter_types_iter::{ParameterTypesIter, take_second},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HighFunctionId(pub usize);

impl From<HighRegularFunctionId> for HighFunctionId {
    fn from(value: HighRegularFunctionId) -> Self {
        Self(value.0)
    }
}

impl From<HighBuiltinFunctionId> for HighFunctionId {
    fn from(value: HighBuiltinFunctionId) -> Self {
        Self(value.0)
    }
}

#[derive(Debug, Clone)]
pub enum HighFunctionDeclaration {
    Regular(HighRegularFunctionDeclaration),
    Builtin(HighBuiltinFunctionDeclaration),
}

impl HighFunctionDeclaration {
    #[must_use]
    pub fn name(&self) -> &str {
        match self {
            Self::Regular(declaration) => &declaration.name,
            Self::Builtin(declaration) => &declaration.name,
        }
    }

    #[must_use]
    pub const fn generic_count(&self) -> usize {
        match self {
            Self::Regular(declaration) => declaration.generic_names.len(),
            Self::Builtin(declaration) => declaration.generic_names.len(),
        }
    }

    #[must_use]
    pub fn parameter_types(
        &self,
    ) -> ParameterTypesIter<'_, Option<UnresolvedPattern>, UnresolvedDataType> {
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
    pub const fn return_type(&self) -> &UnresolvedDataType {
        match self {
            Self::Regular(declaration) => &declaration.return_type,
            Self::Builtin(declaration) => &declaration.return_type,
        }
    }
}
