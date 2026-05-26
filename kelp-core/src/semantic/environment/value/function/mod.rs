pub mod builtin;
pub mod regular;

use crate::{
    parameter_types_iter::{take_second, ParameterTypesIter},
    semantic::pattern::SemanticPattern,
};
use crate::semantic::data_type::SemanticDataType;
use crate::semantic::environment::value::function::{
    builtin::{HighBuiltinFunctionId, ResolvedBuiltinFunctionDeclaration},
    regular::{HighRegularFunctionId, ResolvedRegularFunctionDeclaration},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HighFunctionId(pub u32);

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
pub enum ResolvedFunctionDeclaration {
    Regular(ResolvedRegularFunctionDeclaration),
    Builtin(ResolvedBuiltinFunctionDeclaration),
}

impl ResolvedFunctionDeclaration {
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
            Self::Regular(declaration) => declaration.generic_ids.len(),
            Self::Builtin(declaration) => declaration.generic_ids.len(),
        }
    }

    #[must_use]
    pub fn parameter_types(
        &self,
    ) -> ParameterTypesIter<'_, Option<SemanticPattern>, SemanticDataType> {
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
    pub const fn return_type(&self) -> &SemanticDataType {
        match self {
            Self::Regular(declaration) => &declaration.return_type,
            Self::Builtin(declaration) => &declaration.return_type,
        }
    }
}
