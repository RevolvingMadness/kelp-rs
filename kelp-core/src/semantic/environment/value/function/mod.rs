pub mod builtin;
pub mod regular;

use crate::semantic::data_type::SemanticDataType;
use crate::semantic::environment::value::function::{
    builtin::SemanticBuiltinFunctionDeclaration, regular::SemanticRegularFunctionDeclaration,
};
use crate::{
    parameter_types_iter::{ParameterTypesIter, take_second},
    semantic::pattern::SemanticPattern,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HighFunctionId(pub u32);

#[derive(Debug, Clone)]
pub enum SemanticFunctionDeclaration {
    Regular(SemanticRegularFunctionDeclaration),
    Builtin(SemanticBuiltinFunctionDeclaration),
}

impl SemanticFunctionDeclaration {
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
