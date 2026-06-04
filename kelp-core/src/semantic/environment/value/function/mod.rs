pub mod builtin;
pub mod regular;

use crate::make_id;
use crate::semantic::data_type::SemanticDataType;
use crate::semantic::environment::r#type::HighGenericId;
use crate::semantic::environment::value::HighValueId;
use crate::semantic::environment::value::function::{
    builtin::SemanticBuiltinFunctionDeclaration, regular::SemanticRegularFunctionDeclaration,
};
use crate::{
    parameter_types_iter::{ParameterTypesIter, take_second},
    semantic::pattern::SemanticPattern,
};

make_id!(HighFunctionId);

impl From<HighFunctionId> for HighValueId {
    fn from(value: HighFunctionId) -> Self {
        Self(value.0)
    }
}

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
    pub fn generic_ids(&self) -> &[HighGenericId] {
        match self {
            Self::Regular(declaration) => &declaration.generic_ids,
            Self::Builtin(declaration) => &declaration.generic_ids,
        }
    }

    #[must_use]
    pub const fn declared_generic_count(&self) -> usize {
        match self {
            Self::Regular(declaration) => declaration.generic_count(),
            Self::Builtin(declaration) => declaration.generic_ids.len(),
        }
    }

    #[must_use]
    pub fn is_method(&self) -> bool {
        match self {
            Self::Regular(declaration) => declaration.is_method,
            Self::Builtin(_declaration) => false,
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
