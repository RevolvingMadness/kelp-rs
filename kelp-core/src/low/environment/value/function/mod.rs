use crate::{
    parameter_types_iter::{take_second, ParameterTypesIter},
    semantic::pattern::SemanticPattern,
};
use crate::low::environment::value::function::{
    builtin::{BuiltinFunctionDeclaration, BuiltinFunctionId},
    regular::{RegularFunctionDeclaration, RegularFunctionId},
};
use crate::low::data_type::DataType;

pub mod builtin;
pub mod regular;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FunctionId(pub u32);

impl From<RegularFunctionId> for FunctionId {
    fn from(value: RegularFunctionId) -> Self {
        Self(value.0)
    }
}

impl From<BuiltinFunctionId> for FunctionId {
    fn from(value: BuiltinFunctionId) -> Self {
        Self(value.0)
    }
}

#[derive(Debug, Clone)]
pub enum FunctionDeclaration {
    Regular(RegularFunctionDeclaration),
    Builtin(BuiltinFunctionDeclaration),
}

impl From<RegularFunctionDeclaration> for FunctionDeclaration {
    fn from(value: RegularFunctionDeclaration) -> Self {
        Self::Regular(value)
    }
}

impl From<BuiltinFunctionDeclaration> for FunctionDeclaration {
    fn from(value: BuiltinFunctionDeclaration) -> Self {
        Self::Builtin(value)
    }
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
    pub fn parameter_types(&self) -> ParameterTypesIter<'_, SemanticPattern, DataType> {
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
