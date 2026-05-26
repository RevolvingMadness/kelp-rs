pub mod builtin;
pub mod regular;

use crate::semantic::environment::value::function::ResolvedFunctionDeclaration;
use crate::parsed::environment::value::function::{
    builtin::UnresolvedBuiltinFunctionDeclaration,
    regular::UnresolvedRegularFunctionDeclaration,
};

#[derive(Debug, Clone)]
pub enum UnresolvedFunctionDeclaration {
    Regular(UnresolvedRegularFunctionDeclaration),
    Builtin(UnresolvedBuiltinFunctionDeclaration),
}

impl From<ResolvedFunctionDeclaration> for UnresolvedFunctionDeclaration {
    fn from(value: ResolvedFunctionDeclaration) -> Self {
        match value {
            ResolvedFunctionDeclaration::Regular(declaration) => Self::Regular(declaration.into()),
            ResolvedFunctionDeclaration::Builtin(declaration) => Self::Builtin(declaration.into()),
        }
    }
}

impl UnresolvedFunctionDeclaration {
    #[must_use]
    pub fn name(&self) -> &str {
        match self {
            Self::Regular(declaration) => &declaration.name,
            Self::Builtin(declaration) => &declaration.name,
        }
    }
}
