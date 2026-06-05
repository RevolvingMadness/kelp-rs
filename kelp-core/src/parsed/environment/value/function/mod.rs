pub mod builtin;
pub mod regular;

use crate::parsed::environment::value::function::{
    builtin::ParsedBuiltinFunctionDeclaration, regular::ParsedRegularFunctionDeclaration,
};
use crate::semantic::environment::value::function::SemanticFunctionDeclaration;
use crate::span::Span;

#[derive(Debug, Clone)]
pub enum ParsedFunctionDeclaration {
    Regular(ParsedRegularFunctionDeclaration),
    Builtin(ParsedBuiltinFunctionDeclaration),
}

impl From<SemanticFunctionDeclaration> for ParsedFunctionDeclaration {
    fn from(value: SemanticFunctionDeclaration) -> Self {
        match value {
            SemanticFunctionDeclaration::Regular(declaration) => Self::Regular(declaration.into()),
            SemanticFunctionDeclaration::Builtin(declaration) => Self::Builtin(declaration.into()),
        }
    }
}

impl ParsedFunctionDeclaration {
    #[must_use]
    pub fn name(&self) -> &str {
        match self {
            Self::Regular(declaration) => &declaration.name,
            Self::Builtin(declaration) => &declaration.name,
        }
    }

    #[must_use]
    pub const fn name_span(&self) -> Option<Span> {
        match self {
            Self::Regular(declaration) => Some(declaration.name_span),
            Self::Builtin(..) => None,
        }
    }
}
