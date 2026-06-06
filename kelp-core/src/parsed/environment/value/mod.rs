use crate::parsed::environment::value::constant::ParsedConstantDeclaration;
use crate::parsed::environment::value::{
    function::ParsedFunctionDeclaration, variable::ParsedVariableDeclaration,
};
use crate::parsed::semantic_analysis::info::error::ValueKind;
use crate::semantic::environment::r#type::module::HighModuleId;
use crate::semantic::environment::value::SemanticValueDeclaration;
use crate::span::Span;
use crate::visibility::Visibility;

pub mod constant;
pub mod function;
pub mod variable;

#[derive(Debug, Clone)]
pub enum ParsedValueDeclarationKind {
    Variable(ParsedVariableDeclaration),
    Constant(ParsedConstantDeclaration),
    Function(ParsedFunctionDeclaration),
}

impl From<SemanticValueDeclaration> for ParsedValueDeclarationKind {
    fn from(value: SemanticValueDeclaration) -> Self {
        match value {
            SemanticValueDeclaration::Variable(declaration) => Self::Variable(declaration.into()),
            SemanticValueDeclaration::Constant(declaration) => Self::Constant(declaration.into()),
            SemanticValueDeclaration::Function(declaration) => Self::Function(declaration.into()),
        }
    }
}

impl ParsedValueDeclarationKind {
    #[must_use]
    pub const fn get_kind(&self) -> ValueKind {
        match self {
            Self::Variable(..) => ValueKind::Variable,
            Self::Constant(..) => ValueKind::Constant,
            Self::Function(..) => ValueKind::Function,
        }
    }

    #[must_use]
    pub fn name_span(&self) -> Option<Span> {
        match self {
            Self::Variable(declaration) => Some(declaration.name_span),
            Self::Constant(declaration) => Some(declaration.name_span),
            Self::Function(declaration) => declaration.name_span(),
        }
    }

    #[must_use]
    pub fn name(&self) -> &str {
        match self {
            Self::Variable(declaration) => &declaration.name,
            Self::Constant(declaration) => &declaration.name,
            Self::Function(declaration) => declaration.name(),
        }
    }

    #[must_use]
    pub fn generic_count(&self) -> usize {
        match self {
            Self::Variable(..) => 0,
            Self::Constant(..) => 0,
            Self::Function(declaration) => declaration.generic_count(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ParsedValueDeclaration {
    pub visibility: Visibility,
    pub module_path: Vec<HighModuleId>,
    pub kind: ParsedValueDeclarationKind,
}

impl ParsedValueDeclaration {
    #[must_use]
    pub fn is_visible(&self, current_module_path: &[HighModuleId]) -> bool {
        if matches!(self.visibility, Visibility::Public) {
            return true;
        }

        current_module_path.starts_with(&self.module_path)
    }
}
