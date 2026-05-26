use crate::{
    parsed::environment::value::{
        function::ParsedFunctionDeclaration, variable::ParsedVariableDeclaration,
    },
    typed::environment::value::SemanticValueDeclarationKind,
    visibility::Visibility,
};

pub mod function;
pub mod variable;

#[derive(Debug, Clone)]
pub enum ParsedValueDeclarationKind {
    Variable(ParsedVariableDeclaration),
    Function(Box<ParsedFunctionDeclaration>),
}

impl From<SemanticValueDeclarationKind> for ParsedValueDeclarationKind {
    fn from(value: SemanticValueDeclarationKind) -> Self {
        match value {
            SemanticValueDeclarationKind::Variable(declaration) => {
                Self::Variable(declaration.into())
            }
            SemanticValueDeclarationKind::Function(declaration) => {
                Self::Function(Box::new((*declaration).into()))
            }
        }
    }
}

impl ParsedValueDeclarationKind {
    #[must_use]
    pub fn name(&self) -> &str {
        match self {
            Self::Variable(declaration) => &declaration.name,
            Self::Function(declaration) => declaration.name(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ParsedValueDeclaration {
    pub visibility: Visibility,
    pub module_path: Vec<String>,
    pub kind: ParsedValueDeclarationKind,
}

impl ParsedValueDeclaration {
    #[must_use]
    pub fn is_visible(&self, current_module_path: &[String]) -> bool {
        if matches!(self.visibility, Visibility::Public) {
            return true;
        }

        current_module_path.starts_with(&self.module_path)
    }
}
