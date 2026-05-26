use crate::visibility::Visibility;
use crate::parsed::environment::value::{
    function::UnresolvedFunctionDeclaration, variable::UnresolvedVariableDeclaration,
};
use crate::semantic::environment::value::ResolvedValueDeclarationKind;

pub mod function;
pub mod variable;

#[derive(Debug, Clone)]
pub enum UnresolvedValueDeclarationKind {
    Variable(UnresolvedVariableDeclaration),
    Function(Box<UnresolvedFunctionDeclaration>),
}

impl From<ResolvedValueDeclarationKind> for UnresolvedValueDeclarationKind {
    fn from(value: ResolvedValueDeclarationKind) -> Self {
        match value {
            ResolvedValueDeclarationKind::Variable(declaration) => {
                Self::Variable(declaration.into())
            }
            ResolvedValueDeclarationKind::Function(declaration) => {
                Self::Function(Box::new((*declaration).into()))
            }
        }
    }
}

impl UnresolvedValueDeclarationKind {
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
    pub kind: UnresolvedValueDeclarationKind,
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
