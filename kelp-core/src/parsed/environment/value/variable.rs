use crate::typed::environment::value::variable::SemanticVariableDeclaration;

#[derive(Debug, Clone)]
pub struct ParsedVariableDeclaration {
    pub name: String,
}

impl From<SemanticVariableDeclaration> for ParsedVariableDeclaration {
    fn from(value: SemanticVariableDeclaration) -> Self {
        Self { name: value.name }
    }
}
