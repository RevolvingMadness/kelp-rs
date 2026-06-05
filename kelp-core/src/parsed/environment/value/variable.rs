use crate::{semantic::environment::value::variable::SemanticVariableDeclaration, span::Span};

#[derive(Debug, Clone)]
pub struct ParsedVariableDeclaration {
    pub name_span: Span,
    pub name: String,
}

impl From<SemanticVariableDeclaration> for ParsedVariableDeclaration {
    fn from(value: SemanticVariableDeclaration) -> Self {
        Self {
            name_span: value.name_span,
            name: value.name,
        }
    }
}
