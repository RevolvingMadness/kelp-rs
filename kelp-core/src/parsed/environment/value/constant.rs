use crate::{semantic::environment::value::constant::SemanticConstantDeclaration, span::Span};

#[derive(Debug, Clone)]
pub struct ParsedConstantDeclaration {
    pub name_span: Span,
    pub name: String,
}

impl From<SemanticConstantDeclaration> for ParsedConstantDeclaration {
    fn from(value: SemanticConstantDeclaration) -> Self {
        Self {
            name_span: value.name_span,
            name: value.name,
        }
    }
}
