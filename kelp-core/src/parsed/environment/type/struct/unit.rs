use crate::semantic::environment::r#type::r#struct::unit::SemanticUnitStructDeclaration;
use crate::span::Span;

#[derive(Debug, Clone)]
pub struct ParsedUnitStructDeclaration {
    pub name_span: Span,
    pub name: String,
}

impl From<SemanticUnitStructDeclaration> for ParsedUnitStructDeclaration {
    fn from(value: SemanticUnitStructDeclaration) -> Self {
        Self {
            name_span: value.name_span,
            name: value.name,
        }
    }
}

impl ParsedUnitStructDeclaration {
    #[inline]
    #[must_use]
    pub fn name(&self) -> &str {
        &self.name
    }
}
