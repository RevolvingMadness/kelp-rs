use crate::span::Span;

#[derive(Debug, Clone)]
pub struct SemanticGenericDeclaration {
    pub name_span: Span,
    pub name: String,
}
