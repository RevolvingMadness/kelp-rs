use crate::span::Span;

#[derive(Debug, Clone)]
pub struct ParsedGenericDeclaration {
    pub name_span: Span,
    pub name: String,
}
