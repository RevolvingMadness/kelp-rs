use crate::span::Span;

#[derive(Debug, Clone)]
pub struct ParsedGenericDeclaration {
    pub span: Span,
    pub name: String,
}
