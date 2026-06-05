use crate::semantic::data_type::SemanticDataType;
use crate::span::Span;

#[derive(Debug, Clone)]
pub struct SemanticTypedPathSegment {
    pub name: String,
    pub name_span: Span,
    pub generic_spans: Vec<Span>,
    pub generic_types: Vec<SemanticDataType>,
}

#[derive(Debug, Clone)]
pub struct SemanticTypedPath {
    pub span: Span,
    pub segments: Vec<SemanticTypedPathSegment>,
}
