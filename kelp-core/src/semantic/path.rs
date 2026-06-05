use crate::span::Span;

#[derive(Debug, Clone)]
pub struct ParsedPathSegment {
    pub name: String,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ParsedPath {
    pub span: Span,
    pub segments: Vec<ParsedPathSegment>,
}
