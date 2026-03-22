use crate::span::Span;

#[derive(Debug, Clone)]
pub struct PathSegment {
    pub name: String,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Path {
    pub span: Span,
    pub segments: Vec<PathSegment>,
}
