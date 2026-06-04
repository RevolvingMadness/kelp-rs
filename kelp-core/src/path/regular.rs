use crate::{
    path::generic::{TypedPath, TypedPathSegment},
    span::Span,
};

#[derive(Debug, Clone)]
pub struct PathSegment {
    pub name: String,
    pub span: Span,
}

impl PathSegment {
    #[must_use]
    pub fn into_typed<T>(self) -> TypedPathSegment<T> {
        TypedPathSegment {
            name: self.name,
            name_span: self.span,
            generic_spans: Vec::new(),
            generic_types: Vec::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Path {
    pub span: Span,
    pub segments: Vec<PathSegment>,
}

impl Path {
    #[must_use]
    pub fn into_typed<T>(self) -> TypedPath<T> {
        TypedPath {
            span: self.span,
            segments: self
                .segments
                .into_iter()
                .map(PathSegment::into_typed)
                .collect(),
        }
    }
}
