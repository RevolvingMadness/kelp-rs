use crate::{path::regular::Path, span::Span};

#[derive(Debug, Clone)]
pub enum UseTree {
    Wildcard(Path),
    Group(Option<Path>, Vec<Self>),
    As(Path, Span, String),
    Path(Path),
}
