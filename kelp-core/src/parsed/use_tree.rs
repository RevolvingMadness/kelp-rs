use crate::{semantic::path::ParsedPath, span::Span};

#[derive(Debug, Clone)]
pub enum UseTree {
    Wildcard(ParsedPath),
    Group(Option<ParsedPath>, Vec<Self>),
    As(ParsedPath, Span, String),
    Path(ParsedPath),
}
