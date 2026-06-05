use crate::{make_id, semantic::environment::r#type::HighTypeId, span::Span};

make_id!(HighGenericId);

impl From<HighGenericId> for HighTypeId {
    fn from(value: HighGenericId) -> Self {
        Self(value.0)
    }
}

#[derive(Debug, Clone)]
pub struct SemanticGenericDeclaration {
    pub name_span: Span,
    pub name: String,
}
