use crate::make_id;
use crate::semantic::environment::r#type::HighTypeId;
use crate::semantic::environment::r#type::r#struct::HighStructId;
use crate::span::Span;

make_id!(HighUnitStructId);

impl From<HighUnitStructId> for HighStructId {
    fn from(value: HighUnitStructId) -> Self {
        Self(value.0)
    }
}

impl From<HighUnitStructId> for HighTypeId {
    fn from(value: HighUnitStructId) -> Self {
        Self(value.0)
    }
}

#[derive(Debug, Clone)]
pub struct SemanticUnitStructDeclaration {
    pub name_span: Span,
    pub name: String,
}
