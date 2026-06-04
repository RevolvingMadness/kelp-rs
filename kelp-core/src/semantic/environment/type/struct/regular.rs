use std::collections::HashMap;

use crate::make_id;
use crate::semantic::data_type::SemanticDataType;
use crate::semantic::environment::r#type::r#struct::HighStructId;
use crate::semantic::environment::r#type::{HighGenericId, HighVisibleTypeId};
use crate::span::Span;

make_id!(HighRegularStructId);

impl From<HighRegularStructId> for HighStructId {
    fn from(value: HighRegularStructId) -> Self {
        Self(value.0)
    }
}

impl From<HighRegularStructId> for HighVisibleTypeId {
    fn from(value: HighRegularStructId) -> Self {
        Self(value.0)
    }
}

#[derive(Debug, Clone)]
pub struct SemanticRegularStructDeclaration {
    pub name_span: Span,
    pub name: String,
    pub generic_ids: Vec<HighGenericId>,
    pub field_types: HashMap<String, SemanticDataType>,
}
