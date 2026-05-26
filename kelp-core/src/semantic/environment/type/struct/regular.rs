use std::collections::HashMap;

use crate::semantic::data_type::SemanticDataType;
use crate::semantic::environment::r#type::r#struct::HighStructId;
use crate::semantic::environment::r#type::{HighGenericId, HighTypeId};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HighRegularStructId(pub u32);

impl From<HighRegularStructId> for HighStructId {
    fn from(value: HighRegularStructId) -> Self {
        Self(value.0)
    }
}

impl From<HighRegularStructId> for HighTypeId {
    fn from(value: HighRegularStructId) -> Self {
        Self(value.0)
    }
}

#[derive(Debug, Clone)]
pub struct SemanticRegularStructDeclaration {
    pub name: String,
    pub generic_ids: Vec<HighGenericId>,
    pub field_types: HashMap<String, SemanticDataType>,
}
