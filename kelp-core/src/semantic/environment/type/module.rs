use std::collections::HashMap;

use crate::semantic::environment::{r#type::HighTypeId, value::HighValueId};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HighModuleId(pub u32);

impl From<HighModuleId> for HighTypeId {
    fn from(value: HighModuleId) -> Self {
        Self(value.0)
    }
}

#[derive(Debug, Clone)]
pub struct SemanticModuleDeclaration {
    pub name: String,
    pub types: HashMap<String, HighTypeId>,
    pub values: HashMap<String, HighValueId>,
}
