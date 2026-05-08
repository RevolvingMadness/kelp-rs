use std::collections::HashMap;

use crate::high::environment::{r#type::HighTypeId, value::HighValueId};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HighModuleId(pub u32);

#[derive(Debug, Clone)]
pub struct HighModuleDeclaration {
    pub name: String,
    pub types: HashMap<String, HighTypeId>,
    pub values: HashMap<String, HighValueId>,
}

impl HighModuleDeclaration {
    #[inline]
    #[must_use]
    pub fn get_type_id(&self, name: &str) -> Option<HighTypeId> {
        self.types.get(name).copied()
    }

    #[inline]
    #[must_use]
    pub fn get_value_id(&self, name: &str) -> Option<HighValueId> {
        self.values.get(name).copied()
    }
}
