use std::collections::HashMap;

use crate::{high::environment::r#type::HighTypeId, low::environment::value::ValueId};

#[derive(Debug, Clone)]
pub struct HighModuleDeclaration {
    pub name: String,
    pub types: HashMap<String, HighTypeId>,
    pub values: HashMap<String, ValueId>,
}

impl HighModuleDeclaration {
    #[inline]
    #[must_use]
    pub fn get_type_id(&self, name: &str) -> Option<HighTypeId> {
        self.types.get(name).copied()
    }

    #[inline]
    #[must_use]
    pub fn get_value_id(&self, name: &str) -> Option<ValueId> {
        self.values.get(name).copied()
    }
}
