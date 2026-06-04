use std::collections::HashMap;

use crate::semantic::{
    data_type::SemanticDataType,
    environment::{r#type::HighTypeId, value::HighValueId},
};

#[derive(Debug, Clone)]
pub struct SemanticImplementation {
    target_type: SemanticDataType,
    generic_names: Vec<String>,
    types: HashMap<String, HighTypeId>,
    values: HashMap<String, HighValueId>,
}

impl SemanticImplementation {
    #[inline]
    #[must_use]
    pub const fn new(
        generic_names: Vec<String>,
        target_type: SemanticDataType,
        types: HashMap<String, HighTypeId>,
        values: HashMap<String, HighValueId>,
    ) -> Self {
        Self {
            target_type,
            generic_names,
            types,
            values,
        }
    }

    #[inline]
    #[must_use]
    pub const fn get_target_type(&self) -> &SemanticDataType {
        &self.target_type
    }

    #[must_use]
    pub fn get_type(&self, name: &str) -> Option<HighTypeId> {
        self.types.get(name).copied()
    }

    #[must_use]
    pub fn get_value(&self, name: &str) -> Option<HighValueId> {
        self.values.get(name).copied()
    }
}
