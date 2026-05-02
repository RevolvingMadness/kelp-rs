use std::collections::HashMap;

use crate::high::environment::{r#type::HighTypeId, value::HighValueId};

#[derive(Debug, Default, Clone)]
pub struct Scope {
    pub types: HashMap<String, HighTypeId>,
    pub values: HashMap<String, HighValueId>,
}

impl Scope {
    #[inline]
    pub fn declare_type(&mut self, name: String, id: HighTypeId) {
        self.types.insert(name, id);
    }

    #[inline]
    pub fn declare_value(&mut self, name: String, id: HighValueId) {
        self.values.insert(name, id);
    }
}
