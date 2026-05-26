use crate::{
    parsed::environment::r#type::module::ParsedModuleDeclaration,
    semantic::environment::{r#type::HighTypeId, value::HighValueId},
};
use std::collections::HashMap;

#[derive(Debug, Default, Clone)]
pub struct Scope {
    types: HashMap<String, HighTypeId>,
    values: HashMap<String, HighValueId>,
}

impl Scope {
    #[inline]
    #[must_use]
    pub fn into_tuple(self) -> (HashMap<String, HighTypeId>, HashMap<String, HighValueId>) {
        (self.types, self.values)
    }

    #[inline]
    pub fn declare_type(&mut self, name: String, id: HighTypeId) {
        self.types.insert(name, id);
    }

    #[inline]
    #[must_use]
    pub fn type_is_declared(&self, name: &str) -> bool {
        self.types.contains_key(name)
    }

    #[inline]
    #[must_use]
    pub fn get_type_id(&self, name: &str) -> Option<HighTypeId> {
        self.types.get(name).copied()
    }

    #[inline]
    pub fn declare_value(&mut self, name: String, id: HighValueId) {
        self.values.insert(name, id);
    }

    #[inline]
    #[must_use]
    pub fn value_is_declared(&self, name: &str) -> bool {
        self.values.contains_key(name)
    }

    #[inline]
    #[must_use]
    pub fn get_value_id(&self, name: &str) -> Option<HighValueId> {
        self.values.get(name).copied()
    }

    #[inline]
    #[must_use]
    pub fn into_module_declaration(self, module_name: String) -> ParsedModuleDeclaration {
        ParsedModuleDeclaration {
            name: module_name,
            types: self.types,
            values: self.values,
        }
    }
}
