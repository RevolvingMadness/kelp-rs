use std::collections::HashMap;

use crate::{
    high::environment::{r#type::HighTypeId, value::HighValueId},
    visibility::Visibility,
};

#[derive(Debug, Clone)]
pub struct BasicHighItemDeclaration<I> {
    pub module_path: Vec<String>,
    pub visibility: Visibility,
    pub id: I,
}

pub type BasicHighTypeDeclaration = BasicHighItemDeclaration<HighTypeId>;
pub type BasicHighValueDeclaration = BasicHighItemDeclaration<HighValueId>;

impl<T> BasicHighItemDeclaration<T> {
    #[must_use]
    pub fn is_visible(&self, current_module_path: &[String]) -> bool {
        if matches!(self.visibility, Visibility::Public) {
            return true;
        }

        current_module_path.starts_with(&self.module_path)
    }
}

#[derive(Debug, Clone, Default)]
pub struct NamesEnvironment {
    types: HashMap<HighTypeId, BasicHighTypeDeclaration>,
    values: HashMap<HighValueId, BasicHighValueDeclaration>,
}

impl NamesEnvironment {
    #[inline]
    pub fn declare_type(&mut self, id: HighTypeId, declaration: BasicHighTypeDeclaration) {
        self.types.insert(id, declaration);
    }

    #[inline]
    pub fn get_type(&mut self, id: HighTypeId) -> &BasicHighTypeDeclaration {
        self.types.get(&id).unwrap()
    }

    #[inline]
    pub fn declare_value(&mut self, id: HighValueId, declaration: BasicHighValueDeclaration) {
        self.values.insert(id, declaration);
    }

    #[inline]
    pub fn get_value(&mut self, id: HighValueId) -> &BasicHighValueDeclaration {
        self.values.get(&id).unwrap()
    }
}
