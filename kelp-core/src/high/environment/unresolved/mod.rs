use std::collections::HashMap;

use crate::{
    high::environment::{
        resolved::{r#type::HighTypeId, value::HighValueId},
        unresolved::{r#type::UnresolvedTypeDeclaration, value::UnresolvedValueDeclaration},
    },
    low::data_type::unresolved::UnresolvedDataType,
};

pub mod r#type;
pub mod value;

#[derive(Debug, Clone)]
pub struct HighImpl {
    pub generic_names: Vec<String>,
    pub target_type: UnresolvedDataType,
    pub types: HashMap<String, HighTypeId>,
    pub values: HashMap<String, HighValueId>,
}

#[derive(Debug, Clone, Default)]
pub struct UnresolvedEnvironment {
    types: Vec<UnresolvedTypeDeclaration>,
    values: Vec<UnresolvedValueDeclaration>,

    pub impls: HashMap<HighTypeId, Vec<HighImpl>>,
}

impl UnresolvedEnvironment {
    #[must_use]
    pub fn declare_type(&mut self, declaration: UnresolvedTypeDeclaration) -> HighTypeId {
        let id = HighTypeId(self.types.len() as u32);

        self.types.push(declaration);

        id
    }

    #[inline]
    #[must_use]
    pub fn get_type(&self, id: HighTypeId) -> &UnresolvedTypeDeclaration {
        &self.types[id.0 as usize]
    }

    #[must_use]
    pub fn declare_value(&mut self, declaration: UnresolvedValueDeclaration) -> HighValueId {
        let id = HighValueId(self.values.len() as u32);

        self.values.push(declaration);

        id
    }

    #[inline]
    #[must_use]
    pub fn get_value(&self, id: HighValueId) -> &UnresolvedValueDeclaration {
        &self.values[id.0 as usize]
    }
}
