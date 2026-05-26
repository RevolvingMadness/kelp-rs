use std::collections::HashMap;

use crate::{
    parsed::environment::{
        resolved::{r#type::HighTypeId, value::HighValueId},
        unresolved::{r#type::ParsedTypeDeclaration, value::ParsedValueDeclaration},
    },
    typed::data_type::unresolved::SemanticDataType,
};

pub mod r#type;
pub mod value;

#[derive(Debug, Clone)]
pub struct HighImpl {
    pub generic_names: Vec<String>,
    pub target_type: SemanticDataType,
    pub types: HashMap<String, HighTypeId>,
    pub values: HashMap<String, HighValueId>,
}

#[derive(Debug, Clone, Default)]
pub struct ParsedEnvironment {
    types: Vec<ParsedTypeDeclaration>,
    values: Vec<ParsedValueDeclaration>,

    pub impls: HashMap<HighTypeId, Vec<HighImpl>>,
}

impl ParsedEnvironment {
    #[must_use]
    pub fn declare_type(&mut self, declaration: ParsedTypeDeclaration) -> HighTypeId {
        let id = HighTypeId(self.types.len() as u32);

        self.types.push(declaration);

        id
    }

    #[inline]
    #[must_use]
    pub fn get_type(&self, id: HighTypeId) -> &ParsedTypeDeclaration {
        &self.types[id.0 as usize]
    }

    #[must_use]
    pub fn declare_value(&mut self, declaration: ParsedValueDeclaration) -> HighValueId {
        let id = HighValueId(self.values.len() as u32);

        self.values.push(declaration);

        id
    }

    #[inline]
    #[must_use]
    pub fn get_value(&self, id: HighValueId) -> &ParsedValueDeclaration {
        &self.values[id.0 as usize]
    }
}
