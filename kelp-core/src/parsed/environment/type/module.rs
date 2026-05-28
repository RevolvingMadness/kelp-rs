use crate::semantic::environment::{
    r#type::{HighTypeId, module::SemanticModuleDeclaration},
    value::HighValueId,
};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct ParsedModuleDeclaration {
    pub name: String,
    pub types: HashMap<String, HighTypeId>,
    pub values: HashMap<String, HighValueId>,
}

impl From<SemanticModuleDeclaration> for ParsedModuleDeclaration {
    fn from(value: SemanticModuleDeclaration) -> Self {
        Self {
            name: value.name,
            types: value.types,
            values: value.values,
        }
    }
}
