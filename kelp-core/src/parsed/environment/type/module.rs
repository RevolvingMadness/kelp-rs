use crate::{
    semantic::environment::{
        r#type::{HighTypeId, module::SemanticModuleDeclaration},
        value::HighValueId,
    },
    span::Span,
};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct ParsedModuleDeclaration {
    pub name_span: Option<Span>,
    pub name: String,
    pub types: HashMap<String, HighTypeId>,
    pub values: HashMap<String, HighValueId>,
}

impl From<SemanticModuleDeclaration> for ParsedModuleDeclaration {
    fn from(value: SemanticModuleDeclaration) -> Self {
        Self {
            name_span: value.name_span,
            name: value.name,
            types: value.types,
            values: value.values,
        }
    }
}
