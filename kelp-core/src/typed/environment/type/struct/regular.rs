use std::collections::HashMap;

use crate::typed::{data_type::unresolved::SemanticDataType, environment::r#type::HighGenericId};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HighRegularStructId(pub u32);

#[derive(Debug, Clone)]
pub struct SemanticRegularStructDeclaration {
    pub name: String,
    pub generic_ids: Vec<HighGenericId>,
    pub field_types: HashMap<String, SemanticDataType>,
}
