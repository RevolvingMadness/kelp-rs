use std::collections::HashMap;

use crate::high::data_type::resolved::PartiallyResolvedDataType;

#[derive(Debug, Clone)]
pub struct HighStructDeclaration {
    pub name: String,
    pub generic_names: Vec<String>,
    pub field_types: HashMap<String, Option<PartiallyResolvedDataType>>,
}
