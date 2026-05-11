use std::collections::HashMap;

use crate::low::data_type::unresolved::UnresolvedDataType;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HighRegularStructId(pub u32);

#[derive(Debug, Clone)]
pub struct HighRegularStructDeclaration {
    pub name: String,
    pub generic_names: Vec<String>,
    pub field_types: HashMap<String, UnresolvedDataType>,
}
