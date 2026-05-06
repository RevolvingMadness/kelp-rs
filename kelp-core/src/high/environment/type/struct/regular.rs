use std::collections::HashMap;

use crate::low::data_type::unresolved::UnresolvedDataType;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HighStructStructId(pub usize);

#[derive(Debug, Clone)]
pub struct HighStructStructDeclaration {
    pub name: String,
    pub generic_names: Vec<String>,
    pub field_types: HashMap<String, UnresolvedDataType>,
}
