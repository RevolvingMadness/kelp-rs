use std::collections::HashMap;

use crate::{
    high::environment::resolved::r#type::HighGenericId,
    low::data_type::unresolved::UnresolvedDataType,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HighRegularStructId(pub u32);

#[derive(Debug, Clone)]
pub struct ResolvedRegularStructDeclaration {
    pub name: String,
    pub generic_ids: Vec<HighGenericId>,
    pub field_types: HashMap<String, UnresolvedDataType>,
}
