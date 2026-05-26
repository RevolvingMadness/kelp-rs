use std::collections::HashMap;

use crate::parsed::environment::resolved::{r#type::HighTypeId, value::HighValueId};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HighModuleId(pub u32);

#[derive(Debug, Clone)]
pub struct ResolvedModuleDeclaration {
    pub name: String,
    pub types: HashMap<String, HighTypeId>,
    pub values: HashMap<String, HighValueId>,
}
