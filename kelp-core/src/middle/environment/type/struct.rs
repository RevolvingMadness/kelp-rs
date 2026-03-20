use std::collections::HashMap;

use crate::middle::data_type::DataType;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StructId(pub usize);

#[derive(Debug, Clone)]
pub struct StructDeclaration {
    pub name: String,
    pub generic_types: Vec<DataType>,
    pub field_types: HashMap<String, DataType>,
}
