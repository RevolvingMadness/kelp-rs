use std::collections::HashMap;

use crate::semantic::{
    data_type::SemanticDataType,
    environment::{r#type::HighTypeId, value::HighValueId},
};

#[derive(Debug, Clone)]
pub struct SemanticImplementation {
    pub generic_names: Vec<String>,
    pub target_type: SemanticDataType,
    pub types: HashMap<String, HighTypeId>,
    pub values: HashMap<String, HighValueId>,
}
