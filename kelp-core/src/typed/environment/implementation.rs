use std::collections::HashMap;

use crate::typed::{
    data_type::SemanticDataType,
    environment::{r#type::HighTypeId, value::HighValueId},
};

#[derive(Debug, Clone)]
pub struct TypedImplementation {
    pub generic_names: Vec<String>,
    pub target_type: SemanticDataType,
    pub types: HashMap<String, HighTypeId>,
    pub values: HashMap<String, HighValueId>,
}
