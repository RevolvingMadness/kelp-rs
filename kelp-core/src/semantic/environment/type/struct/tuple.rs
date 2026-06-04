use crate::make_id;
use crate::semantic::data_type::SemanticDataType;
use crate::semantic::environment::r#type::r#struct::HighStructId;
use crate::semantic::environment::r#type::{HighGenericId, HighTypeId};

make_id!(HighTupleStructId);

impl From<HighTupleStructId> for HighStructId {
    fn from(value: HighTupleStructId) -> Self {
        Self(value.0)
    }
}

impl From<HighTupleStructId> for HighTypeId {
    fn from(value: HighTupleStructId) -> Self {
        Self(value.0)
    }
}

#[derive(Debug, Clone)]
pub struct SemanticTupleStructDeclaration {
    pub name: String,
    pub generic_ids: Vec<HighGenericId>,
    pub field_types: Vec<SemanticDataType>,
}
