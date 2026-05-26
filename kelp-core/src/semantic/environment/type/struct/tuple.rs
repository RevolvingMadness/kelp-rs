use crate::semantic::data_type::SemanticDataType;
use crate::semantic::environment::r#type::HighGenericId;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HighTupleStructId(pub u32);

#[derive(Debug, Clone)]
pub struct SemanticTupleStructDeclaration {
    pub name: String,
    pub generic_ids: Vec<HighGenericId>,
    pub field_types: Vec<SemanticDataType>,
}
