use crate::{
    parsed::environment::resolved::r#type::HighGenericId,
    typed::data_type::unresolved::SemanticDataType,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HighTupleStructId(pub u32);

#[derive(Debug, Clone)]
pub struct SemanticTupleStructDeclaration {
    pub name: String,
    pub generic_ids: Vec<HighGenericId>,
    pub field_types: Vec<SemanticDataType>,
}
