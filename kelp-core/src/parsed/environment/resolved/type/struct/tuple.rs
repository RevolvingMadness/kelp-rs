use crate::{
    parsed::environment::resolved::r#type::HighGenericId,
    typed::data_type::unresolved::UnresolvedDataType,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HighTupleStructId(pub u32);

#[derive(Debug, Clone)]
pub struct ResolvedTupleStructDeclaration {
    pub name: String,
    pub generic_ids: Vec<HighGenericId>,
    pub field_types: Vec<UnresolvedDataType>,
}
