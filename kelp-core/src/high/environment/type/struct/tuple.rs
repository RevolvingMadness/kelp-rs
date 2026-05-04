use crate::low::data_type::DataType;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HighTupleStructId(pub usize);

#[derive(Debug, Clone)]
pub struct HighTupleStructDeclaration {
    pub name: String,
    pub generic_names: Vec<String>,
    pub field_types: Vec<Option<DataType>>,
}
