use crate::{high::data_type::DataType, span::Span};

#[derive(Debug, Clone)]
pub struct TypeAliasDeclarationItem {
    pub name_span: Span,
    pub name: String,
    pub generic_names: Vec<String>,
    pub alias: DataType,
}
