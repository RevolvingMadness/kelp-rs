use crate::{
    high::{data_type::DataType, expression::block::BlockExpression, pattern::Pattern},
    span::Span,
};

#[derive(Debug, Clone)]
pub struct FunctionDeclarationItem {
    pub recursive_keyword_span: Option<Span>,
    pub runtime_keyword_span: Option<Span>,
    pub name_span: Span,
    pub name: String,
    pub generic_names: Vec<String>,
    pub parameters: Vec<(Pattern, DataType)>,
    pub return_type: DataType,
    pub body: BlockExpression,
}
