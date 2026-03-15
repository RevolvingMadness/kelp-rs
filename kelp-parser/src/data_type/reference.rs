use kelp_core::high::data_type::{DataType, DataTypeKind};

use crate::{
    cst::CSTReferenceDataType,
    data_type::{lower_data_type, try_parse_data_type},
    parser::Parser,
    span::span_of_cst_node,
    syntax::SyntaxKind,
};

#[must_use]
pub fn try_parse_reference_data_type(parser: &mut Parser) -> bool {
    let checkpoint = parser.checkpoint();
    parser.start_node_at(checkpoint, SyntaxKind::ReferenceDataType);

    parser.bump_char();

    if !try_parse_data_type(parser) {
        parser.recover_newline("Expected data type after '&'");
    }

    parser.finish_node();
    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_reference_data_type(node: CSTReferenceDataType) -> Option<DataType> {
    let span = span_of_cst_node(&node);

    let data_type = lower_data_type(node.data_type()?)?;

    Some(DataTypeKind::Reference(Box::new(data_type)).with_span(span))
}
