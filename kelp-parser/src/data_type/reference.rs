use kelp_core::high::data_type::unresolved::UnresolvedDataType;

use crate::{
    cst::CSTReferenceDataType,
    data_type::{lower_data_type, try_parse_data_type},
    parser::Parser,
    syntax::SyntaxKind,
};

#[must_use]
pub fn try_parse_reference_data_type(parser: &mut Parser) -> bool {
    let checkpoint = parser.checkpoint();
    parser.start_node_at(checkpoint, SyntaxKind::ReferenceDataType);

    parser.bump_char();

    if !try_parse_data_type(parser) {
        parser.recover_not_whitespace("Expected data type after '&'");
    }

    parser.finish_node();
    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_reference_data_type(node: CSTReferenceDataType) -> Option<UnresolvedDataType> {
    let data_type = lower_data_type(node.data_type()?)?;

    Some(UnresolvedDataType::Reference(Box::new(data_type)))
}
