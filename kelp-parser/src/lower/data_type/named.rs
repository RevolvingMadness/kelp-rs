use kelp_core::data_type::high::{HighDataType, HighDataTypeKind};

use crate::{
    cst::CSTNamedDataType,
    lower::data_type::generics::{lower_generic_data_types, try_parse_generic_data_types},
    parser::Parser,
    span::{span_of_cst_node, span_of_syntax_token},
    syntax::SyntaxKind,
};

pub fn try_parse_named_data_type(parser: &mut Parser) -> bool {
    let checkpoint = parser.checkpoint();
    parser.start_node_at(checkpoint, SyntaxKind::NamedDataType);

    if let Some(text) = parser.peek_identifier() {
        parser.add_token(SyntaxKind::DataTypeName, text.len());
    }

    let _ = try_parse_generic_data_types(parser);

    parser.finish_node();
    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_named_data_type(node: CSTNamedDataType) -> Option<HighDataType> {
    let span = span_of_cst_node(&node);

    let name_token = node.data_type_name_token()?;
    let name_span = span_of_syntax_token(&name_token);
    let name = name_token.text();

    let generics = node.generic_data_types().and_then(lower_generic_data_types);

    Some(
        HighDataTypeKind::Named(name_span, name.to_string(), generics.unwrap_or_default())
            .with_span(span),
    )
}
