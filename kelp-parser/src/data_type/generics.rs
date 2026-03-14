use kelp_core::data_type::high::DataType;

use crate::{
    cst::{CSTGenericDataTypes, CSTGenericNames},
    data_type::{lower_data_type, try_parse_data_type},
    parser::Parser,
    syntax::SyntaxKind,
};

pub fn try_parse_generic_names(parser: &mut Parser) -> bool {
    if parser.peek_char() != Some('<') {
        return false;
    }

    parser.start_node(SyntaxKind::GenericNames);

    parser.bump_char();
    parser.skip_inline_whitespace();

    while parser.peek_char() != Some('>') && parser.peek_char().is_some() {
        if !parser
            .expect_identifier_kind(SyntaxKind::DataTypeName, "Expected generic argument name")
        {
            break;
        }

        parser.skip_inline_whitespace();

        if parser.try_bump_char(',') {
            parser.skip_inline_whitespace();
        } else {
            break;
        }
    }

    parser.expect_char('>', "Expected closing angle bracket '>'");

    parser.finish_node();

    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_generic_names(node: CSTGenericNames) -> Option<Vec<String>> {
    Some(
        node.generics()
            .map(|token| token.text().to_owned())
            .collect(),
    )
}

#[must_use]
pub fn try_parse_generic_data_types(parser: &mut Parser) -> bool {
    if parser.peek_char() != Some('<') {
        return false;
    }

    parser.start_node(SyntaxKind::GenericDataTypes);

    parser.bump_char();
    parser.skip_inline_whitespace();

    while parser.peek_char() != Some('>') && parser.peek_char().is_some() {
        if !try_parse_data_type(parser) {
            parser.error("Expected data type in generic arguments");

            break;
        }

        parser.skip_inline_whitespace();

        if parser.try_bump_char(',') {
            parser.skip_inline_whitespace();
        } else {
            break;
        }
    }

    parser.expect_char('>', "Expected closing angle bracket '>'");

    parser.finish_node();

    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_generic_data_types(node: CSTGenericDataTypes) -> Option<Vec<DataType>> {
    Some(node.generics().filter_map(lower_data_type).collect())
}
