use std::collections::HashMap;

use kelp_core::high::data_type::unresolved::UnresolvedDataType;

use crate::{
    cst::{CSTStructField, CSTStructFields, CSTTupleField, CSTTupleFields},
    data_type::{lower_data_type, try_parse_data_type},
    parser::Parser,
    syntax::SyntaxKind,
};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_struct_field(node: CSTStructField) -> Option<(String, UnresolvedDataType)> {
    let name_token = node.name()?;
    let name = name_token.text();

    let data_type = lower_data_type(node.data_type()?)?;

    Some((name.to_owned(), data_type))
}

#[must_use]
pub fn try_parse_struct_field(parser: &mut Parser) -> bool {
    let checkpoint = parser.checkpoint();

    if !parser.try_bump_identifier_kind(SyntaxKind::StructFieldName) {
        return false;
    }

    parser.start_node_at(checkpoint, SyntaxKind::StructField);

    parser.skip_whitespace();

    let parsed_colon = parser.expect_char(':', "Expected ':'");

    parser.skip_whitespace();

    if !try_parse_data_type(parser) && parsed_colon {
        parser.error("Expected data type");
    }

    parser.finish_node();

    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_struct_fields(node: CSTStructFields) -> Option<HashMap<String, UnresolvedDataType>> {
    let fields = node
        .struct_fields()
        .filter_map(lower_struct_field)
        .collect();

    Some(fields)
}

#[must_use]
pub fn try_parse_struct_fields(parser: &mut Parser) -> bool {
    let checkpoint = parser.checkpoint();

    if !try_parse_struct_field(parser) {
        return false;
    }

    parser.start_node_at(checkpoint, SyntaxKind::StructFields);

    loop {
        let state = parser.save_state();
        parser.skip_whitespace();

        if !parser.try_bump_char(',') {
            parser.restore_state(state);
            break;
        }

        parser.skip_whitespace();

        if !try_parse_struct_field(parser) {
            break;
        }
    }

    parser.finish_node();
    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_tuple_field(node: CSTTupleField) -> Option<UnresolvedDataType> {
    let data_type = lower_data_type(node.data_type()?)?;

    Some(data_type)
}

#[must_use]
pub fn try_parse_tuple_field(parser: &mut Parser) -> bool {
    let checkpoint = parser.checkpoint();

    if !try_parse_data_type(parser) {
        return false;
    }

    parser.start_node_at(checkpoint, SyntaxKind::TupleField);

    parser.finish_node();

    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_tuple_fields(node: CSTTupleFields) -> Option<Vec<UnresolvedDataType>> {
    let fields = node.tuple_fields().filter_map(lower_tuple_field).collect();

    Some(fields)
}

#[must_use]
pub fn try_parse_tuple_fields(parser: &mut Parser) -> bool {
    let checkpoint = parser.checkpoint();

    if !try_parse_tuple_field(parser) {
        return false;
    }

    parser.start_node_at(checkpoint, SyntaxKind::TupleFields);

    loop {
        let state = parser.save_state();
        parser.skip_whitespace();

        if !parser.try_bump_char(',') {
            parser.restore_state(state);
            break;
        }

        parser.skip_whitespace();
        if !try_parse_tuple_field(parser) {
            break;
        }
    }

    parser.finish_node();
    true
}
