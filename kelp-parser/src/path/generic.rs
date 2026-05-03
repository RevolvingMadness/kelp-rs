use kelp_core::{
    high::data_type::unresolved::UnresolvedDataType,
    path::generic::{GenericPath, GenericPathSegment},
};

use crate::{
    cst::{CSTGenericPath, CSTGenericPathSegment},
    data_type::generics::{lower_generic_data_types, try_parse_generic_data_types},
    parser::Parser,
    span::{span_of_cst_node, text_range_to_span},
    syntax::SyntaxKind,
};

#[must_use]
fn try_parse_generic_path_segment(parser: &mut Parser, is_type: bool) -> bool {
    let checkpoint = parser.checkpoint();

    if !parser.try_bump_identifier_kind(SyntaxKind::PathIdentifier) {
        return false;
    }

    parser.start_node_at(checkpoint, SyntaxKind::GenericPathSegment);

    let state = parser.save_state();

    if is_type {
        parser.try_bump_str("::", SyntaxKind::ColonColon);

        if !try_parse_generic_data_types(parser) {
            parser.restore_state(state);
        }
    } else if parser.try_bump_str("::", SyntaxKind::ColonColon)
        && !try_parse_generic_data_types(parser)
    {
        parser.restore_state(state);
    }

    parser.finish_node();

    true
}

#[must_use]
pub fn try_parse_generic_path(parser: &mut Parser, is_type: bool) -> bool {
    let checkpoint = parser.checkpoint();

    if !try_parse_generic_path_segment(parser, is_type) {
        return false;
    }

    parser.start_node_at(checkpoint, SyntaxKind::GenericPath);

    loop {
        let state = parser.save_state();

        parser.skip_inline_whitespace();

        if !parser.try_bump_str("::", SyntaxKind::ColonColon) {
            parser.restore_state(state);

            break;
        }

        if !try_parse_generic_path_segment(parser, is_type) {
            parser.error("Expected path segment");
        }
    }

    parser.finish_node();

    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
fn lower_generic_path_segment(
    node: CSTGenericPathSegment,
) -> Option<GenericPathSegment<UnresolvedDataType>> {
    let name_token = node.path_identifier_token()?;
    let name_span = text_range_to_span(name_token.text_range());
    let name = name_token.text();

    let (generic_spans, generic_types) = node
        .generic_data_types()
        .and_then(lower_generic_data_types)
        .unwrap_or_default();

    Some(GenericPathSegment {
        name_span,
        name: name.to_owned(),
        generic_spans,
        generic_types,
    })
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_generic_path(node: CSTGenericPath) -> Option<GenericPath<UnresolvedDataType>> {
    let span = span_of_cst_node(&node);

    let segments = node
        .generic_path_segments()
        .filter_map(lower_generic_path_segment)
        .collect();

    Some(GenericPath { span, segments })
}
