use kelp_core::{
    high::data_type::unresolved::UnresolvedDataType,
    path::{Path, PathSegment},
};

use crate::{
    cst::{CSTPath, CSTPathSegment},
    data_type::generics::{lower_generic_data_types, try_parse_generic_data_types},
    parser::Parser,
    span::span_of_cst_node,
    syntax::SyntaxKind,
};

#[must_use]
pub fn try_parse_path_segment(parser: &mut Parser, is_type: bool) -> bool {
    let checkpoint = parser.checkpoint();

    if !parser.try_bump_identifier_kind(SyntaxKind::PathIdentifier) {
        return false;
    }

    parser.start_node_at(checkpoint, SyntaxKind::PathSegment);

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
pub fn try_parse_path(parser: &mut Parser, is_type: bool) -> bool {
    let checkpoint = parser.checkpoint();

    if !try_parse_path_segment(parser, is_type) {
        return false;
    }

    parser.start_node_at(checkpoint, SyntaxKind::Path);

    loop {
        parser.skip_inline_whitespace();

        if !parser.try_bump_str("::", SyntaxKind::ColonColon) {
            break;
        }

        if !try_parse_path_segment(parser, is_type) {
            parser.error("Expected path segment");
        }
    }

    parser.finish_node();
    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_path_segment(node: CSTPathSegment) -> Option<PathSegment<UnresolvedDataType>> {
    let span = span_of_cst_node(&node);

    let name_token = node.path_identifier_token()?;
    let name = name_token.text();

    let generic_types = node.generic_data_types().and_then(lower_generic_data_types);

    Some(PathSegment {
        span,
        name: name.to_owned(),
        generic_types: generic_types.unwrap_or_default(),
    })
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_path(node: CSTPath) -> Option<Path<UnresolvedDataType>> {
    let span = span_of_cst_node(&node);

    let segments = node
        .path_segments()
        .filter_map(lower_path_segment)
        .collect();

    Some(Path { span, segments })
}
