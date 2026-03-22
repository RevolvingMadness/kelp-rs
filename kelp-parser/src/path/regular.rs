use kelp_core::path::regular::{Path, PathSegment};

use crate::{
    cst::{CSTPath, CSTPathSegment},
    parser::Parser,
    span::span_of_cst_node,
    syntax::SyntaxKind,
};

#[must_use]
fn try_parse_path_segment(parser: &mut Parser) -> bool {
    let checkpoint = parser.checkpoint();

    if !parser.try_bump_identifier_kind(SyntaxKind::PathIdentifier) {
        return false;
    }

    parser.start_node_at(checkpoint, SyntaxKind::PathSegment);

    parser.finish_node();

    true
}

#[must_use]
pub fn try_parse_path(parser: &mut Parser) -> bool {
    let checkpoint = parser.checkpoint();

    if !try_parse_path_segment(parser) {
        return false;
    }

    parser.start_node_at(checkpoint, SyntaxKind::Path);

    loop {
        let state = parser.save_state();

        parser.skip_inline_whitespace();

        if !parser.try_bump_str("::", SyntaxKind::ColonColon) || !try_parse_path_segment(parser) {
            parser.restore_state(state);

            break;
        }
    }

    parser.finish_node();
    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
fn lower_path_segment(node: CSTPathSegment) -> Option<PathSegment> {
    let span = span_of_cst_node(&node);

    let name_token = node.path_identifier_token()?;
    let name = name_token.text();

    Some(PathSegment {
        span,
        name: name.to_owned(),
    })
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_path(node: CSTPath) -> Option<Path> {
    let span = span_of_cst_node(&node);

    let segments = node
        .path_segments()
        .filter_map(lower_path_segment)
        .collect();

    Some(Path { span, segments })
}
