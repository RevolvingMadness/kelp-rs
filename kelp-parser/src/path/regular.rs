use kelp_core::path::regular::{Path, PathSegment};

use crate::{
    cst::{CSTPath, CSTPathSegment},
    extension_traits::AstNodeExt,
    parser::Parser,
    syntax::SyntaxKind,
};

#[must_use]
fn try_parse_path_segment(parser: &mut Parser) -> bool {
    let checkpoint = parser.mark();

    if !parser.try_bump_identifier_kind(SyntaxKind::PathIdentifier) {
        return false;
    }

    checkpoint.start_node(parser, SyntaxKind::PathSegment);

    parser.finish_node();

    true
}

#[must_use]
pub fn try_parse_path(parser: &mut Parser) -> bool {
    let checkpoint = parser.mark();

    if !try_parse_path_segment(parser) {
        return false;
    }

    checkpoint.start_node(parser, SyntaxKind::Path);

    loop {
        let state = parser.save_state();

        parser.skip_inline_whitespace();

        if !parser.try_bump_str("::", SyntaxKind::ColonColon) || !try_parse_path_segment(parser) {
            state.restore(parser);

            break;
        }
    }

    parser.finish_node();
    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
fn lower_path_segment(node: CSTPathSegment) -> Option<PathSegment> {
    let span = node.span();

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
    let span = node.span();

    let segments = node
        .path_segments()
        .filter_map(lower_path_segment)
        .collect();

    Some(Path { span, segments })
}
