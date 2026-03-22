use kelp_core::high::use_tree::UseTree;

use crate::{
    cst::CSTUseTree,
    parser::Parser,
    path::regular::{lower_path, try_parse_path},
    span::text_range_to_span,
    syntax::SyntaxKind,
};

#[must_use]
pub fn try_parse_use_tree(parser: &mut Parser) -> bool {
    let checkpoint = parser.checkpoint();

    if parser.peek_char() == Some('{') {
        parser.start_node_at(checkpoint, SyntaxKind::GroupUseTree);
        parse_use_tree_group(parser);
        parser.finish_node();
        return true;
    }

    if !try_parse_path(parser) {
        return false;
    }

    parser.skip_inline_whitespace();

    let state = parser.save_state();
    if parser.try_bump_str("::", SyntaxKind::ColonColon) {
        parser.skip_inline_whitespace();

        if parser.peek_char() == Some('*') {
            parser.start_node_at(checkpoint, SyntaxKind::WildcardUseTree);
            parser.bump_char_kind(SyntaxKind::Star);
        } else if parser.peek_char() == Some('{') {
            parser.start_node_at(checkpoint, SyntaxKind::GroupUseTree);
            parse_use_tree_group(parser);
        } else {
            parser.restore_state(state);
            parser.start_node_at(checkpoint, SyntaxKind::PathUseTree);
        }
    } else if parser.peek_identifier() == Some("as") {
        parser.start_node_at(checkpoint, SyntaxKind::AsUseTree);
        parser.bump_identifier_kind(SyntaxKind::AsKeyword, "as");
        parser.expect_inline_whitespace();

        if let Some(ident) = parser.peek_identifier() {
            parser.bump_identifier_kind(SyntaxKind::Identifier, ident);
        } else {
            parser.error("Expected alias identifier after 'as'");
        }
    } else {
        parser.start_node_at(checkpoint, SyntaxKind::PathUseTree);
    }

    parser.finish_node();

    true
}

fn parse_use_tree_group(parser: &mut Parser) {
    parser.bump_char();

    loop {
        parser.skip_whitespace();

        if parser.peek_char() == Some('}') {
            break;
        }

        if !try_parse_use_tree(parser) {
            parser.error("Expected use tree");
            parser.bump_until_char(&[',', '}']);
        }

        parser.skip_whitespace();

        if !parser.try_bump_char(',') {
            break;
        }
    }

    parser.expect_char('}', "Expected '}'");
}

#[must_use]
pub fn lower_use_tree(node: CSTUseTree) -> Option<UseTree> {
    match node {
        CSTUseTree::PathUseTree(node) => {
            let path = lower_path(node.path()?)?;

            Some(UseTree::Path(path))
        }
        CSTUseTree::WildcardUseTree(node) => {
            let path = lower_path(node.path()?)?;

            Some(UseTree::Wildcard(path))
        }
        CSTUseTree::AsUseTree(node) => {
            let path = lower_path(node.path()?)?;

            let alias_token = node.identifier_token()?;
            let alias_span = text_range_to_span(alias_token.text_range());
            let alias = alias_token.text();

            Some(UseTree::As(path, alias_span, alias.to_owned()))
        }
        CSTUseTree::GroupUseTree(node) => {
            let prefix_path = node.path().and_then(lower_path);

            let nested_trees = node
                .use_trees()
                .filter_map(lower_use_tree)
                .collect::<Vec<_>>();

            Some(UseTree::Group(prefix_path, nested_trees))
        }
    }
}
