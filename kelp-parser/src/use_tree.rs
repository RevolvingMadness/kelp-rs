use kelp_core::{parsed::use_tree::UseTree, trait_ext::CollectOptionAllIterExt};

use crate::{
    cst::{CSTPath, CSTUseTree},
    extension_traits::{LowerableAstNode, ParsableAstNode, SyntaxTokenExt},
    lower_context::LowerContext,
    parser::Parser,
    syntax::SyntaxKind,
};

impl ParsableAstNode for CSTUseTree {
    fn try_parse(parser: &mut Parser) -> bool {
        let marker = parser.mark();

        if parser.peek_char() == Some('{') {
            marker.start_node(parser, SyntaxKind::GroupUseTree);
            parse_use_tree_group(parser);
            parser.finish_node();
            return true;
        }

        if !CSTPath::try_parse(parser) {
            return false;
        }

        parser.skip_inline_whitespace();

        let state = parser.save_state();
        if parser.try_bump_str("::", SyntaxKind::ColonColon) {
            parser.skip_inline_whitespace();

            if parser.try_bump_char('*') {
                marker.start_node(parser, SyntaxKind::WildcardUseTree);
            } else if parser.peek_char() == Some('{') {
                marker.start_node(parser, SyntaxKind::GroupUseTree);
                parse_use_tree_group(parser);
            } else {
                state.restore(parser);
                marker.start_node(parser, SyntaxKind::PathUseTree);
            }
        } else if parser.peek_identifier() == Some("as") {
            marker.start_node(parser, SyntaxKind::AsUseTree);
            parser.bump_identifier_kind(SyntaxKind::AsKeyword, "as");
            parser.expect_inline_whitespace();

            if let Some(ident) = parser.peek_identifier() {
                parser.bump_identifier_kind(SyntaxKind::Identifier, ident);
            } else {
                parser.error("Expected alias identifier after 'as'");
            }
        } else {
            marker.start_node(parser, SyntaxKind::PathUseTree);
        }

        parser.finish_node();

        true
    }
}

fn parse_use_tree_group(parser: &mut Parser) {
    parser.bump_char();

    loop {
        parser.skip_whitespace();

        if parser.peek_char() == Some('}') {
            break;
        }

        if !CSTUseTree::expect(parser, "Expected use tree") {
            parser.bump_until_char(&[',', '}']);
        }

        parser.skip_whitespace();

        if !parser.try_bump_char(',') {
            break;
        }
    }

    parser.expect_char('}', "Expected '}'");
}

impl LowerableAstNode for CSTUseTree {
    type Lowered = UseTree;

    fn lower(&self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        match self {
            Self::PathUseTree(node) => {
                let path = node.path()?.lower(ctx)?;

                Some(UseTree::Path(path))
            }
            Self::WildcardUseTree(node) => {
                let path = node.path()?.lower(ctx)?;

                Some(UseTree::Wildcard(path))
            }
            Self::AsUseTree(node) => {
                let path = node.path()?.lower(ctx)?;

                let alias_token = node.identifier_token()?;
                let alias_span = alias_token.span();
                let alias = alias_token.text();

                Some(UseTree::As(path, alias_span, alias.to_owned()))
            }
            Self::GroupUseTree(node) => {
                let prefix_path = node.path().and_then(|path| path.lower(ctx));

                let nested_trees = node
                    .use_trees()
                    .map(|tree| tree.lower(ctx))
                    .collect_option_all()?;

                Some(UseTree::Group(prefix_path, nested_trees))
            }
        }
    }
}
