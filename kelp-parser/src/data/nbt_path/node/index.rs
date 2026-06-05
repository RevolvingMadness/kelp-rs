use crate::{
    cst::{CSTExpression, CSTIndexNBTPathNode},
    extension_traits::ParsableAstNode,
    parser::Parser,
    syntax::SyntaxKind,
};

impl ParsableAstNode for CSTIndexNBTPathNode {
    fn try_parse(parser: &mut Parser) -> bool {
        if parser.peek_char() != Some('[') {
            return false;
        }

        parser.start_node(SyntaxKind::IndexNBTPathNode);
        parser.bump_char();
        parser.skip_whitespace();

        if parser.peek_char() != Some(']') {
            CSTExpression::expect(parser, "Expected expression");

            parser.skip_whitespace();
        }

        parser.expect_char(']');
        parser.finish_node();

        true
    }
}
