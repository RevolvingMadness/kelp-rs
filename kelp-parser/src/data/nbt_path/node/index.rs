use crate::{expression::try_parse_expression, parser::Parser, syntax::SyntaxKind};

pub fn try_parse_index_nbt_path_node(parser: &mut Parser) -> bool {
    if parser.peek_char() != Some('[') {
        return false;
    }

    parser.start_node(SyntaxKind::IndexNBTPathNode);
    parser.bump_char();
    parser.skip_whitespace();

    if parser.peek_char() != Some(']') {
        if !try_parse_expression(parser) {
            parser.error("Expected index expression");
        }

        parser.skip_whitespace();
    }

    parser.expect_char(']', "Expected ']'");
    parser.finish_node();
    true
}
