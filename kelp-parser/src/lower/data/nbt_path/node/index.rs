use crate::{
    cst_node, lower::expression::CSTExpression, parser::Parser, semantic_token::SemanticToken,
    syntax::SyntaxKind,
};

cst_node!(CSTNBTPathIndexNode, SyntaxKind::NBTPathIndex);

impl<'a> CSTNBTPathIndexNode<'a> {
    pub fn try_parse(parser: &mut Parser) -> bool {
        if parser.peek_char() != Some('[') {
            return false;
        }

        parser.start_node(SyntaxKind::NBTPathIndex);
        parser.bump_char();
        parser.skip_whitespace();

        if parser.peek_char() != Some(']') {
            if !CSTExpression::try_parse(parser) {
                parser.error("Expected index expression");
            }

            parser.skip_whitespace();
        }

        parser.expect_char(']', "Expected ']'");
        parser.finish_node();
        true
    }

    pub fn index(&self) -> Option<CSTExpression<'a>> {
        self.0.children().find_map(CSTExpression::cast)
    }

    pub fn collect_semantic_tokens(&self, tokens: &mut Vec<SemanticToken>) {
        if let Some(index_expr) = self.index() {
            index_expr.collect_semantic_tokens(tokens);
        }
    }
}
