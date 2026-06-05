use kelp_core::parsed::expression::block::{ParsedBlockExpression, ParsedBlockExpressionInfo};

use crate::{
    cst::{CSTBlockExpression, CSTStatement},
    extension_traits::{AstNodeExt, LowerableAstNode, ParsableAstNode},
    lower_context::LowerContext,
    parser::Parser,
    syntax::SyntaxKind,
};

impl ParsableAstNode for CSTBlockExpression {
    fn try_parse(parser: &mut Parser) -> bool {
        let marker = parser.mark();

        if !parser.try_bump_char('{') {
            return false;
        }

        marker.start_node(parser, SyntaxKind::BlockExpression);

        loop {
            parser.skip_whitespace();

            if parser.is_eof() || parser.peek_char() == Some('}') {
                break;
            }

            CSTStatement::expect(parser, "Expected statement");
        }

        parser.expect_char('}');

        parser.finish_node();

        true
    }
}

impl LowerableAstNode for CSTBlockExpression {
    type Lowered = ParsedBlockExpression;

    fn lower(&self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let cst_statements: Vec<_> = self.statements().collect();
        let mut statements = Vec::new();
        let mut tail_expression = None;

        let len = cst_statements.len();

        for (i, cst_statement) in cst_statements.into_iter().enumerate() {
            let is_last = i == len - 1;

            if is_last
                && let CSTStatement::ExpressionStatement(cst_expression_statement) = &cst_statement
                && cst_expression_statement.semicolon_token().is_none()
            {
                if let Some(with_block) = cst_expression_statement.expression_with_block() {
                    tail_expression = with_block.lower(ctx);

                    continue;
                } else if let Some(without_block) =
                    cst_expression_statement.expression_without_block()
                {
                    tail_expression = without_block.lower(ctx);

                    continue;
                }

                continue;
            }

            if let Some(stmt) = cst_statement.lower(ctx) {
                statements.push(stmt);
            }
        }

        Some(
            ParsedBlockExpressionInfo {
                statements,
                tail_expression: tail_expression.map(Box::new),
            }
            .with_span(self.span()),
        )
    }
}
