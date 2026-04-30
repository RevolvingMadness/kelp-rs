use kelp_core::high::{
    expression::block::{BlockExpression, BlockExpressionInfo},
    semantic_analysis::SemanticAnalysisContext,
};

use crate::{
    cst::{CSTBlockExpression, CSTStatement},
    expression::{
        with_block::lower_expression_with_block, without_block::lower_expression_without_block,
    },
    parser::Parser,
    span::span_of_cst_node,
    statement::{lower_statement, try_parse_statement},
    syntax::SyntaxKind,
};

pub fn try_parse_block_expression(parser: &mut Parser) -> bool {
    let checkpoint = parser.checkpoint();

    if !parser.try_bump_char('{') {
        return false;
    }

    parser.start_node_at(checkpoint, SyntaxKind::BlockExpression);

    loop {
        parser.skip_whitespace();

        if parser.is_eof() || parser.peek_char() == Some('}') {
            break;
        }

        if !try_parse_statement(parser) {
            parser.recover_not_whitespace("Expected statement");
        }
    }

    parser.expect_char('}', "Expected '}'");

    parser.finish_node();

    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_block_expression(
    node: CSTBlockExpression,
    ctx: &mut SemanticAnalysisContext,
) -> Option<BlockExpression> {
    let span = span_of_cst_node(&node);

    let cst_statements: Vec<_> = node.statements().collect();
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
                tail_expression = lower_expression_with_block(with_block, ctx);

                continue;
            } else if let Some(without_block) = cst_expression_statement.expression_without_block()
            {
                tail_expression = lower_expression_without_block(without_block, ctx);

                continue;
            }

            continue;
        }

        if let Some(stmt) = lower_statement(cst_statement, ctx) {
            statements.push(stmt);
        }
    }

    Some(
        BlockExpressionInfo {
            statements,
            tail_expression: tail_expression.map(Box::new),
        }
        .with_span(span),
    )
}
