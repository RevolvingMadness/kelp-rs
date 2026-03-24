use kelp_core::high::{
    expression::{Expression, ExpressionKind},
    semantic_analysis::SemanticAnalysisContext,
};

use crate::{
    cst::CSTIfExpression,
    expression::{
        lower_expression, try_parse_expression,
        with_block::block::{lower_block_expression, try_parse_block_expression},
    },
    parser::Parser,
    span::span_of_cst_node,
    syntax::SyntaxKind,
};

pub fn try_parse_if_expression(parser: &mut Parser) -> bool {
    let state = parser.save_state();

    parser.start_node(SyntaxKind::IfExpression);
    parser.bump_str(SyntaxKind::IfKeyword, "if");
    parser.skip_inline_whitespace();

    if !try_parse_expression(parser) {
        parser.restore_state(state);

        return false;
    }

    parser.skip_inline_whitespace();

    if !try_parse_block_expression(parser) {
        parser.recover_not_whitespace("Expected block statement");
    }

    parser.skip_inline_whitespace();

    if parser.peek_identifier() == Some("else") {
        parser.bump_str(SyntaxKind::ElseKeyword, "else");

        parser.skip_inline_whitespace();

        if parser.peek_char() == Some('{') {
            if !try_parse_block_expression(parser) {
                parser.recover_not_whitespace("Expected block statement");
            }
        } else if parser.peek_identifier() == Some("if") {
            if !try_parse_if_expression(parser) {
                parser.recover_not_whitespace("Expected if statement");
            }
        } else {
            parser.recover_not_whitespace("Expected block or if statement");
        }
    }

    parser.finish_node();

    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_if_expression(
    node: CSTIfExpression,
    ctx: &mut SemanticAnalysisContext,
) -> Option<Expression> {
    let span = span_of_cst_node(&node);

    let condition = lower_expression(node.condition()?, ctx)?;
    let body = lower_block_expression(node.body()?, ctx)?;
    let else_body = node
        .else_body_block()
        .and_then(|expression| {
            let span = span_of_cst_node(&expression);

            let expression = lower_block_expression(expression, ctx)?;

            Some(ExpressionKind::Block(expression).with_span(span))
        })
        .or_else(|| lower_if_expression(node.else_body_if()?, ctx))
        .map(Box::new);

    Some(ExpressionKind::If(Box::new(condition), Box::new(body), else_body).with_span(span))
}
