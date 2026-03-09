use kelp_core::{
    expression::{Expression, ExpressionKind},
    semantic_analysis_context::SemanticAnalysisContext,
};

use crate::{
    cst::CSTListExpression,
    lower::expression::{lower_expression, try_parse_expression},
    parser::Parser,
    span::span_of_cst_node,
    syntax::SyntaxKind,
};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn try_parse_list_expression(parser: &mut Parser) -> bool {
    if parser.peek_char() != Some('[') {
        return false;
    }

    parser.start_node(SyntaxKind::ListExpression);
    parser.bump_char();

    let mut is_first = true;

    loop {
        parser.skip_whitespace();

        if parser.is_eof() || parser.peek_char() == Some(']') {
            break;
        }

        if !is_first {
            if parser.try_bump_char(',') {
                parser.skip_whitespace();
            } else {
                parser.error("Expected ',' between array elements");
            }
        }

        if parser.peek_char() == Some(']') {
            break;
        }

        if !try_parse_expression(parser) {
            parser.error("Expected expression");
            parser.bump_until_char(&[',', ']']);
        }

        is_first = false;
    }

    parser.expect_char(']', "Expected ']' to terminate array");
    parser.finish_node();
    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_list_expression(
    node: CSTListExpression,
    ctx: &mut SemanticAnalysisContext,
) -> Option<Expression> {
    let span = span_of_cst_node(&node);

    let expressions = node
        .expressions()
        .filter_map(|expression| lower_expression(expression, ctx))
        .collect();

    Some(ExpressionKind::List(expressions).with_span(span))
}
