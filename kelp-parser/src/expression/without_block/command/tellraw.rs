use kelp_core::high::{
    command::Command,
    expression::{Expression, ExpressionKind},
    semantic_analysis::SemanticAnalysisContext,
};

use crate::{
    cst::CSTTellrawCommandExpression,
    entity_selector::{lower_entity_selector, try_parse_entity_selector},
    expression::{lower_expression, try_parse_expression},
    parser::Parser,
    span::span_of_cst_node,
    syntax::SyntaxKind,
};

pub fn try_parse_tellraw_command_expression(parser: &mut Parser) -> bool {
    let state = parser.save_state();

    parser.start_node(SyntaxKind::TellrawCommandExpression);
    parser.bump_str(SyntaxKind::TellrawKeyword, "tellraw");

    if !parser.expect_inline_whitespace() || !try_parse_entity_selector(parser) {
        parser.restore_state(state);

        return false;
    }

    let parsed_whitespace = parser.expect_inline_whitespace();

    if !try_parse_expression(parser) {
        if parsed_whitespace {
            parser.recover_not_whitespace("Expected expression");
        } else {
            parser.bump_until_not_whitespace();
        }
    }

    parser.finish_node();

    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_tellraw_command_expression(
    node: CSTTellrawCommandExpression,
    ctx: &mut SemanticAnalysisContext,
) -> Option<Expression> {
    let span = span_of_cst_node(&node);

    let selector = lower_entity_selector(node.entity_selector()?, ctx)?;
    let value = lower_expression(node.expression()?, ctx)?;

    Some(ExpressionKind::Command(Box::new(Command::Tellraw(selector, value))).with_span(span))
}
