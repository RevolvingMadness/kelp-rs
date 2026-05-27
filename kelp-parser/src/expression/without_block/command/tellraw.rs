use kelp_core::parsed::{
    command::ParsedCommand,
    expression::{ParsedExpression, ParsedExpressionKind},
};

use crate::{
    cst::{CSTExpression, CSTTellrawCommandExpression},
    entity_selector::{lower_entity_selector, try_parse_entity_selector},
    extension_traits::{AstNodeExt, LowerableAstNode, ParsableAstNode},
    lower_context::LowerContext,
    parser::Parser,
    syntax::SyntaxKind,
};

pub fn try_parse_tellraw_command_expression(parser: &mut Parser) -> bool {
    let state = parser.save_state();

    parser.start_node(SyntaxKind::TellrawCommandExpression);
    parser.bump_str(SyntaxKind::TellrawKeyword, "tellraw");

    if !parser.expect_inline_whitespace() || !try_parse_entity_selector(parser) {
        state.restore(parser);

        return false;
    }

    let parsed_whitespace = parser.expect_inline_whitespace();

    if !CSTExpression::try_parse(parser) {
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
    ctx: &mut LowerContext,
) -> Option<ParsedExpression> {
    let span = node.span();

    let selector = lower_entity_selector(node.entity_selector()?, ctx)?;
    let value = node.expression()?.lower(ctx)?;

    Some(
        ParsedExpressionKind::Command(Box::new(ParsedCommand::Tellraw(selector, value)))
            .with_span(span),
    )
}
