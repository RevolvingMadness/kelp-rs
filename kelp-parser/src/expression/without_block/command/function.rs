use kelp_core::parsed::{
    command::ParsedCommand,
    expression::{ParsedExpression, ParsedExpressionKind},
};

use crate::{
    cst::CSTFunctionCommandExpression,
    extension_traits::AstNodeExt,
    lower_context::LowerContext,
    parser::Parser,
    resource_location::{lower_resource_location, try_parse_resource_location},
    syntax::SyntaxKind,
};

pub fn try_parse_function_command_expression(parser: &mut Parser) -> bool {
    let state = parser.save_state();

    parser.start_node(SyntaxKind::FunctionCommandExpression);
    parser.bump_str(SyntaxKind::FunctionKeyword, "function");

    if !parser.expect_inline_whitespace() || !try_parse_resource_location(parser) {
        state.restore(parser);

        return false;
    }

    parser.finish_node();

    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_function_command_expression(
    node: CSTFunctionCommandExpression,
    ctx: &mut LowerContext,
) -> Option<ParsedExpression> {
    let span = node.span();

    let resource_location = lower_resource_location(node.resource_location()?, ctx)?;

    Some(
        ParsedExpressionKind::Command(Box::new(ParsedCommand::Function(resource_location, None)))
            .with_span(span),
    )
}
