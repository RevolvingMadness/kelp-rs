use kelp_core::high::{
    command::Command,
    expression::{Expression, ExpressionKind},
    semantic_analysis::SemanticAnalysisContext,
};

use crate::{
    cst::CSTFunctionCommandExpression,
    parser::Parser,
    resource_location::{lower_resource_location, try_parse_resource_location},
    span::span_of_cst_node,
    syntax::SyntaxKind,
};

pub fn try_parse_function_command_expression(parser: &mut Parser) -> bool {
    let state = parser.save_state();

    parser.start_node(SyntaxKind::FunctionCommandExpression);
    parser.bump_str(SyntaxKind::FunctionKeyword, "function");

    if !parser.expect_inline_whitespace() || !try_parse_resource_location(parser) {
        parser.restore_state(state);

        return false;
    }

    parser.finish_node();

    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_function_command_expression(
    node: CSTFunctionCommandExpression,
    ctx: &mut SemanticAnalysisContext,
) -> Option<Expression> {
    let span = span_of_cst_node(&node);

    let resource_location = lower_resource_location(node.resource_location()?, ctx)?;

    Some(
        ExpressionKind::Command(Box::new(Command::Function(resource_location, None)))
            .with_span(span),
    )
}
