use kelp_core::high::{
    command::{Command, r#return::ReturnCommand},
    expression::{Expression, ExpressionKind},
    semantic_analysis_context::SemanticAnalysisContext,
};

use crate::{
    cst::CSTReturnCommandExpression, parser::Parser, span::span_of_cst_node, syntax::SyntaxKind,
    whole_value::expect_whole_value,
};

pub fn try_parse_return_command_expression(parser: &mut Parser) -> bool {
    let state = parser.save_state();

    parser.start_node(SyntaxKind::ReturnCommandExpression);
    parser.bump_str(SyntaxKind::ReturnKeyword, "return");

    if !parser.expect_inline_whitespace() || !expect_whole_value(parser) {
        parser.restore_state(state);

        return false;
    }

    parser.finish_node();

    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_return_command_expression(
    node: CSTReturnCommandExpression,
    _ctx: &mut SemanticAnalysisContext,
) -> Option<Expression> {
    let span = span_of_cst_node(&node);

    let value_token = node.whole_value_token()?;
    let value = value_token.text().parse().ok()?;

    Some(
        ExpressionKind::Command(Box::new(Command::Return(ReturnCommand::Value(value))))
            .with_span(span),
    )
}
