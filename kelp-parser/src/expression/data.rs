use kelp_core::{
    high::expression::{Expression, ExpressionKind},
    semantic_analysis_context::SemanticAnalysisContext,
};

use crate::{
    cst::CSTDataExpression,
    data::{
        nbt_path::{lower_nbt_path, try_parse_nbt_path},
        target::{lower_data_target, try_parse_data_target},
    },
    parser::Parser,
    span::span_of_cst_node,
    syntax::SyntaxKind,
};

pub fn try_parse_data_expression(parser: &mut Parser) -> bool {
    let checkpoint = parser.checkpoint();

    if !try_parse_data_target(parser) {
        return false;
    }

    parser.start_node_at(checkpoint, SyntaxKind::DataExpression);

    parser.expect_inline_whitespace();

    try_parse_nbt_path(parser);

    parser.finish_node();

    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_data_expression(
    node: CSTDataExpression,
    ctx: &mut SemanticAnalysisContext,
) -> Option<Expression> {
    let span = span_of_cst_node(&node);

    let target = lower_data_target(node.data_target()?)?;
    let path = lower_nbt_path(node.n_b_t_path()?, ctx)?;

    Some(ExpressionKind::Data(Box::new((target, path))).with_span(span))
}
