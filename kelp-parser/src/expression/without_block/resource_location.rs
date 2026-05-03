use kelp_core::high::{
    expression::{Expression, ExpressionKind},
    semantic_analysis::SemanticAnalysisContext,
};

use crate::{
    cst::CSTResourceLocationExpression,
    parser::Parser,
    resource_location::{lower_resource_location, try_parse_resource_location},
    span::span_of_cst_node,
    syntax::SyntaxKind,
};

pub fn try_parse_resource_location_expression(parser: &mut Parser) -> bool {
    let state = parser.save_state();

    parser.start_node(SyntaxKind::ResourceLocationExpression);

    parser.bump_identifier_kind(SyntaxKind::ResourceLocationKeyword, "resource_location");

    parser.skip_whitespace();

    if !parser.try_bump_char(':') {
        parser.restore_state(state);

        return false;
    }

    parser.skip_whitespace();

    if !try_parse_resource_location(parser) {
        parser.error("Expected resource location");
    }

    parser.finish_node();

    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_resource_location_expression(
    node: CSTResourceLocationExpression,
    ctx: &mut SemanticAnalysisContext,
) -> Option<Expression> {
    let resource_location = lower_resource_location(node.resource_location()?, ctx)?;

    let span = span_of_cst_node(&node);

    Some(ExpressionKind::ResourceLocation(Box::new(resource_location)).with_span(span))
}
