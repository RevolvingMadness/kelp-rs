use kelp_core::high::{expression::Expression, semantic_analysis_context::SemanticAnalysisContext};

use crate::{
    cst::CSTLocalCoordinate,
    expression::{lower_expression, try_parse_expression},
    parser::Parser,
    syntax::SyntaxKind,
};

pub fn parse_local_coordinate(parser: &mut Parser) {
    parser.start_node(SyntaxKind::LocalCoordinate);

    if !parser.expect_char('^', "Expected '^'") {
        parser.bump_char();
    }

    try_parse_expression(parser);

    parser.finish_node();
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_local_coordinate(
    node: CSTLocalCoordinate,
    ctx: &mut SemanticAnalysisContext,
) -> Option<Option<Expression>> {
    let result = node
        .expression()
        .map(|expression| lower_expression(expression, ctx));

    match result {
        Some(Some(expression)) => Some(Some(expression)),
        Some(None) => None,
        None => Some(None),
    }
}
