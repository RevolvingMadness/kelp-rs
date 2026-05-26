use kelp_core::parsed::expression::{ParsedExpression, ParsedExpressionId};

use crate::{
    cst::CSTEntitySelectorExpression,
    entity_selector::{lower_entity_selector, try_parse_entity_selector},
    lower_context::LowerContext,
    parser::Parser,
    span::span_of_cst_node,
    syntax::SyntaxKind,
};

pub fn try_parse_entity_selector_expression(parser: &mut Parser) -> bool {
    let state = parser.save_state();

    parser.start_node(SyntaxKind::EntitySelectorExpression);

    parser.bump_identifier_kind(SyntaxKind::EntitySelectorKeyword, "entity_selector");

    parser.skip_whitespace();

    if !parser.try_bump_char(':') {
        parser.restore_state(state);

        return false;
    }

    parser.skip_whitespace();

    if !try_parse_entity_selector(parser) {
        parser.error("Expected entity selector");
    }

    parser.finish_node();

    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_entity_selector_expression(
    node: CSTEntitySelectorExpression,
    ctx: &mut LowerContext,
) -> Option<ParsedExpressionId> {
    let entity_selector = lower_entity_selector(node.entity_selector()?, ctx)?;

    let span = span_of_cst_node(&node);

    Some(ctx.allocator.allocate_expression(
        span,
        ParsedExpression::EntitySelector(Box::new(entity_selector)),
    ))
}
