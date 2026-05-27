use kelp_core::parsed::expression::{ParsedExpression, ParsedExpressionKind};

use crate::{
    cst::CSTEntitySelectorExpression,
    entity_selector::{lower_entity_selector, try_parse_entity_selector},
    extension_traits::AstNodeExt,
    lower_context::LowerContext,
    parser::Parser,
    syntax::SyntaxKind,
};

pub fn try_parse_entity_selector_expression(parser: &mut Parser) -> bool {
    let state = parser.save_state();

    parser.start_node(SyntaxKind::EntitySelectorExpression);

    parser.bump_identifier_kind(SyntaxKind::EntitySelectorKeyword, "entity_selector");

    parser.skip_whitespace();

    if !parser.try_bump_char(':') {
        state.restore(parser);

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
) -> Option<ParsedExpression> {
    let entity_selector = lower_entity_selector(node.entity_selector()?, ctx)?;

    let span = node.span();

    Some(ParsedExpressionKind::EntitySelector(Box::new(entity_selector)).with_span(span))
}
