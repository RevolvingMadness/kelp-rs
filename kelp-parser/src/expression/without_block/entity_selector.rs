use kelp_core::parsed::expression::{ParsedExpression, ParsedExpressionKind};

use crate::{
    cst::CSTEntitySelectorExpression,
    entity_selector::{lower_entity_selector, try_parse_entity_selector},
    extension_traits::{AstNodeExt, LowerableAstNode, ParsableAstNode},
    lower_context::LowerContext,
    parser::Parser,
    syntax::SyntaxKind,
};

impl ParsableAstNode for CSTEntitySelectorExpression {
    fn try_parse(parser: &mut Parser) -> bool {
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
}

impl LowerableAstNode for CSTEntitySelectorExpression {
    type Lowered = ParsedExpression;

    fn lower(self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let entity_selector = lower_entity_selector(self.entity_selector()?, ctx)?;

        Some(ParsedExpressionKind::EntitySelector(Box::new(entity_selector)).with_span(self.span()))
    }
}
