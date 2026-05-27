use kelp_core::parsed::expression::{ParsedExpression, ParsedExpressionKind};

use crate::{
    cst::{CSTEntitySelector, CSTEntitySelectorExpression},
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

        CSTEntitySelector::expect(parser, "Expected entity selector");

        parser.finish_node();

        true
    }
}

impl LowerableAstNode for CSTEntitySelectorExpression {
    type Lowered = ParsedExpression;

    fn lower(&self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let entity_selector = self.entity_selector()?.lower(ctx)?;

        Some(ParsedExpressionKind::EntitySelector(Box::new(entity_selector)).with_span(self.span()))
    }
}
