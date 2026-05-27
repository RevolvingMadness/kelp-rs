use kelp_core::parsed::{
    command::ParsedCommand,
    expression::{ParsedExpression, ParsedExpressionKind},
};

use crate::{
    cst::{CSTEntitySelector, CSTExpression, CSTTellrawCommandExpression},
    extension_traits::{AstNodeExt, LowerableAstNode, ParsableAstNode},
    lower_context::LowerContext,
    parser::Parser,
    syntax::SyntaxKind,
};

impl ParsableAstNode for CSTTellrawCommandExpression {
    fn try_parse(parser: &mut Parser) -> bool {
        let state = parser.save_state();

        parser.start_node(SyntaxKind::TellrawCommandExpression);
        parser.bump_str(SyntaxKind::TellrawKeyword, "tellraw");

        if !parser.expect_inline_whitespace() || !CSTEntitySelector::try_parse(parser) {
            state.restore(parser);

            return false;
        }

        let parsed_whitespace = parser.expect_inline_whitespace();

        if !CSTExpression::try_parse(parser) {
            if parsed_whitespace {
                parser.recover_not_whitespace("Expected expression");
            } else {
                parser.bump_until_not_whitespace();
            }
        }

        parser.finish_node();

        true
    }
}

impl LowerableAstNode for CSTTellrawCommandExpression {
    type Lowered = ParsedExpression;

    fn lower(&self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let selector = self.entity_selector()?.lower(ctx)?;
        let value = self.expression()?.lower(ctx)?;

        Some(
            ParsedExpressionKind::Command(Box::new(ParsedCommand::Tellraw(selector, value)))
                .with_span(self.span()),
        )
    }
}
