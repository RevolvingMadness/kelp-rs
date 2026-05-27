use kelp_core::parsed::{
    command::ParsedCommand,
    expression::{ParsedExpression, ParsedExpressionKind},
};

use crate::{
    cst::{CSTFunctionCommandExpression, CSTResourceLocation},
    extension_traits::{AstNodeExt, LowerableAstNode, ParsableAstNode},
    lower_context::LowerContext,
    parser::Parser,
    syntax::SyntaxKind,
};

impl ParsableAstNode for CSTFunctionCommandExpression {
    fn try_parse(parser: &mut Parser) -> bool {
        let state = parser.save_state();

        parser.start_node(SyntaxKind::FunctionCommandExpression);
        parser.bump_str(SyntaxKind::FunctionKeyword, "function");

        if !parser.expect_inline_whitespace() || !CSTResourceLocation::try_parse(parser) {
            state.restore(parser);

            return false;
        }

        parser.finish_node();

        true
    }
}

impl LowerableAstNode for CSTFunctionCommandExpression {
    type Lowered = ParsedExpression;

    fn lower(self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let resource_location = self.resource_location()?.lower(ctx)?;

        Some(
            ParsedExpressionKind::Command(Box::new(ParsedCommand::Function(
                resource_location,
                None,
            )))
            .with_span(self.span()),
        )
    }
}
