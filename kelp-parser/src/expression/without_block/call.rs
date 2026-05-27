use kelp_core::{
    parsed::expression::{ParsedExpression, ParsedExpressionKind},
    trait_ext::CollectOptionAllIterExt,
};

use crate::{
    cst::{CSTCallArguments, CSTCallExpression, CSTExpression},
    extension_traits::{AstNodeExt, LowerableAstNode, ParsableAstNode},
    lower_context::LowerContext,
    parser::Parser,
    syntax::SyntaxKind,
};

impl ParsableAstNode for CSTCallArguments {
    fn try_parse(parser: &mut Parser) -> bool {
        parser.start_node(SyntaxKind::CallArguments);

        loop {
            if !CSTExpression::try_parse(parser) {
                break;
            }

            let comma_state = parser.save_state();
            parser.skip_whitespace();

            if !parser.try_bump_char(',') {
                comma_state.restore(parser);
                break;
            }

            parser.skip_whitespace();

            if parser.peek_char() == Some(')') {
                break;
            }
        }

        parser.finish_node();

        true
    }
}

impl LowerableAstNode for CSTCallArguments {
    type Lowered = Vec<ParsedExpression>;

    fn lower(self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        self.expressions()
            .map(|expression| expression.lower(ctx))
            .collect_option_all()
    }
}

impl LowerableAstNode for CSTCallExpression {
    type Lowered = ParsedExpression;

    fn lower(self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let callee = self.callee()?.lower(ctx)?;

        let arguments = self.call_arguments().map(|arguments| arguments.lower(ctx));

        let arguments = match arguments {
            Some(Some(arguments)) => Some(arguments),
            Some(None) => return None,
            None => None,
        };

        Some(
            ParsedExpressionKind::Call {
                callee: Box::new(callee),
                arguments: arguments.unwrap_or_default(),
            }
            .with_span(self.span()),
        )
    }
}
