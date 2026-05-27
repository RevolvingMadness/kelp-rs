use kelp_core::parsed::expression::{ParsedExpression, ParsedExpressionKind};

use crate::{
    cst::{CSTCallArguments, CSTCallExpression, CSTExpression},
    extension_traits::{AstNodeExt, LowerableAstNode, ParsableAstNode},
    lower_context::LowerContext,
    parser::Parser,
    syntax::SyntaxKind,
};

pub fn try_parse_call_arguments(parser: &mut Parser) {
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
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_call_arguments(
    node: CSTCallArguments,
    ctx: &mut LowerContext,
) -> Vec<ParsedExpression> {
    node.expressions()
        .filter_map(|expression| expression.lower(ctx))
        .collect()
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_call_expression(
    node: CSTCallExpression,
    ctx: &mut LowerContext,
) -> Option<ParsedExpression> {
    let span = node.span();

    let callee = node.callee()?.lower(ctx)?;

    let arguments = node
        .call_arguments()
        .map(|arguments| lower_call_arguments(arguments, ctx));

    Some(
        ParsedExpressionKind::Call {
            callee: Box::new(callee),
            arguments: arguments.unwrap_or_default(),
        }
        .with_span(span),
    )
}
