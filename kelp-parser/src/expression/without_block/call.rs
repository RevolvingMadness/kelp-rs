use kelp_core::parsed::expression::{ParsedExpression, ParsedExpressionId};

use crate::{
    cst::{CSTCallArguments, CSTCallExpression},
    expression::{lower_expression, try_parse_expression},
    lower_context::LowerContext,
    parser::Parser,
    span::span_of_cst_node,
    syntax::SyntaxKind,
};

pub fn try_parse_call_arguments(parser: &mut Parser) {
    parser.start_node(SyntaxKind::CallArguments);

    loop {
        if !try_parse_expression(parser) {
            break;
        }

        let comma_state = parser.save_state();
        parser.skip_whitespace();

        if !parser.try_bump_char(',') {
            parser.restore_state(comma_state);
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
) -> Vec<ParsedExpressionId> {
    node.expressions()
        .filter_map(|expression| lower_expression(expression, ctx))
        .collect()
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_call_expression(
    node: CSTCallExpression,
    ctx: &mut LowerContext,
) -> Option<ParsedExpressionId> {
    let span = span_of_cst_node(&node);

    let callee = lower_expression(node.callee()?, ctx)?;

    let arguments = node
        .call_arguments()
        .map(|arguments| lower_call_arguments(arguments, ctx));

    Some(ctx.allocator.allocate_expression(
        span,
        ParsedExpression::Call {
            callee,
            arguments: arguments.unwrap_or_default(),
        },
    ))
}
