use kelp_core::parsed::statement::{ParsedStatement, ParsedStatementKind};

use crate::{
    cst::{CSTExpression, CSTLetStatement, CSTPattern},
    data_type::{lower_data_type, try_parse_data_type},
    extension_traits::{AstNodeExt, LowerableAstNode, ParsableAstNode},
    lower_context::LowerContext,
    parser::Parser,
    pattern::lower_pattern,
    statement::expect_semicolon_ending,
    syntax::SyntaxKind,
};

pub fn try_parse_let_statement(parser: &mut Parser) -> bool {
    let state = parser.save_state();

    parser.start_node(SyntaxKind::LetStatement);
    parser.bump_str(SyntaxKind::LetKeyword, "let");
    parser.skip_whitespace();

    if !CSTPattern::try_parse(parser) {
        state.restore(parser);

        return false;
    }

    parser.skip_whitespace();

    if parser.try_bump_char(':') {
        parser.skip_whitespace();

        if !try_parse_data_type(parser) {
            parser.error("Expected data type");
        }

        parser.skip_whitespace();
    }

    let parsed_equals = parser.try_bump_char('=');
    if !parsed_equals {
        parser.error("Expected '='");
    }

    parser.skip_whitespace();

    if !CSTExpression::try_parse(parser) && parsed_equals {
        parser.recover_not_whitespace("Expected expression");
    }

    expect_semicolon_ending(parser);

    parser.finish_node();

    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_let_statement(
    node: CSTLetStatement,
    ctx: &mut LowerContext,
) -> Option<ParsedStatement> {
    let span = node.span();

    let pattern = lower_pattern(node.pattern()?, ctx)?;

    let data_type = node.data_type().and_then(lower_data_type);

    let value = node.expression()?.lower(ctx)?;

    Some(ParsedStatementKind::Let(data_type, pattern, value).with_span(span))
}
