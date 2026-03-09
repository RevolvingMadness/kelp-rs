use kelp_core::{
    semantic_analysis_context::SemanticAnalysisContext,
    statement::{Statement, StatementKind},
};

use crate::{
    cst::CSTLetStatement,
    lower::{
        data_type::{lower_data_type, try_parse_data_type},
        expression::{lower_expression, try_parse_expression},
        pattern::{lower_pattern, try_parse_pattern},
    },
    parser::Parser,
    span::span_of_cst_node,
    syntax::SyntaxKind,
};

pub fn try_parse_let_statement(parser: &mut Parser) -> bool {
    let state = parser.save_state();

    parser.start_node(SyntaxKind::LetStatement);
    parser.bump_str(SyntaxKind::LetKeyword, "let");
    parser.skip_whitespace();

    if !try_parse_pattern(parser) {
        parser.restore_state(state);

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

    if !try_parse_expression(parser) && parsed_equals {
        parser.recover_newline("Expected expression");
    }

    parser.finish_node();

    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_let_statement(
    node: CSTLetStatement,
    ctx: &mut SemanticAnalysisContext,
) -> Option<Statement> {
    let span = span_of_cst_node(&node);

    let pattern = lower_pattern(node.pattern()?)?;

    let data_type = node.data_type().and_then(lower_data_type);

    let value = lower_expression(node.expression()?, ctx)?;

    Some(StatementKind::Let(data_type, pattern, value).with_span(span))
}
