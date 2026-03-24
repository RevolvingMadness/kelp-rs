use kelp_core::high::{
    semantic_analysis::SemanticAnalysisContext,
    statement::{Statement, StatementKind},
};

use crate::{
    cst::{CSTExpressionStatement, CSTStatement},
    expression::{
        is_expression_recovery, try_parse_expression_with_block,
        try_parse_expression_without_block, with_block::lower_expression_with_block,
        without_block::lower_expression_without_block,
    },
    parser::Parser,
    span::span_of_cst_node,
    statement::{
        append::{lower_append_statement, try_parse_append_statement},
        r#break::{lower_break_statement, try_parse_break_statement},
        r#continue::{lower_continue_statement, try_parse_continue_statement},
        item::{lower_item_statement, try_parse_item_statement},
        r#let::{lower_let_statement, try_parse_let_statement},
        remove::{lower_remove_statement, try_parse_remove_statement},
    },
    syntax::SyntaxKind,
};

pub mod append;
pub mod r#break;
pub mod r#continue;
pub mod item;
pub mod r#let;
pub mod remove;

#[must_use]
pub fn is_statement_recovery(char: char) -> bool {
    is_expression_recovery(char) || char == '\n' || char == '{'
}

pub fn expect_semicolon_ending(parser: &mut Parser) -> bool {
    let state = parser.save_state();

    parser.skip_whitespace();

    if parser.try_bump_char(';') {
        return true;
    }

    parser.restore_state(state);
    parser.error("Expected ';'");

    false
}

#[must_use]
pub fn try_parse_statement(parser: &mut Parser) -> bool {
    if let Some(text) = parser.peek_identifier() {
        match text {
            "let" => {
                if try_parse_let_statement(parser) {
                    return true;
                }
            }
            "break" => {
                if try_parse_break_statement(parser) {
                    return true;
                }
            }
            "continue" => {
                if try_parse_continue_statement(parser) {
                    return true;
                }
            }
            "append" => {
                if try_parse_append_statement(parser) {
                    return true;
                }
            }
            "remove" => {
                if try_parse_remove_statement(parser) {
                    return true;
                }
            }
            _ => {
                if try_parse_item_statement(parser) {
                    return true;
                }
            }
        }
    }

    parser.start_node(SyntaxKind::ExpressionStatement);

    if try_parse_expression_with_block(parser) {
        let state = parser.save_state();
        parser.skip_whitespace();

        if !parser.try_bump_char(';') {
            parser.restore_state(state);
        }
    } else if try_parse_expression_without_block(parser) {
        let state = parser.save_state();
        parser.skip_whitespace();

        if !parser.try_bump_char(';') {
            let next_char = parser.peek_char();

            parser.restore_state(state);

            if next_char != Some('}') && next_char.is_some() {
                parser.error("Expected ';'");
            }
        }
    } else {
        let chars = parser.source[parser.pos..].chars();
        let mut length = 0;

        for char in chars {
            if is_statement_recovery(char) {
                break;
            }

            length += char.len_utf8();
        }

        if length > 0 {
            parser.error_with_len("Expected statement", length);

            parser.add_token(SyntaxKind::Garbage, length);
        }
    }

    parser.finish_node();

    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_expression_statement(
    node: CSTExpressionStatement,
    ctx: &mut SemanticAnalysisContext,
) -> Option<Statement> {
    let expression = if let Some(without_block) = node.expression_without_block() {
        lower_expression_without_block(without_block, ctx)?
    } else if let Some(with_block) = node.expression_with_block() {
        lower_expression_with_block(with_block, ctx)?
    } else {
        unreachable!()
    };

    Some(StatementKind::Expression(expression).with_span(span_of_cst_node(&node)))
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_statement(node: CSTStatement, ctx: &mut SemanticAnalysisContext) -> Option<Statement> {
    match node {
        CSTStatement::ExpressionStatement(node) => lower_expression_statement(node, ctx),
        CSTStatement::LetStatement(statement) => lower_let_statement(statement, ctx),
        CSTStatement::BreakStatement(node) => lower_break_statement(node, ctx),
        CSTStatement::ContinueStatement(node) => lower_continue_statement(node, ctx),
        CSTStatement::AppendStatement(node) => lower_append_statement(node, ctx),
        CSTStatement::RemoveStatement(node) => lower_remove_statement(node, ctx),
        CSTStatement::ItemStatement(node) => lower_item_statement(node, ctx),
    }
}
