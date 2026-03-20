use kelp_core::high::{
    semantic_analysis_context::SemanticAnalysisContext,
    statement::{Statement, StatementKind},
};

use crate::{
    cst::{CSTExpressionStatement, CSTStatement},
    expression::{is_expression_recovery, lower_expression, try_parse_expression},
    parser::Parser,
    span::span_of_cst_node,
    statement::{
        append::{lower_append_statement, try_parse_append_statement},
        block::{lower_block_statement, try_parse_block_statement},
        r#break::{lower_break_statement, try_parse_break_statement},
        r#continue::{lower_continue_statement, try_parse_continue_statement},
        r#if::{lower_if_statement, try_parse_if_statement},
        item::{lower_item_statement, try_parse_item_statement},
        r#let::{lower_let_statement, try_parse_let_statement},
        r#loop::{lower_loop_statement, try_parse_loop_statement},
        remove::{lower_remove_statement, try_parse_remove_statement},
        r#while::{lower_while_statement, try_parse_while_statement},
    },
    syntax::SyntaxKind,
};

pub mod append;
pub mod block;
pub mod r#break;
pub mod r#continue;
pub mod r#if;
pub mod item;
pub mod r#let;
pub mod r#loop;
pub mod remove;
pub mod r#while;

#[must_use]
pub fn is_statement_recovery(char: char) -> bool {
    is_expression_recovery(char) || char == '\n' || char == '{'
}

#[must_use]
pub fn try_parse_statement(parser: &mut Parser) -> bool {
    let Some(c) = parser.peek_char() else {
        return false;
    };

    #[allow(clippy::single_match_else)]
    match c {
        '{' => try_parse_block_statement(parser),
        _ => {
            if let Some(text) = parser.peek_identifier() {
                match text {
                    "if" => {
                        if try_parse_if_statement(parser) {
                            return true;
                        }
                    }
                    "let" => {
                        if try_parse_let_statement(parser) {
                            return true;
                        }
                    }
                    "while" => {
                        if try_parse_while_statement(parser) {
                            return true;
                        }
                    }
                    "loop" => {
                        if try_parse_loop_statement(parser) {
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

            if !try_parse_expression(parser) {
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
    }
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_expression_statement(
    node: CSTExpressionStatement,
    ctx: &mut SemanticAnalysisContext,
) -> Option<Statement> {
    let expression = lower_expression(node.expression()?, ctx)?;

    Some(StatementKind::Expression(expression).with_span(span_of_cst_node(&node)))
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_statement(node: CSTStatement, ctx: &mut SemanticAnalysisContext) -> Option<Statement> {
    match node {
        CSTStatement::BlockStatement(statement) => lower_block_statement(statement, ctx),
        CSTStatement::ExpressionStatement(node) => lower_expression_statement(node, ctx),
        CSTStatement::IfStatement(statement) => lower_if_statement(statement, ctx),
        CSTStatement::LetStatement(statement) => lower_let_statement(statement, ctx),
        CSTStatement::WhileStatement(node) => lower_while_statement(node, ctx),
        CSTStatement::LoopStatement(node) => lower_loop_statement(node, ctx),
        CSTStatement::BreakStatement(node) => lower_break_statement(node, ctx),
        CSTStatement::ContinueStatement(node) => lower_continue_statement(node, ctx),
        CSTStatement::AppendStatement(node) => lower_append_statement(node, ctx),
        CSTStatement::RemoveStatement(node) => lower_remove_statement(node, ctx),
        CSTStatement::ItemStatement(node) => lower_item_statement(node, ctx),
    }
}
