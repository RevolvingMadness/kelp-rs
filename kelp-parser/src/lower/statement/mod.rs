use kelp_core::statement::{Statement, StatementKind};

use crate::{
    cst::{CSTExpressionStatement, CSTStatement},
    lower::{
        data_type::generics::lower_generic_names,
        expression::{is_expression_recovery, lower_expression, try_parse_expression},
        statement::{
            block::{lower_block_statement, try_parse_block_statement},
            r#break::{lower_break_statement, try_parse_break_statement},
            r#continue::{lower_continue_statement, try_parse_continue_statement},
            r#if::{lower_if_statement, try_parse_if_statement},
            r#let::{lower_let_statement, try_parse_let_statement},
            mcfn_declaration::{
                lower_mcfn_declaration_statement, try_parse_mcfn_declaration_statement,
            },
            struct_declaration::{
                lower_struct_declaration_statement_field, try_parse_struct_declaration_statement,
            },
            type_alias_declaration::{
                lower_type_alias_declaration_statement, try_parse_type_alias_declaration_statement,
            },
            r#while::{lower_while_statement, try_parse_while_statement},
        },
    },
    parser::Parser,
    span::span_of_cst_node,
    syntax::SyntaxKind,
};

pub mod block;
pub mod r#break;
pub mod r#continue;
pub mod r#if;
pub mod r#let;
pub mod mcfn_declaration;
pub mod struct_declaration;
pub mod type_alias_declaration;
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
                    "mcfn" => {
                        if try_parse_mcfn_declaration_statement(parser) {
                            return true;
                        }
                    }
                    "struct" => {
                        if try_parse_struct_declaration_statement(parser) {
                            return true;
                        }
                    }
                    "while" => {
                        if try_parse_while_statement(parser) {
                            return true;
                        }
                    }
                    "type" => {
                        if try_parse_type_alias_declaration_statement(parser) {
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
                    _ => {}
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
pub fn lower_expression_statement(node: CSTExpressionStatement) -> Option<Statement> {
    let expression = lower_expression(node.expression()?)?;

    Some(StatementKind::Expression(expression).with_span(span_of_cst_node(&node)))
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_statement(node: CSTStatement) -> Option<Statement> {
    match node {
        CSTStatement::BlockStatement(statement) => lower_block_statement(statement),
        CSTStatement::ExpressionStatement(node) => lower_expression_statement(node),
        CSTStatement::IfStatement(statement) => lower_if_statement(statement),
        CSTStatement::LetStatement(statement) => lower_let_statement(statement),
        CSTStatement::MCFNDeclarationStatement(node) => lower_mcfn_declaration_statement(node),
        CSTStatement::StructDeclarationStatement(node) => {
            let span = span_of_cst_node(&node);

            let struct_name_token = node.name()?;
            let struct_name = struct_name_token.text().to_owned();

            let generics = node.generic_names().and_then(lower_generic_names);

            let fields = node
                .fields()
                .filter_map(lower_struct_declaration_statement_field)
                .collect();

            Some(
                StatementKind::StructDeclaration(struct_name, generics.unwrap_or_default(), fields)
                    .with_span(span),
            )
        }
        CSTStatement::WhileStatement(node) => lower_while_statement(node),
        CSTStatement::TypeAliasDeclarationStatement(node) => {
            lower_type_alias_declaration_statement(node)
        }
        CSTStatement::BreakStatement(node) => lower_break_statement(node),
        CSTStatement::ContinueStatement(node) => lower_continue_statement(node),
    }
}
