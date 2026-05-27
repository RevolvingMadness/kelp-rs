use kelp_core::parsed::statement::ParsedStatement;

use crate::{
    cst::{
        CSTAppendStatement, CSTBreakStatement, CSTContinueStatement, CSTExpressionWithBlock,
        CSTExpressionWithoutBlock, CSTItemStatement, CSTLetStatement, CSTRemoveStatement,
        CSTStatement,
    },
    expression::is_expression_recovery,
    extension_traits::{LowerableAstNode, ParsableAstNode},
    lower_context::LowerContext,
    parser::Parser,
    syntax::SyntaxKind,
};

pub mod append;
pub mod r#break;
pub mod r#continue;
pub mod expression;
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

    state.restore(parser);
    parser.error("Expected ';'");

    false
}

impl ParsableAstNode for CSTStatement {
    fn try_parse(parser: &mut Parser) -> bool {
        if let Some(text) = parser.peek_identifier() {
            match text {
                "let" => {
                    if CSTLetStatement::try_parse(parser) {
                        return true;
                    }
                }
                "break" => {
                    if CSTBreakStatement::try_parse(parser) {
                        return true;
                    }
                }
                "continue" => {
                    if CSTContinueStatement::try_parse(parser) {
                        return true;
                    }
                }
                "append" => {
                    if CSTAppendStatement::try_parse(parser) {
                        return true;
                    }
                }
                "remove" => {
                    if CSTRemoveStatement::try_parse(parser) {
                        return true;
                    }
                }
                _ => {
                    if CSTItemStatement::try_parse(parser) {
                        return true;
                    }
                }
            }
        }

        parser.start_node(SyntaxKind::ExpressionStatement);

        if CSTExpressionWithBlock::try_parse(parser) {
            let state = parser.save_state();
            parser.skip_whitespace();

            if !parser.try_bump_char(';') {
                state.restore(parser);
            }
        } else if CSTExpressionWithoutBlock::try_parse(parser) {
            let state = parser.save_state();
            parser.skip_whitespace();

            if !parser.try_bump_char(';') {
                let next_char = parser.peek_char();

                state.restore(parser);

                if let Some(next_char) = next_char
                    && next_char != '}'
                {
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
            }
        }

        parser.finish_node();

        true
    }
}

impl LowerableAstNode for CSTStatement {
    type Lowered = ParsedStatement;

    fn lower(&self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        match self {
            Self::ExpressionStatement(node) => node.lower(ctx),
            Self::LetStatement(node) => node.lower(ctx),
            Self::BreakStatement(node) => node.lower(ctx),
            Self::ContinueStatement(node) => node.lower(ctx),
            Self::AppendStatement(node) => node.lower(ctx),
            Self::RemoveStatement(node) => node.lower(ctx),
            Self::ItemStatement(node) => node.lower(ctx),
        }
    }
}
