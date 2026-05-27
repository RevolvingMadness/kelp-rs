use std::collections::HashMap;

use kelp_core::{
    parsed::expression::ParsedExpression, span::Span, trait_ext::CollectOptionAllIterExt,
};

use crate::{
    cst::{CSTCompoundExpression, CSTCompoundExpressionEntry, CSTExpression},
    extension_traits::{AstNodeExt, LowerableAstNode, ParsableAstNode},
    lower_context::LowerContext,
    parser::Parser,
    syntax::SyntaxKind,
};

impl ParsableAstNode for CSTCompoundExpression {
    fn try_parse(parser: &mut Parser) -> bool {
        if parser.peek_char() != Some('{') {
            return false;
        }

        parser.start_node(SyntaxKind::CompoundExpression);
        parser.bump_char();

        loop {
            parser.skip_whitespace();

            if parser.is_eof() || parser.peek_char() == Some('}') {
                break;
            }

            parser.start_node(SyntaxKind::CompoundExpressionEntry);

            if !parser.try_parse_string_or_identifier_kind(SyntaxKind::CompoundKey) {
                parser.error("Expected compound key");
                bump_until_next_compound_entry_or_end(parser);
                parser.finish_node();

                continue;
            }

            parser.skip_whitespace();

            if !parser.expect_char(':', "Expected ':'") {
                bump_until_next_compound_entry_or_end(parser);
                parser.finish_node();

                continue;
            }

            parser.skip_whitespace();

            if !CSTExpression::try_parse(parser) {
                parser.error("Expected expression");

                bump_until_next_compound_entry_or_end(parser);

                parser.finish_node();

                continue;
            }

            parser.finish_node();
            parser.skip_whitespace();

            if !parser.try_bump_char(',') && parser.peek_char() != Some('}') {
                parser.error("Expected ',' or '}'");
                bump_until_next_compound_entry_or_end(parser);
            }
        }

        parser.expect_char('}', "Expected '}'");
        parser.finish_node();

        true
    }
}

fn bump_until_next_compound_entry_or_end(parser: &mut Parser) {
    let chars = parser.source[parser.pos..].chars();
    let mut length = 0;

    for char in chars {
        if char == ',' || char == '}' || char == '\n' {
            break;
        }

        length += char.len_utf8();
    }

    if length > 0 {
        parser.add_token(SyntaxKind::Garbage, length);
    }

    if parser.peek_char() == Some(',') {
        parser.bump_char();
    }
}

impl LowerableAstNode for CSTCompoundExpressionEntry {
    type Lowered = (String, ParsedExpression);

    fn lower(self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let key_token = self.name()?;
        let value = self.value()?;
        let value = value.lower(ctx)?;

        let key = key_token.text();

        Some((key.to_owned(), value))
    }
}

impl LowerableAstNode for CSTCompoundExpression {
    type Lowered = (Span, HashMap<String, ParsedExpression>);

    fn lower(self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let compound = self
            .entries()
            .map(|entry| entry.lower(ctx))
            .collect_option_all()?;

        Some((self.span(), compound))
    }
}
