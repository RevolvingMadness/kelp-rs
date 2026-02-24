use std::collections::BTreeMap;

use kelp_core::{expression::Expression, high::snbt_string::HighSNBTString};
use minecraft_command_types::snbt::SNBTString;

use crate::{
    cst_node,
    lower::expression::{CSTExpression, compound::entry::CSTCompoundExpressionEntry},
    parser::Parser,
    semantic_token::SemanticToken,
    syntax::SyntaxKind,
};

pub mod entry;

cst_node!(CSTCompoundExpression, SyntaxKind::CompoundExpression);

impl<'a> CSTCompoundExpression<'a> {
    pub fn try_parse(parser: &mut Parser) -> bool {
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

            if !parser.try_parse_string_or_identifier() {
                parser.error("Expected compound key");
                CSTCompoundExpression::bump_until_next_compound_entry_or_end(parser);
                parser.finish_node();

                continue;
            }

            parser.skip_whitespace();

            if !parser.expect_char(':', "Expected ':'") {
                CSTCompoundExpression::bump_until_next_compound_entry_or_end(parser);
                parser.finish_node();

                continue;
            }

            parser.skip_whitespace();

            if !CSTExpression::try_parse(parser) {
                parser.error("Expected expression");
                CSTCompoundExpression::bump_until_next_compound_entry_or_end(parser);
                parser.finish_node();

                continue;
            }

            parser.finish_node();
            parser.skip_whitespace();

            if !parser.try_bump_char(',') && parser.peek_char() != Some('}') {
                parser.error("Expected ',' or '}'");
                CSTCompoundExpression::bump_until_next_compound_entry_or_end(parser);
            }
        }

        parser.expect_char('}', "Expected '}'");
        parser.finish_node();

        true
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

    pub fn entries(&self) -> impl Iterator<Item = CSTCompoundExpressionEntry<'a>> {
        self.0
            .children()
            .filter_map(CSTCompoundExpressionEntry::cast)
    }

    #[must_use]
    pub fn lower(self, text: &str) -> BTreeMap<HighSNBTString, Expression> {
        let mut compound = BTreeMap::new();

        for entry in self.entries() {
            if let (Some((key_span, key)), Some(value)) = (entry.key(text), entry.value())
                && let Some(value) = value.lower(text)
            {
                compound.insert(
                    HighSNBTString {
                        span: key_span,
                        snbt_string: SNBTString(false, key.to_string()),
                    },
                    value,
                );
            }
        }

        compound
    }

    pub fn collect_semantic_tokens(&self, tokens: &mut Vec<SemanticToken>) {
        for entry in self.entries() {
            entry.collect_semantic_tokens(tokens);
        }
    }
}
