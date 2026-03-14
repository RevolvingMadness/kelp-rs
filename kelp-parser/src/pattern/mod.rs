use kelp_core::pattern::Pattern;

use crate::{
    cst::CSTPattern,
    parser::Parser,
    pattern::{
        binding::lower_binding_pattern, compound::lower_compound_pattern,
        r#struct::lower_struct_pattern, tuple::lower_tuple_pattern,
        wildcard::lower_wildcard_pattern,
    },
    syntax::SyntaxKind,
};

pub mod binding;
pub mod compound;
pub mod r#struct;
pub mod tuple;
pub mod wildcard;

#[must_use]
pub fn try_parse_pattern(parser: &mut Parser) -> bool {
    match parser.peek_char() {
        Some('(') => {
            parser.start_node(SyntaxKind::TuplePattern);

            parser.bump_char();

            parser.skip_whitespace();

            if !try_parse_pattern(parser) {
                parser.error("Expected pattern");
            }

            parser.skip_whitespace();

            if parser.peek_char() == Some(',') {
                while parser.try_bump_char(',') {
                    parser.skip_whitespace();

                    if parser.peek_char() == Some(')') {
                        break;
                    }

                    if !try_parse_pattern(parser) {
                        parser.error("Expected pattern");
                    }

                    parser.skip_whitespace();
                }
            }

            parser.skip_whitespace();

            parser.expect_char(')', "Expected ')'");

            parser.finish_node();

            true
        }
        Some('{') => {
            parser.start_node(SyntaxKind::CompoundPattern);

            parser.bump_char();

            parser.skip_whitespace();

            while let Some(key) = parser.peek_identifier() {
                parser.start_node(SyntaxKind::CompoundPatternEntry);

                parser.bump_identifier_kind(SyntaxKind::CompoundKey, key);
                parser.skip_whitespace();

                if parser.try_bump_char(':') {
                    parser.skip_whitespace();

                    if !try_parse_pattern(parser) {
                        parser.error("Expected pattern");
                    }
                }

                parser.skip_whitespace();

                parser.finish_node();

                if parser.peek_char() == Some(',') {
                    parser.bump_char();
                    parser.skip_whitespace();
                } else {
                    break;
                }
            }

            parser.expect_char('}', "Expected closing brace after struct pattern");

            parser.finish_node();

            true
        }
        _ => match parser.peek_identifier() {
            Some("_") => {
                parser.start_node(SyntaxKind::WildcardPattern);
                parser.bump_char();
                parser.finish_node();

                true
            }
            Some(name) => {
                let checkpoint = parser.checkpoint();

                let pos = parser.bump_identifier_kind(SyntaxKind::BindingPatternName, name);

                parser.skip_whitespace();

                if parser.peek_char() == Some('{') {
                    parser.start_node_at(checkpoint, SyntaxKind::StructPattern);

                    parser.replace_token_at(pos, SyntaxKind::StructName);

                    parser.bump_char();
                    parser.skip_whitespace();

                    while let Some(field_name) = parser.peek_identifier() {
                        let field_checkpoint = parser.checkpoint();
                        parser.bump_identifier_kind(SyntaxKind::StructFieldName, field_name);
                        parser.skip_whitespace();

                        if parser.try_bump_char(':') {
                            parser.skip_whitespace();

                            if !try_parse_pattern(parser) {
                                parser.error("Expected pattern");
                            }
                        }

                        parser.start_node_at(field_checkpoint, SyntaxKind::StructPatternField);
                        parser.finish_node();

                        parser.skip_whitespace();
                        if parser.peek_char() == Some(',') {
                            parser.bump_char();
                            parser.skip_whitespace();
                        } else {
                            break;
                        }
                    }

                    parser.expect_char('}', "Expected closing brace after struct pattern");
                } else {
                    parser.start_node_at(checkpoint, SyntaxKind::BindingPattern);
                }

                parser.finish_node();

                true
            }
            _ => false,
        },
    }
}

#[must_use]
pub fn lower_pattern(node: CSTPattern) -> Option<Pattern> {
    match node {
        CSTPattern::WildcardPattern(node) => lower_wildcard_pattern(node),
        CSTPattern::TuplePattern(node) => lower_tuple_pattern(node),
        CSTPattern::BindingPattern(node) => lower_binding_pattern(node),
        CSTPattern::StructPattern(node) => lower_struct_pattern(node),
        CSTPattern::CompoundPattern(node) => lower_compound_pattern(node),
    }
}
