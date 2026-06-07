use kelp_core::parsed::pattern::ParsedPattern;

use crate::{
    cst::{
        CSTDataPattern, CSTGenericPath, CSTPattern, CSTRegularStructPatternFields, CSTScorePattern,
        CSTTupleStructPatternFields,
    },
    extension_traits::{LowerableAstNode, ParsableAstNode},
    lower_context::LowerContext,
    parser::Parser,
    syntax::SyntaxKind,
};

pub mod binding;
pub mod compound;
pub mod data;
pub mod score;
pub mod r#struct;
pub mod tuple;
pub mod wildcard;

impl ParsableAstNode for CSTPattern {
    fn try_parse(parser: &mut Parser) -> bool {
        match parser.peek_char() {
            Some('(') => {
                parser.start_node(SyntaxKind::TuplePattern);

                parser.bump_char();

                parser.skip_whitespace();

                Self::expect(parser, "Expected pattern");

                parser.skip_whitespace();

                if parser.peek_char() == Some(',') {
                    while parser.try_bump_char(',') {
                        parser.skip_whitespace();

                        if parser.peek_char() == Some(')') {
                            break;
                        }

                        Self::expect(parser, "Expected pattern");

                        parser.skip_whitespace();
                    }
                }

                parser.skip_whitespace();

                parser.expect_char(')');

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

                        Self::expect(parser, "Expected pattern");
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

                parser.expect_char('}');

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
                Some(..) => {
                    if CSTScorePattern::try_parse(parser) {
                        return true;
                    }

                    if CSTDataPattern::try_parse(parser) {
                        return true;
                    }

                    let marker = parser.mark();

                    assert!(CSTGenericPath::try_parse(parser));

                    let state = parser.save_state();

                    parser.skip_whitespace();

                    match parser.peek_char() {
                        Some('{') => {
                            marker.replace_token(parser, SyntaxKind::TypeName);
                            marker.start_node(parser, SyntaxKind::RegularStructPattern);

                            parser.bump_char();

                            parser.skip_whitespace();

                            let _ = CSTRegularStructPatternFields::try_parse(parser);

                            parser.skip_whitespace();

                            parser.expect_char('}');
                        }
                        Some('(') => {
                            marker.replace_token(parser, SyntaxKind::TypeName);
                            marker.start_node(parser, SyntaxKind::TupleStructPattern);

                            parser.bump_char();

                            parser.skip_whitespace();

                            let _ = CSTTupleStructPatternFields::try_parse(parser);

                            parser.skip_whitespace();

                            parser.expect_char(')');
                        }
                        _ => {
                            state.restore(parser);

                            marker.start_node(parser, SyntaxKind::PathPattern);
                        }
                    }

                    parser.finish_node();

                    true
                }
                _ => false,
            },
        }
    }
}

impl LowerableAstNode for CSTPattern {
    type Lowered = ParsedPattern;

    fn lower(&self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        match self {
            Self::WildcardPattern(node) => node.lower(ctx),
            Self::TuplePattern(node) => node.lower(ctx),
            Self::PathPattern(node) => node.lower(ctx),
            Self::ScorePattern(node) => node.lower(ctx),
            Self::DataPattern(node) => node.lower(ctx),
            Self::RegularStructPattern(node) => node.lower(ctx),
            Self::TupleStructPattern(node) => node.lower(ctx),
            Self::CompoundPattern(node) => node.lower(ctx),
        }
    }
}
