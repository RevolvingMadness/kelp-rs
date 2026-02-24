use kelp_core::{
    high::snbt_string::HighSNBTString,
    pattern::{Pattern, PatternKind},
    span::Span,
};
use minecraft_command_types::snbt::SNBTString;

use crate::{
    cstlib::CSTNodeType,
    lower::pattern::{
        binding::CSTBindingPattern, r#struct::CSTStructPattern, tuple::CSTTuplePattern,
        wildcard::CSTWildcardPattern,
    },
    parser::Parser,
    semantic_token::SemanticToken,
    syntax::SyntaxKind,
};

pub mod binding;
pub mod r#struct;
pub mod tuple;
pub mod wildcard;

#[derive(Debug)]
pub enum CSTPatternKind<'a> {
    Wildcard(CSTWildcardPattern<'a>),
    Tuple(CSTTuplePattern<'a>),
    Binding(CSTBindingPattern<'a>),
    Struct(CSTStructPattern<'a>),
}

impl<'a> CSTPatternKind<'a> {
    #[inline]
    #[must_use]
    pub fn with_span(self, span: Span) -> CSTPattern<'a> {
        CSTPattern { span, kind: self }
    }
}

#[derive(Debug)]
pub struct CSTPattern<'a> {
    pub span: Span,
    pub kind: CSTPatternKind<'a>,
}

impl<'a> CSTPattern<'a> {
    #[must_use]
    pub fn try_parse(parser: &mut Parser) -> bool {
        match parser.peek_char() {
            Some('(') => {
                parser.start_node(SyntaxKind::TuplePattern);

                parser.bump_char();

                parser.skip_whitespace();

                if !CSTPattern::try_parse(parser) {
                    parser.error("Expected pattern");
                }

                parser.skip_whitespace();

                if parser.peek_char() == Some(',') {
                    while parser.try_bump_char(',') {
                        parser.skip_whitespace();

                        if parser.peek_char() == Some(')') {
                            break;
                        }

                        if !CSTPattern::try_parse(parser) {
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
            _ => match parser.peek_identifier() {
                Some("_") => {
                    parser.start_bump_finish_node(SyntaxKind::WildcardPattern, "_".len());

                    true
                }
                Some(name) => {
                    let checkpoint = parser.checkpoint();

                    parser.bump_identifier(name);

                    parser.skip_whitespace();

                    if parser.peek_char() == Some('{') {
                        parser.start_node_at(checkpoint, SyntaxKind::StructPattern);

                        parser.bump_char();
                        parser.skip_whitespace();

                        while let Some(field_name) = parser.peek_identifier() {
                            let field_checkpoint = parser.checkpoint();
                            parser.bump_identifier(field_name);
                            parser.skip_whitespace();

                            if parser.peek_char() == Some(':') {
                                parser.bump_char();
                                parser.skip_whitespace();
                                if !CSTPattern::try_parse(parser) {
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
    pub fn cast(node: &'a CSTNodeType) -> Option<CSTPattern<'a>> {
        Some(
            (match node.kind()? {
                SyntaxKind::WildcardPattern => {
                    CSTPatternKind::Wildcard(CSTWildcardPattern::cast(node)?)
                }
                SyntaxKind::TuplePattern => CSTPatternKind::Tuple(CSTTuplePattern::cast(node)?),
                SyntaxKind::BindingPattern => {
                    CSTPatternKind::Binding(CSTBindingPattern::cast(node)?)
                }
                SyntaxKind::StructPattern => CSTPatternKind::Struct(CSTStructPattern::cast(node)?),
                _ => {
                    #[cfg(debug_assertions)]
                    println!("Failed to cast node {:?} to CSTPattern", node);

                    return None;
                }
            })
            .with_span(node.span()),
        )
    }

    #[must_use]
    pub fn lower(self, text: &str) -> Option<Pattern> {
        Some(
            (match self.kind {
                CSTPatternKind::Wildcard(_) => PatternKind::Wildcard,
                CSTPatternKind::Tuple(pattern) => {
                    let patterns = pattern
                        .patterns()
                        .filter_map(|pattern| pattern.lower(text))
                        .collect();

                    PatternKind::Tuple(patterns)
                }
                CSTPatternKind::Binding(pattern) => {
                    let name = pattern.name(text)?.to_string();

                    PatternKind::Binding(name)
                }
                CSTPatternKind::Struct(pattern) => {
                    let struct_name = pattern.name(text)?.to_string();

                    let fields = pattern
                        .fields()
                        .into_iter()
                        .filter_map(|field| {
                            let (field_name_span, field_name) = field.name(text)?;
                            let field_name = field_name.to_string();

                            let field_pattern =
                                field.pattern().and_then(|pattern| pattern.lower(text));

                            Some((
                                HighSNBTString {
                                    snbt_string: SNBTString(false, field_name),
                                    span: field_name_span,
                                },
                                field_pattern,
                            ))
                        })
                        .collect();

                    PatternKind::Struct(struct_name, fields)
                }
            })
            .with_span(self.span),
        )
    }

    pub fn collect_semantic_tokens(&self, tokens: &mut Vec<SemanticToken>) {
        match &self.kind {
            CSTPatternKind::Wildcard(pattern) => {
                pattern.collect_semantic_tokens(tokens);
            }
            CSTPatternKind::Tuple(pattern) => {
                pattern.collect_semantic_tokens(tokens);
            }
            CSTPatternKind::Binding(pattern) => {
                pattern.collect_semantic_tokens(tokens);
            }
            CSTPatternKind::Struct(pattern) => {
                pattern.collect_semantic_tokens(tokens);
            }
        }
    }
}
