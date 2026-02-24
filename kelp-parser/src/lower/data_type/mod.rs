use std::collections::BTreeMap;

use kelp_core::{
    data_type::high::{HighDataType, HighDataTypeKind},
    high::snbt_string::HighSNBTString,
    span::Span,
};
use minecraft_command_types::snbt::SNBTString;

use crate::{
    cstlib::CSTNodeType,
    lower::data_type::{
        named::CSTNamedDataType, reference::CSTReferenceDataType, tuple::CSTTupleDataType,
        typed_compound::CSTTypedCompoundDataType, unit::CSTUnitDataType,
    },
    parser::Parser,
    semantic_token::{SemanticToken, SemanticTokenType},
    syntax::SyntaxKind,
};

pub mod named;
pub mod reference;
pub mod tuple;
pub mod typed_compound;
pub mod unit;

#[derive(Debug)]
pub enum CSTDataTypeKind<'a> {
    Reference(CSTReferenceDataType<'a>),
    Tuple(CSTTupleDataType<'a>),
    Unit(CSTUnitDataType<'a>),
    TypedCompound(CSTTypedCompoundDataType<'a>),
    Named(CSTNamedDataType<'a>),
}

impl<'a> CSTDataTypeKind<'a> {
    #[inline]
    #[must_use]
    pub fn with_span(self, span: Span) -> CSTDataType<'a> {
        CSTDataType { span, kind: self }
    }
}

#[derive(Debug)]
pub struct CSTDataType<'a> {
    pub span: Span,
    pub kind: CSTDataTypeKind<'a>,
}

impl<'a> CSTDataType<'a> {
    #[must_use]
    pub fn try_parse(parser: &mut Parser) -> bool {
        match parser.peek_char() {
            Some('&') => Self::parse_reference(parser),
            Some('(') => Self::parse_tuple_or_unit(parser),
            Some('{') => Self::parse_typed_compound(parser),
            _ => {
                if parser.peek_identifier().is_some() {
                    Self::parse_named(parser)
                } else {
                    false
                }
            }
        }
    }

    fn parse_reference(parser: &mut Parser) -> bool {
        let checkpoint = parser.checkpoint();
        parser.start_node_at(checkpoint, SyntaxKind::ReferenceDataType);

        parser.bump_char();

        if !Self::try_parse(parser) {
            parser.recover_newline("Expected data type after '&'");
        }

        parser.finish_node();
        true
    }

    fn parse_tuple_or_unit(parser: &mut Parser) -> bool {
        let checkpoint = parser.checkpoint();
        parser.bump_char();
        parser.skip_whitespace();

        if parser.peek_char() == Some(')') {
            parser.start_node_at(checkpoint, SyntaxKind::UnitDataType);
            parser.bump_char();
            parser.finish_node();
            return true;
        }

        let _ = Self::try_parse(parser);
        parser.skip_whitespace();

        while parser.try_bump_char(',') {
            parser.skip_whitespace();

            if parser.peek_char() == Some(')') {
                break;
            }

            let _ = Self::try_parse(parser);
            parser.skip_whitespace();
        }

        parser.start_node_at(checkpoint, SyntaxKind::TupleDataType);
        parser.expect_char(')', "Expected ')'");
        parser.finish_node();

        true
    }

    fn parse_typed_compound(parser: &mut Parser) -> bool {
        let checkpoint = parser.checkpoint();
        parser.start_node_at(checkpoint, SyntaxKind::TypedCompoundDataType);

        parser.bump_char();
        parser.skip_whitespace();

        while parser.peek_char() != Some('}') && parser.peek_char().is_some() {
            let field_checkpoint = parser.checkpoint();
            parser.start_node_at(field_checkpoint, SyntaxKind::TypedCompoundField);

            if let Some(text) = parser.peek_identifier() {
                parser.add_token(SyntaxKind::Identifier, text.len());
            } else {
                parser.error("Expected field name");
                parser.finish_node();
                break;
            }

            parser.skip_whitespace();
            parser.expect_char(':', "Expected ':' after field name");
            parser.skip_whitespace();

            if !Self::try_parse(parser) {
                parser.error("Expected data type");
            }

            parser.skip_whitespace();
            parser.finish_node();

            if parser.try_bump_char(',') {
                parser.skip_whitespace();
            } else {
                break;
            }
        }

        parser.expect_char('}', "Expected closing brace '}'");
        parser.finish_node();
        true
    }

    fn parse_named(parser: &mut Parser) -> bool {
        let checkpoint = parser.checkpoint();
        parser.start_node_at(checkpoint, SyntaxKind::NamedDataType);

        if let Some(text) = parser.peek_identifier() {
            parser.add_token(SyntaxKind::Identifier, text.len());
        }

        if parser.peek_char() == Some('<') {
            parser.bump_char();
            parser.skip_inline_whitespace();

            while parser.peek_char() != Some('>') && parser.peek_char().is_some() {
                if !Self::try_parse(parser) {
                    parser.error("Expected data type in generic arguments");
                    break;
                }

                parser.skip_inline_whitespace();

                if parser.try_bump_char(',') {
                    parser.skip_inline_whitespace();
                } else {
                    break;
                }
            }

            parser.expect_char('>', "Expected closing angle bracket '>'");
        }

        parser.finish_node();
        true
    }

    pub fn cast(node: &'a CSTNodeType) -> Option<Self> {
        Some(
            (match node.kind()? {
                SyntaxKind::ReferenceDataType => {
                    CSTDataTypeKind::Reference(CSTReferenceDataType::cast(node)?)
                }
                SyntaxKind::TupleDataType => CSTDataTypeKind::Tuple(CSTTupleDataType::cast(node)?),
                SyntaxKind::UnitDataType => CSTDataTypeKind::Unit(CSTUnitDataType::cast(node)?),
                SyntaxKind::TypedCompoundDataType => {
                    CSTDataTypeKind::TypedCompound(CSTTypedCompoundDataType::cast(node)?)
                }
                SyntaxKind::NamedDataType => CSTDataTypeKind::Named(CSTNamedDataType::cast(node)?),
                _ => {
                    #[cfg(debug_assertions)]
                    println!("Failed to cast node {:?} to CSTDataType", node);

                    return None;
                }
            })
            .with_span(node.span()),
        )
    }

    pub fn lower(self, text: &str) -> Option<HighDataType> {
        let kind = match self.kind {
            CSTDataTypeKind::Reference(data_type) => {
                let inner = data_type.inner()?.lower(text)?;
                HighDataTypeKind::Reference(Box::new(inner))
            }
            CSTDataTypeKind::Tuple(data_type) => {
                let data_types = data_type
                    .data_types()
                    .into_iter()
                    .filter_map(|data_type| data_type.lower(text))
                    .collect();
                HighDataTypeKind::Tuple(data_types)
            }
            CSTDataTypeKind::Unit(_) => HighDataTypeKind::Unit,
            CSTDataTypeKind::TypedCompound(data_type) => {
                let mut elements = BTreeMap::new();

                for field in data_type.fields() {
                    let name_token = field.name_token()?;
                    let data_type = field.data_type()?.lower(text)?;

                    elements.insert(
                        HighSNBTString {
                            span: name_token.span,
                            snbt_string: SNBTString(
                                false,
                                text[name_token.span.into_range()].to_string(),
                            ),
                        },
                        data_type,
                    );
                }

                HighDataTypeKind::TypedCompound(elements)
            }
            CSTDataTypeKind::Named(data_type) => {
                let name_token = data_type.name_token()?;
                let generics = data_type
                    .generics()
                    .into_iter()
                    .filter_map(|data_type| data_type.lower(text))
                    .collect();

                HighDataTypeKind::Named(
                    name_token.span,
                    text[name_token.span.into_range()].to_string(),
                    generics,
                )
            }
        };

        Some(HighDataType {
            span: self.span,
            kind,
        })
    }

    pub fn collect_semantic_tokens(&self, tokens: &mut Vec<SemanticToken>) {
        match &self.kind {
            CSTDataTypeKind::Reference(data_type) => {
                if let Some(inner) = data_type.inner() {
                    inner.collect_semantic_tokens(tokens);
                }
            }
            CSTDataTypeKind::Tuple(data_type) => {
                for data_type in data_type.data_types() {
                    data_type.collect_semantic_tokens(tokens);
                }
            }
            CSTDataTypeKind::Unit(_) => {}
            CSTDataTypeKind::TypedCompound(data_type) => {
                for field in data_type.fields() {
                    if let Some(name_token) = field.name_token() {
                        tokens.push(SemanticToken::new(
                            name_token.span,
                            SemanticTokenType::Variable,
                        ));
                    }
                    if let Some(data_type) = field.data_type() {
                        data_type.collect_semantic_tokens(tokens);
                    }
                }
            }
            CSTDataTypeKind::Named(data_type) => {
                if let Some(name_token) = data_type.name_token() {
                    tokens.push(SemanticToken::new(
                        name_token.span,
                        SemanticTokenType::Class,
                    ));
                }

                for generic in data_type.generics() {
                    generic.collect_semantic_tokens(tokens);
                }
            }
        }
    }
}
