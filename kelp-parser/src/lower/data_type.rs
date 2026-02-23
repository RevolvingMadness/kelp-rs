use std::collections::BTreeMap;

use kelp_core::{
    data_type::high::{HighDataType, HighDataTypeKind},
    high::snbt_string::HighSNBTString,
    span::Span,
};
use minecraft_command_types::snbt::SNBTString;

use crate::{
    cst_node,
    cstlib::{CSTNodeType, token::CSTToken},
    parser::Parser,
    syntax::SyntaxKind,
};

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

    pub fn expect(parser: &mut Parser) -> bool {
        if Self::try_parse(parser) {
            true
        } else {
            parser.error("Expected data type");

            false
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
        parser.expect_char(')', "Expected closing parenthesis ')'");
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

    pub fn lower(self) -> Option<HighDataType> {
        let kind = match self.kind {
            CSTDataTypeKind::Reference(ty) => {
                let inner = ty.inner()?.lower()?;
                HighDataTypeKind::Reference(Box::new(inner))
            }
            CSTDataTypeKind::Tuple(ty) => {
                let data_types = ty
                    .data_types()
                    .into_iter()
                    .filter_map(CSTDataType::lower)
                    .collect();
                HighDataTypeKind::Tuple(data_types)
            }
            CSTDataTypeKind::Unit(_) => HighDataTypeKind::Unit,
            CSTDataTypeKind::TypedCompound(ty) => {
                let mut elements = BTreeMap::new();

                for field in ty.fields() {
                    let name_token = field.name_token()?;
                    let data_type = field.data_type()?.lower()?;

                    elements.insert(
                        HighSNBTString {
                            span: name_token.span,
                            snbt_string: SNBTString(false, name_token.text.to_string()),
                        },
                        data_type,
                    );
                }

                HighDataTypeKind::TypedCompound(elements)
            }
            CSTDataTypeKind::Named(ty) => {
                let name_token = ty.name_token()?;
                let generics = ty
                    .generics()
                    .into_iter()
                    .filter_map(CSTDataType::lower)
                    .collect();

                HighDataTypeKind::Named(name_token.span, name_token.text.to_string(), generics)
            }
        };

        Some(HighDataType {
            span: self.span,
            kind,
        })
    }
}

cst_node!(CSTReferenceDataType, SyntaxKind::ReferenceDataType);

impl<'a> CSTReferenceDataType<'a> {
    pub fn inner(&self) -> Option<CSTDataType<'a>> {
        self.0.children().find_map(CSTDataType::cast)
    }
}

cst_node!(CSTTupleDataType, SyntaxKind::TupleDataType);

impl<'a> CSTTupleDataType<'a> {
    pub fn data_types(&self) -> Vec<CSTDataType<'a>> {
        self.0.children().filter_map(CSTDataType::cast).collect()
    }
}

cst_node!(CSTUnitDataType, SyntaxKind::UnitDataType);

cst_node!(CSTTypedCompoundDataType, SyntaxKind::TypedCompoundDataType);

impl<'a> CSTTypedCompoundDataType<'a> {
    pub fn fields(&self) -> Vec<CSTTypedCompoundField<'a>> {
        self.0
            .children()
            .filter_map(CSTTypedCompoundField::cast)
            .collect()
    }
}

cst_node!(CSTTypedCompoundField, SyntaxKind::TypedCompoundField);

impl<'a> CSTTypedCompoundField<'a> {
    pub fn name_token(&self) -> Option<&'a CSTToken<'a>> {
        self.0
            .children_tokens()
            .find(|t| t.kind == SyntaxKind::Identifier)
    }

    pub fn data_type(&self) -> Option<CSTDataType<'a>> {
        self.0.children().find_map(CSTDataType::cast)
    }
}

cst_node!(CSTNamedDataType, SyntaxKind::NamedDataType);

impl<'a> CSTNamedDataType<'a> {
    pub fn name_token(&self) -> Option<&'a CSTToken<'a>> {
        self.0
            .children_tokens()
            .find(|t| t.kind == SyntaxKind::Identifier)
    }

    pub fn generics(&self) -> Vec<CSTDataType<'a>> {
        self.0.children().filter_map(CSTDataType::cast).collect()
    }
}
