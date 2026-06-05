use kelp_core::parsed::data_type::ParsedDataType;

use crate::{
    cst::{CSTDataType, CSTPathDataType, CSTReferenceDataType, CSTTypedCompoundDataTypeField},
    data_type::typed_compound::try_parse_typed_compound_data_type,
    extension_traits::{LowerableAstNode, ParsableAstNode},
    lower_context::LowerContext,
    parser::Parser,
    syntax::SyntaxKind,
};

pub mod generics;
pub mod inferred;
pub mod path;
pub mod reference;
pub mod typed_compound;

impl ParsableAstNode for CSTDataType {
    fn try_parse(parser: &mut Parser) -> bool {
        match parser.peek_char() {
            Some('&') => CSTReferenceDataType::try_parse(parser),
            Some('(') => try_parse_tuple_or_unit_data_type(parser),
            Some('!') => {
                parser.start_node(SyntaxKind::NeverDataType);
                parser.bump_char();
                parser.finish_node();

                true
            }
            Some('{') => try_parse_typed_compound_data_type(parser),
            _ => parser.peek_identifier().is_some_and(|identifier| {
                if identifier == "_" {
                    parser.start_node(SyntaxKind::InferredDataType);
                    parser.bump_char();
                    parser.finish_node();

                    true
                } else {
                    CSTPathDataType::try_parse(parser)
                }
            }),
        }
    }
}

#[must_use]
fn try_parse_tuple_or_unit_data_type(parser: &mut Parser) -> bool {
    let marker = parser.mark();
    parser.bump_char();
    parser.skip_whitespace();

    if parser.peek_char() == Some(')') {
        marker.start_node(parser, SyntaxKind::UnitDataType);
        parser.bump_char();
        parser.finish_node();
        return true;
    }

    let _ = CSTDataType::try_parse(parser);
    parser.skip_whitespace();

    while parser.try_bump_char(',') {
        parser.skip_whitespace();

        if parser.peek_char() == Some(')') {
            break;
        }

        let _ = CSTDataType::try_parse(parser);
        parser.skip_whitespace();
    }

    marker.start_node(parser, SyntaxKind::TupleDataType);
    parser.expect_char(')');
    parser.finish_node();

    true
}

impl LowerableAstNode for CSTTypedCompoundDataTypeField {
    type Lowered = (String, ParsedDataType);

    fn lower(&self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let name_token = self.name()?;
        let name = name_token.text();
        let data_type = self.data_type()?.lower(ctx)?;

        Some((name.to_owned(), data_type))
    }
}

impl LowerableAstNode for CSTDataType {
    type Lowered = ParsedDataType;

    fn lower(&self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        match self {
            Self::ReferenceDataType(node) => node.lower(ctx),
            Self::TupleDataType(node) => {
                let data_types = node
                    .data_types()
                    .filter_map(|data_type| data_type.lower(ctx))
                    .collect();

                Some(ParsedDataType::Tuple(data_types))
            }
            Self::UnitDataType(..) => Some(ParsedDataType::Unit),
            Self::NeverDataType(..) => Some(ParsedDataType::Never),
            Self::TypedCompoundDataType(data_type) => {
                let fields = data_type
                    .fields()
                    .filter_map(|field| field.lower(ctx))
                    .collect();

                Some(ParsedDataType::TypedCompound(fields))
            }
            Self::PathDataType(node) => node.lower(ctx),
            Self::InferredDataType(node) => node.lower(ctx),
        }
    }
}
