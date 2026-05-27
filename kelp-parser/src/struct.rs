use std::collections::HashMap;

use kelp_core::parsed::data_type::ParsedDataType;

use crate::{
    cst::{CSTDataType, CSTStructField, CSTStructFields, CSTTupleField, CSTTupleFields},
    extension_traits::{LowerableAstNode, ParsableAstNode},
    lower_context::LowerContext,
    parser::Parser,
    syntax::SyntaxKind,
};

impl LowerableAstNode for CSTStructField {
    type Lowered = (String, ParsedDataType);

    fn lower(self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let name_token = self.name()?;
        let name = name_token.text();

        let data_type = self.data_type()?.lower(ctx)?;

        Some((name.to_owned(), data_type))
    }
}

#[must_use]
pub fn try_parse_struct_field(parser: &mut Parser) -> bool {
    let checkpoint = parser.mark();

    if !parser.try_bump_identifier_kind(SyntaxKind::StructFieldName) {
        return false;
    }

    checkpoint.start_node(parser, SyntaxKind::StructField);

    parser.skip_whitespace();

    let parsed_colon = parser.expect_char(':', "Expected ':'");

    parser.skip_whitespace();

    if !CSTDataType::try_parse(parser) && parsed_colon {
        parser.error("Expected data type");
    }

    parser.finish_node();

    true
}

impl LowerableAstNode for CSTStructFields {
    type Lowered = HashMap<String, ParsedDataType>;

    fn lower(self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let fields = self
            .struct_fields()
            .filter_map(|field| field.lower(ctx))
            .collect();

        Some(fields)
    }
}

#[must_use]
pub fn try_parse_struct_fields(parser: &mut Parser) -> bool {
    let checkpoint = parser.mark();

    if !try_parse_struct_field(parser) {
        return false;
    }

    checkpoint.start_node(parser, SyntaxKind::StructFields);

    loop {
        let state = parser.save_state();
        parser.skip_whitespace();

        if !parser.try_bump_char(',') {
            state.restore(parser);
            break;
        }

        parser.skip_whitespace();

        if !try_parse_struct_field(parser) {
            break;
        }
    }

    parser.finish_node();
    true
}

impl LowerableAstNode for CSTTupleField {
    type Lowered = ParsedDataType;

    fn lower(self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let data_type = self.data_type()?.lower(ctx)?;

        Some(data_type)
    }
}

#[must_use]
pub fn try_parse_tuple_field(parser: &mut Parser) -> bool {
    let checkpoint = parser.mark();

    if !CSTDataType::try_parse(parser) {
        return false;
    }

    checkpoint.start_node(parser, SyntaxKind::TupleField);

    parser.finish_node();

    true
}

impl LowerableAstNode for CSTTupleFields {
    type Lowered = Vec<ParsedDataType>;

    fn lower(self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let fields = self
            .tuple_fields()
            .filter_map(|field| field.lower(ctx))
            .collect();

        Some(fields)
    }
}

#[must_use]
pub fn try_parse_tuple_fields(parser: &mut Parser) -> bool {
    let checkpoint = parser.mark();

    if !try_parse_tuple_field(parser) {
        return false;
    }

    checkpoint.start_node(parser, SyntaxKind::TupleFields);

    loop {
        let state = parser.save_state();
        parser.skip_whitespace();

        if !parser.try_bump_char(',') {
            state.restore(parser);
            break;
        }

        parser.skip_whitespace();
        if !try_parse_tuple_field(parser) {
            break;
        }
    }

    parser.finish_node();
    true
}
