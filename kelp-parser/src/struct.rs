use std::collections::HashMap;

use kelp_core::{parsed::data_type::ParsedDataType, trait_ext::CollectOptionAllIterExt};

use crate::{
    cst::{
        CSTDataType, CSTRegularStructField, CSTRegularStructFields, CSTTupleStructField,
        CSTTupleStructFields,
    },
    extension_traits::{LowerableAstNode, ParsableAstNode},
    lower_context::LowerContext,
    parser::Parser,
    syntax::SyntaxKind,
};

impl LowerableAstNode for CSTRegularStructField {
    type Lowered = (String, ParsedDataType);

    fn lower(&self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let name_token = self.name()?;
        let name = name_token.text();

        let data_type = self.data_type()?.lower(ctx)?;

        Some((name.to_owned(), data_type))
    }
}

impl ParsableAstNode for CSTRegularStructField {
    fn try_parse(parser: &mut Parser) -> bool {
        let marker = parser.mark();

        if !parser.try_bump_identifier_kind(SyntaxKind::StructFieldName) {
            return false;
        }

        marker.start_node(parser, SyntaxKind::RegularStructField);

        parser.skip_whitespace();

        let parsed_colon = parser.expect_char(':');

        parser.skip_whitespace();

        if !CSTDataType::try_parse(parser) && parsed_colon {
            parser.error("Expected data type");
        }

        parser.finish_node();

        true
    }
}

impl LowerableAstNode for CSTRegularStructFields {
    type Lowered = HashMap<String, ParsedDataType>;

    fn lower(&self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let fields = self
            .regular_struct_fields()
            .filter_map(|field| field.lower(ctx))
            .collect();

        Some(fields)
    }
}

impl ParsableAstNode for CSTRegularStructFields {
    fn try_parse(parser: &mut Parser) -> bool {
        let marker = parser.mark();

        if !CSTRegularStructField::try_parse(parser) {
            return false;
        }

        marker.start_node(parser, SyntaxKind::RegularStructFields);

        loop {
            let state = parser.save_state();
            parser.skip_whitespace();

            if !parser.try_bump_char(',') {
                state.restore(parser);
                break;
            }

            parser.skip_whitespace();

            if !CSTRegularStructField::try_parse(parser) {
                break;
            }
        }

        parser.finish_node();
        true
    }
}

impl ParsableAstNode for CSTTupleStructField {
    fn try_parse(parser: &mut Parser) -> bool {
        let marker = parser.mark();

        if !CSTDataType::try_parse(parser) {
            return false;
        }

        marker.start_node(parser, SyntaxKind::TupleStructField);

        parser.finish_node();

        true
    }
}

impl LowerableAstNode for CSTTupleStructField {
    type Lowered = ParsedDataType;

    fn lower(&self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let data_type = self.data_type()?.lower(ctx)?;

        Some(data_type)
    }
}

impl ParsableAstNode for CSTTupleStructFields {
    fn try_parse(parser: &mut Parser) -> bool {
        let marker = parser.mark();

        if !CSTTupleStructField::try_parse(parser) {
            return false;
        }

        marker.start_node(parser, SyntaxKind::TupleStructFields);

        loop {
            let state = parser.save_state();
            parser.skip_whitespace();

            if !parser.try_bump_char(',') {
                state.restore(parser);
                break;
            }

            parser.skip_whitespace();
            if !CSTTupleStructField::try_parse(parser) {
                break;
            }
        }

        parser.finish_node();
        true
    }
}

impl LowerableAstNode for CSTTupleStructFields {
    type Lowered = Vec<ParsedDataType>;

    fn lower(&self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let fields = self
            .tuple_struct_fields()
            .map(|field| field.lower(ctx))
            .collect_option_all()?;

        Some(fields)
    }
}
