use std::collections::HashMap;

use kelp_core::{
    parsed::pattern::{ParsedPattern, ParsedPatternKind},
    parsed::typed_path::ParsedTypedPath,
    span::Span,
};

use crate::{
    cst::{
        CSTPattern, CSTRegularStructPattern, CSTRegularStructPatternField,
        CSTRegularStructPatternFields, CSTTupleStructPattern, CSTTupleStructPatternField,
        CSTTupleStructPatternFields,
    },
    extension_traits::{AstNodeExt, SyntaxTokenExt},
    extension_traits::{LowerableAstNode, ParsableAstNode},
    lower_context::LowerContext,
    parser::Parser,
    syntax::SyntaxKind,
};

impl ParsableAstNode for CSTRegularStructPatternField {
    fn try_parse(parser: &mut Parser) -> bool {
        let marker = parser.mark();

        if !parser.try_bump_identifier_kind(SyntaxKind::StructFieldName)
            && !parser.try_bump_whole_value()
        {
            return false;
        }

        parser.attempt(|parser| {
            parser.skip_whitespace();

            if parser.try_bump_char(':') {
                parser.skip_whitespace();

                CSTPattern::expect(parser, "Expected pattern");

                true
            } else {
                false
            }
        });

        marker.finish(parser, SyntaxKind::RegularStructPatternField);

        true
    }
}

impl LowerableAstNode for CSTRegularStructPatternField {
    type Lowered = ((Span, String), ParsedPattern);

    fn lower(&self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let field_name_token = self.struct_field_name_token()?;
        let field_name_span = field_name_token.span();
        let field_name = field_name_token.text();

        let field_pattern = self
            .pattern()
            .and_then(|pattern| pattern.lower(ctx))
            .unwrap_or_else(|| ParsedPattern {
                span: field_name_span,
                kind: ParsedPatternKind::Binding(ParsedTypedPath::single(field_name_span, field_name)),
            });

        Some(((field_name_span, field_name.to_owned()), field_pattern))
    }
}

impl ParsableAstNode for CSTRegularStructPatternFields {
    fn try_parse(parser: &mut Parser) -> bool {
        let marker = parser.mark();

        if parser.parse_comma_separated_list(CSTRegularStructPatternField::try_parse) {
            marker.finish(parser, SyntaxKind::RegularStructPatternFields);

            true
        } else {
            false
        }
    }
}

impl LowerableAstNode for CSTRegularStructPatternFields {
    type Lowered = HashMap<(Span, String), ParsedPattern>;

    fn lower(&self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        Some(
            self.regular_struct_pattern_fields()
                .filter_map(|field| field.lower(ctx))
                .collect(),
        )
    }
}

impl LowerableAstNode for CSTRegularStructPattern {
    type Lowered = ParsedPattern;

    fn lower(&self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let path = self.generic_path()?.lower(ctx)?;

        let fields = self
            .regular_struct_pattern_fields()
            .and_then(|fields| fields.lower(ctx));

        Some(
            ParsedPatternKind::RegularStruct(path, fields.unwrap_or_default())
                .with_span(self.span()),
        )
    }
}

impl ParsableAstNode for CSTTupleStructPatternField {
    fn try_parse(parser: &mut Parser) -> bool {
        let marker = parser.mark();

        if !CSTPattern::try_parse(parser) {
            return false;
        }

        marker.finish(parser, SyntaxKind::TupleStructPatternField);

        true
    }
}

impl LowerableAstNode for CSTTupleStructPatternField {
    type Lowered = ParsedPattern;

    fn lower(&self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let field_pattern = self.pattern()?.lower(ctx)?;

        Some(field_pattern)
    }
}

impl ParsableAstNode for CSTTupleStructPatternFields {
    fn try_parse(parser: &mut Parser) -> bool {
        let marker = parser.mark();

        if !CSTTupleStructPatternField::try_parse(parser) {
            return false;
        }

        marker.start_node(parser, SyntaxKind::TupleStructPatternFields);

        loop {
            let state = parser.save_state();
            parser.skip_whitespace();

            if !parser.try_bump_char(',') {
                state.restore(parser);
                break;
            }

            parser.skip_whitespace();

            if !CSTTupleStructPatternField::try_parse(parser) {
                break;
            }
        }

        parser.finish_node();

        true
    }
}

impl LowerableAstNode for CSTTupleStructPatternFields {
    type Lowered = Vec<ParsedPattern>;

    fn lower(&self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        Some(
            self.tuple_struct_pattern_fields()
                .filter_map(|field| field.lower(ctx))
                .collect(),
        )
    }
}

impl LowerableAstNode for CSTTupleStructPattern {
    type Lowered = ParsedPattern;

    fn lower(&self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let path = self.generic_path()?.lower(ctx)?;

        let fields = self
            .tuple_struct_pattern_fields()
            .and_then(|fields| fields.lower(ctx))
            .unwrap_or_default();

        Some(ParsedPatternKind::TupleStruct(path, fields).with_span(self.span()))
    }
}
