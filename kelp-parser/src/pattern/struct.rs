use std::collections::HashMap;

use kelp_core::{
    parsed::pattern::{ParsedPattern, ParsedPatternKind},
    path::generic::GenericPath,
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
    path::generic::lower_generic_path,
    pattern::lower_pattern,
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

    fn lower(self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let field_name_token = self.struct_field_name_token()?;
        let field_name_span = field_name_token.span();
        let field_name = field_name_token.text();

        let field_pattern = self
            .pattern()
            .and_then(|pattern| lower_pattern(pattern, ctx))
            .unwrap_or_else(|| ParsedPattern {
                span: field_name_span,
                kind: ParsedPatternKind::Binding(GenericPath::single(field_name_span, field_name)),
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

    fn lower(self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        Some(
            self.regular_struct_pattern_fields()
                .filter_map(|field| field.lower(ctx))
                .collect(),
        )
    }
}

impl LowerableAstNode for CSTRegularStructPattern {
    type Lowered = ParsedPattern;

    fn lower(self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let path = lower_generic_path(self.generic_path()?)?;

        let fields = self
            .regular_struct_pattern_fields()
            .and_then(|fields| fields.lower(ctx));

        Some(
            ParsedPatternKind::RegularStruct(path, fields.unwrap_or_default())
                .with_span(self.span()),
        )
    }
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_tuple_struct_pattern_field(
    node: CSTTupleStructPatternField,
    ctx: &mut LowerContext,
) -> Option<ParsedPattern> {
    let field_pattern = lower_pattern(node.pattern()?, ctx)?;

    Some(field_pattern)
}

#[must_use]
fn try_parse_tuple_struct_pattern_field(parser: &mut Parser) -> bool {
    let checkpoint = parser.mark();

    if !CSTPattern::try_parse(parser) {
        return false;
    }

    checkpoint.finish(parser, SyntaxKind::TupleStructPatternField);

    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
fn lower_tuple_struct_pattern_fields(
    node: CSTTupleStructPatternFields,
    ctx: &mut LowerContext,
) -> Vec<ParsedPattern> {
    node.tuple_struct_pattern_fields()
        .filter_map(|field| lower_tuple_struct_pattern_field(field, ctx))
        .collect()
}

#[must_use]
pub fn try_parse_tuple_struct_pattern_fields(parser: &mut Parser) -> bool {
    let checkpoint = parser.mark();

    if !try_parse_tuple_struct_pattern_field(parser) {
        return false;
    }

    checkpoint.start_node(parser, SyntaxKind::TupleStructPatternFields);

    loop {
        let state = parser.save_state();
        parser.skip_whitespace();

        if !parser.try_bump_char(',') {
            state.restore(parser);
            break;
        }

        parser.skip_whitespace();

        if !try_parse_tuple_struct_pattern_field(parser) {
            break;
        }
    }

    parser.finish_node();

    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_tuple_struct_pattern(
    node: CSTTupleStructPattern,
    ctx: &mut LowerContext,
) -> Option<ParsedPattern> {
    let span = node.span();

    let path = lower_generic_path(node.generic_path()?)?;

    let fields = node
        .tuple_struct_pattern_fields()
        .map(|fields| lower_tuple_struct_pattern_fields(fields, ctx))
        .unwrap_or_default();

    Some(ParsedPatternKind::TupleStruct(path, fields).with_span(span))
}
