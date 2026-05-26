use std::collections::HashMap;

use kelp_core::{parsed::pattern::Pattern, path::generic::GenericPath, span::Span};
use la_arena::Idx;

use crate::{
    cst::{
        CSTRegularStructPattern, CSTRegularStructPatternField, CSTRegularStructPatternFields,
        CSTTupleStructPattern, CSTTupleStructPatternField, CSTTupleStructPatternFields,
    },
    lower_context::LowerContext,
    parser::Parser,
    path::generic::lower_generic_path,
    pattern::{lower_pattern, try_parse_pattern},
    span::{span_of_cst_node, text_range_to_span},
    syntax::SyntaxKind,
};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_struct_pattern_field(
    node: CSTRegularStructPatternField,
    ctx: &mut LowerContext,
) -> Option<((Span, String), Idx<Pattern>)> {
    let field_name_token = node.struct_field_name_token()?;
    let field_name_span = text_range_to_span(field_name_token.text_range());
    let field_name = field_name_token.text();

    let field_pattern = node
        .pattern()
        .and_then(|pattern| lower_pattern(pattern, ctx))
        .unwrap_or_else(|| {
            ctx.allocator.allocate_pattern(
                field_name_span,
                Pattern::Binding(GenericPath::single(field_name_span, field_name)),
            )
        });

    Some(((field_name_span, field_name.to_owned()), field_pattern))
}

#[must_use]
fn try_parse_struct_pattern_field(parser: &mut Parser) -> bool {
    let checkpoint = parser.checkpoint();

    if !parser.try_bump_identifier_kind(SyntaxKind::StructFieldName)
        && !parser.try_bump_whole_value()
    {
        return false;
    }

    parser.start_node_at(checkpoint, SyntaxKind::RegularStructPatternField);

    let state = parser.save_state();

    parser.skip_whitespace();

    if parser.try_bump_char(':') {
        parser.skip_whitespace();

        if !try_parse_pattern(parser) {
            parser.error("Expected pattern");
        }
    } else {
        parser.restore_state(state);
    }

    parser.finish_node();

    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
fn lower_struct_pattern_fields(
    node: CSTRegularStructPatternFields,
    ctx: &mut LowerContext,
) -> HashMap<(Span, String), Idx<Pattern>> {
    node.regular_struct_pattern_fields()
        .filter_map(|field| lower_struct_pattern_field(field, ctx))
        .collect()
}

#[must_use]
pub fn try_parse_struct_pattern_fields(parser: &mut Parser) -> bool {
    let checkpoint = parser.checkpoint();

    if !try_parse_struct_pattern_field(parser) {
        return false;
    }

    parser.start_node_at(checkpoint, SyntaxKind::RegularStructPatternFields);

    loop {
        let state = parser.save_state();
        parser.skip_whitespace();

        if !parser.try_bump_char(',') {
            parser.restore_state(state);
            break;
        }

        parser.skip_whitespace();

        if !try_parse_struct_pattern_field(parser) {
            break;
        }
    }

    parser.finish_node();

    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_struct_pattern(
    node: CSTRegularStructPattern,
    ctx: &mut LowerContext,
) -> Option<Idx<Pattern>> {
    let span = span_of_cst_node(&node);

    let path = lower_generic_path(node.generic_path()?)?;

    let fields = node
        .regular_struct_pattern_fields()
        .map(|fields| lower_struct_pattern_fields(fields, ctx))
        .unwrap_or_default();

    Some(
        ctx.allocator
            .allocate_pattern(span, Pattern::RegularStruct(path, fields)),
    )
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_tuple_struct_pattern_field(
    node: CSTTupleStructPatternField,
    ctx: &mut LowerContext,
) -> Option<Idx<Pattern>> {
    let field_pattern = lower_pattern(node.pattern()?, ctx)?;

    Some(field_pattern)
}

#[must_use]
fn try_parse_tuple_struct_pattern_field(parser: &mut Parser) -> bool {
    let checkpoint = parser.checkpoint();

    if !try_parse_pattern(parser) {
        return false;
    }

    parser.start_node_at(checkpoint, SyntaxKind::TupleStructPatternField);

    parser.finish_node();

    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
fn lower_tuple_struct_pattern_fields(
    node: CSTTupleStructPatternFields,
    ctx: &mut LowerContext,
) -> Vec<Idx<Pattern>> {
    node.tuple_struct_pattern_fields()
        .filter_map(|field| lower_tuple_struct_pattern_field(field, ctx))
        .collect()
}

#[must_use]
pub fn try_parse_tuple_struct_pattern_fields(parser: &mut Parser) -> bool {
    let checkpoint = parser.checkpoint();

    if !try_parse_tuple_struct_pattern_field(parser) {
        return false;
    }

    parser.start_node_at(checkpoint, SyntaxKind::TupleStructPatternFields);

    loop {
        let state = parser.save_state();
        parser.skip_whitespace();

        if !parser.try_bump_char(',') {
            parser.restore_state(state);
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
) -> Option<Idx<Pattern>> {
    let span = span_of_cst_node(&node);

    let path = lower_generic_path(node.generic_path()?)?;

    let fields = node
        .tuple_struct_pattern_fields()
        .map(|fields| lower_tuple_struct_pattern_fields(fields, ctx))
        .unwrap_or_default();

    Some(
        ctx.allocator
            .allocate_pattern(span, Pattern::TupleStruct(path, fields)),
    )
}
