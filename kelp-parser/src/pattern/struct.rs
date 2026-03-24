use std::collections::HashMap;

use kelp_core::{
    high::{
        pattern::{Pattern, PatternKind},
        semantic_analysis_context::SemanticAnalysisContext,
        snbt_string::SNBTString,
    },
    path::generic::GenericPath,
};
use minecraft_command_types::snbt::SNBTString as LowSNBTString;

use crate::{
    cst::{
        CSTStructStructPattern, CSTStructStructPatternField, CSTStructStructPatternFields,
        CSTTupleStructPattern, CSTTupleStructPatternField, CSTTupleStructPatternFields,
    },
    parser::Parser,
    path::generic::lower_generic_path,
    pattern::{lower_pattern, try_parse_pattern},
    span::{span_of_cst_node, text_range_to_span},
    syntax::SyntaxKind,
};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_struct_struct_pattern_field(
    node: CSTStructStructPatternField,
    ctx: &mut SemanticAnalysisContext,
) -> Option<(SNBTString, Pattern)> {
    let field_name_token = node.struct_field_name_token()?;
    let field_name_span = text_range_to_span(field_name_token.text_range());
    let field_name = field_name_token.text();

    let field_pattern = node
        .pattern()
        .and_then(|pattern| lower_pattern(pattern, ctx))
        .unwrap_or_else(|| Pattern {
            span: field_name_span,
            kind: PatternKind::Binding(GenericPath::single(field_name_span, field_name)),
        });

    Some((
        SNBTString {
            snbt_string: LowSNBTString(false, field_name.to_owned()),
            span: field_name_span,
        },
        field_pattern,
    ))
}

#[must_use]
fn try_parse_struct_struct_pattern_field(parser: &mut Parser) -> bool {
    let checkpoint = parser.checkpoint();

    if !parser.try_bump_identifier_kind(SyntaxKind::StructFieldName)
        && !parser.try_bump_whole_value()
    {
        return false;
    }

    parser.start_node_at(checkpoint, SyntaxKind::StructStructPatternField);

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
fn lower_struct_struct_pattern_fields(
    node: CSTStructStructPatternFields,
    ctx: &mut SemanticAnalysisContext,
) -> HashMap<SNBTString, Pattern> {
    node.struct_struct_pattern_fields()
        .filter_map(|field| lower_struct_struct_pattern_field(field, ctx))
        .collect()
}

#[must_use]
pub fn try_parse_struct_struct_pattern_fields(parser: &mut Parser) -> bool {
    let checkpoint = parser.checkpoint();

    if !try_parse_struct_struct_pattern_field(parser) {
        return false;
    }

    parser.start_node_at(checkpoint, SyntaxKind::StructStructPatternFields);

    loop {
        let state = parser.save_state();
        parser.skip_whitespace();

        if !parser.try_bump_char(',') {
            parser.restore_state(state);
            break;
        }

        parser.skip_whitespace();

        if !try_parse_struct_struct_pattern_field(parser) {
            break;
        }
    }

    parser.finish_node();

    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_struct_struct_pattern(
    node: CSTStructStructPattern,
    ctx: &mut SemanticAnalysisContext,
) -> Option<Pattern> {
    let span = span_of_cst_node(&node);

    let path = lower_generic_path(node.generic_path()?)?;

    let fields = node
        .struct_struct_pattern_fields()
        .map(|fields| lower_struct_struct_pattern_fields(fields, ctx))
        .unwrap_or_default();

    Some(PatternKind::StructStruct(path, fields).with_span(span))
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_tuple_struct_pattern_field(
    node: CSTTupleStructPatternField,
    ctx: &mut SemanticAnalysisContext,
) -> Option<Pattern> {
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
    ctx: &mut SemanticAnalysisContext,
) -> Vec<Pattern> {
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
    ctx: &mut SemanticAnalysisContext,
) -> Option<Pattern> {
    let span = span_of_cst_node(&node);

    let path = lower_generic_path(node.generic_path()?)?;

    let fields = node
        .tuple_struct_pattern_fields()
        .map(|fields| lower_tuple_struct_pattern_fields(fields, ctx))
        .unwrap_or_default();

    Some(PatternKind::TupleStruct(path, fields).with_span(span))
}
