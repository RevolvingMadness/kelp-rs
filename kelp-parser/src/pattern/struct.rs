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
    cst::{CSTStructPattern, CSTStructPatternField},
    path::generic::lower_generic_path,
    pattern::lower_pattern,
    span::{span_of_cst_node, text_range_to_span},
};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_struct_pattern_field(
    node: CSTStructPatternField,
    ctx: &mut SemanticAnalysisContext,
) -> Option<(SNBTString, Pattern)> {
    let field_name_token = node.name()?;
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
#[allow(clippy::needless_pass_by_value)]
pub fn lower_struct_pattern(
    node: CSTStructPattern,
    ctx: &mut SemanticAnalysisContext,
) -> Option<Pattern> {
    let span = span_of_cst_node(&node);

    let path = lower_generic_path(node.generic_path()?)?;

    let fields = node
        .fields()
        .filter_map(|field| lower_struct_pattern_field(field, ctx))
        .collect();

    Some(PatternKind::Struct(path, fields).with_span(span))
}
