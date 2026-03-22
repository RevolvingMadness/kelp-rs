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
    cst::{CSTCompoundPattern, CSTCompoundPatternEntry},
    pattern::lower_pattern,
    span::{span_of_cst_node, text_range_to_span},
};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_compound_pattern_entry(
    node: CSTCompoundPatternEntry,
    ctx: &mut SemanticAnalysisContext,
) -> Option<(SNBTString, Pattern)> {
    let entry_name_token = node.name()?;
    let entry_name_span = text_range_to_span(entry_name_token.text_range());
    let entry_name = entry_name_token.text();

    let entry_pattern = node
        .pattern()
        .and_then(|pattern| lower_pattern(pattern, ctx))
        .unwrap_or_else(|| Pattern {
            span: entry_name_span,
            kind: PatternKind::Binding(GenericPath::single(entry_name_span, entry_name)),
        });

    Some((
        SNBTString {
            snbt_string: LowSNBTString(false, entry_name.to_owned()),
            span: entry_name_span,
        },
        entry_pattern,
    ))
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_compound_pattern(
    node: CSTCompoundPattern,
    ctx: &mut SemanticAnalysisContext,
) -> Option<Pattern> {
    let span = span_of_cst_node(&node);

    let entries = node
        .entries()
        .filter_map(|entry| lower_compound_pattern_entry(entry, ctx))
        .collect();

    Some(PatternKind::Compound(entries).with_span(span))
}
