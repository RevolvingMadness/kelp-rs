use kelp_core::{
    high::pattern::{Pattern, PatternKind},
    high::snbt_string::SNBTString,
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
) -> Option<(SNBTString, Pattern)> {
    let entry_name_token = node.name()?;
    let entry_name_span = text_range_to_span(entry_name_token.text_range());
    let entry_name = entry_name_token.text();

    let entry_pattern = node
        .pattern()
        .and_then(lower_pattern)
        .unwrap_or_else(|| Pattern {
            span: entry_name_span,
            kind: PatternKind::Binding(entry_name.to_owned()),
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
pub fn lower_compound_pattern(node: CSTCompoundPattern) -> Option<Pattern> {
    let span = span_of_cst_node(&node);

    let entries = node
        .entries()
        .filter_map(lower_compound_pattern_entry)
        .collect();

    Some(PatternKind::Compound(entries).with_span(span))
}
