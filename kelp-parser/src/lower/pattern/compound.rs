use kelp_core::{
    high::snbt_string::HighSNBTString,
    pattern::{Pattern, PatternKind},
};
use minecraft_command_types::snbt::SNBTString;

use crate::{
    cst::{CSTCompoundPattern, CSTCompoundPatternEntry},
    lower::pattern::lower_pattern,
    span::{span_of_cst_node, text_range_to_span},
};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_compound_pattern_entry(
    node: CSTCompoundPatternEntry,
) -> Option<(HighSNBTString, Option<Pattern>)> {
    let entry_name_token = node.name()?;
    let entry_name_span = text_range_to_span(entry_name_token.text_range());
    let entry_name = entry_name_token.text();

    let entry_pattern = node.pattern().and_then(lower_pattern);

    Some((
        HighSNBTString {
            snbt_string: SNBTString(false, entry_name.to_owned()),
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
