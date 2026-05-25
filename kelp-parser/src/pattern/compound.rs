use kelp_core::{high::pattern::Pattern, path::generic::GenericPath, span::Span};
use la_arena::Idx;

use crate::{
    cst::{CSTCompoundPattern, CSTCompoundPatternEntry},
    lower_context::LowerContext,
    pattern::lower_pattern,
    span::{span_of_cst_node, text_range_to_span},
};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_compound_pattern_entry(
    node: CSTCompoundPatternEntry,
    ctx: &mut LowerContext,
) -> Option<((Span, String), Idx<Pattern>)> {
    let entry_name_token = node.name()?;
    let entry_name_span = text_range_to_span(entry_name_token.text_range());
    let entry_name = entry_name_token.text();

    let entry_pattern = node
        .pattern()
        .and_then(|pattern| lower_pattern(pattern, ctx))
        .unwrap_or_else(|| {
            ctx.allocator.allocate_pattern(
                entry_name_span,
                Pattern::Binding(GenericPath::single(entry_name_span, entry_name)),
            )
        });

    Some(((entry_name_span, entry_name.to_owned()), entry_pattern))
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_compound_pattern(
    node: CSTCompoundPattern,
    ctx: &mut LowerContext,
) -> Option<Idx<Pattern>> {
    let span = span_of_cst_node(&node);

    let entries = node
        .entries()
        .filter_map(|entry| lower_compound_pattern_entry(entry, ctx))
        .collect();

    Some(
        ctx.allocator
            .allocate_pattern(span, Pattern::Compound(entries)),
    )
}
