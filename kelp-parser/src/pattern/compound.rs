use kelp_core::{
    parsed::pattern::{ParsedPattern, ParsedPatternKind},
    path::generic::GenericPath,
    span::Span,
};

use crate::{
    cst::{CSTCompoundPattern, CSTCompoundPatternEntry},
    extension_traits::{AstNodeExt, SyntaxTokenExt},
    lower_context::LowerContext,
    pattern::lower_pattern,
};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_compound_pattern_entry(
    node: CSTCompoundPatternEntry,
    ctx: &mut LowerContext,
) -> Option<((Span, String), ParsedPattern)> {
    let entry_name_token = node.name()?;
    let entry_name_span = entry_name_token.span();
    let entry_name = entry_name_token.text();

    let entry_pattern = node
        .pattern()
        .and_then(|pattern| lower_pattern(pattern, ctx))
        .unwrap_or_else(|| ParsedPattern {
            span: entry_name_span,
            kind: ParsedPatternKind::Binding(GenericPath::single(entry_name_span, entry_name)),
        });

    Some(((entry_name_span, entry_name.to_owned()), entry_pattern))
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_compound_pattern(
    node: CSTCompoundPattern,
    ctx: &mut LowerContext,
) -> Option<ParsedPattern> {
    let span = node.span();

    let entries = node
        .entries()
        .filter_map(|entry| lower_compound_pattern_entry(entry, ctx))
        .collect();

    Some(ParsedPatternKind::Compound(entries).with_span(span))
}
