use kelp_core::parsed::pattern::{ParsedPattern, ParsedPatternKind};

use crate::{cst::CSTWildcardPattern, span::span_of_cst_node};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_wildcard_pattern(node: CSTWildcardPattern) -> Option<ParsedPattern> {
    Some(ParsedPatternKind::Wildcard.with_span(span_of_cst_node(&node)))
}
