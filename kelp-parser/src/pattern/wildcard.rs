use kelp_core::high::pattern::{Pattern, PatternKind};

use crate::{cst::CSTWildcardPattern, span::span_of_cst_node};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_wildcard_pattern(node: CSTWildcardPattern) -> Option<Pattern> {
    Some(PatternKind::Wildcard.with_span(span_of_cst_node(&node)))
}
