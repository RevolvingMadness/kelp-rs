use kelp_core::high::{
    pattern::{Pattern, PatternKind},
    semantic_analysis::SemanticAnalysisContext,
};

use crate::{cst::CSTTuplePattern, pattern::lower_pattern, span::span_of_cst_node};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_tuple_pattern(
    node: CSTTuplePattern,
    ctx: &mut SemanticAnalysisContext,
) -> Option<Pattern> {
    let span = span_of_cst_node(&node);

    let patterns = node
        .patterns()
        .filter_map(|pattern| lower_pattern(pattern, ctx))
        .collect();

    Some(PatternKind::Tuple(patterns).with_span(span))
}
