use kelp_core::parsed::pattern::Pattern;
use la_arena::Idx;

use crate::{cst::CSTWildcardPattern, lower_context::LowerContext, span::span_of_cst_node};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_wildcard_pattern(
    node: CSTWildcardPattern,
    ctx: &mut LowerContext,
) -> Option<Idx<Pattern>> {
    let span = span_of_cst_node(&node);

    Some(ctx.arena.allocate_pattern(span, Pattern::Wildcard))
}
