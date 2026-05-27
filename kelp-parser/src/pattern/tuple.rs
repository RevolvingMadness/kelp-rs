use kelp_core::parsed::pattern::{ParsedPattern, ParsedPatternKind};

use crate::{
    cst::CSTTuplePattern, extension_traits::AstNodeExt, lower_context::LowerContext,
    pattern::lower_pattern,
};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_tuple_pattern(node: CSTTuplePattern, ctx: &mut LowerContext) -> Option<ParsedPattern> {
    let span = node.span();

    let patterns = node
        .patterns()
        .filter_map(|pattern| lower_pattern(pattern, ctx))
        .collect();

    Some(ParsedPatternKind::Tuple(patterns).with_span(span))
}
