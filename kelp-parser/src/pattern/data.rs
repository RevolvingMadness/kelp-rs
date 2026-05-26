use kelp_core::parsed::pattern::Pattern;
use la_arena::Idx;

use crate::{
    cst::CSTDataPattern,
    data::{lower_data, try_parse_data},
    lower_context::LowerContext,
    parser::Parser,
    span::span_of_cst_node,
    syntax::SyntaxKind,
};

#[must_use]
pub fn try_parse_data_pattern(parser: &mut Parser) -> bool {
    let checkpoint = parser.checkpoint();

    if !try_parse_data(parser) {
        return false;
    }

    parser.start_node_at(checkpoint, SyntaxKind::DataPattern);

    parser.finish_node();

    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_data_pattern(node: CSTDataPattern, ctx: &mut LowerContext) -> Option<Idx<Pattern>> {
    let span = span_of_cst_node(&node);

    let data = lower_data(node.data()?, ctx)?;

    Some(
        ctx.allocator
            .allocate_pattern(span, Pattern::Data(Box::new(data))),
    )
}
