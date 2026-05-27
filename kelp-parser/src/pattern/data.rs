use kelp_core::parsed::pattern::{ParsedPattern, ParsedPatternKind};

use crate::{
    cst::CSTDataPattern,
    data::{lower_data, try_parse_data},
    extension_traits::AstNodeExt as _,
    lower_context::LowerContext,
    parser::Parser,
    syntax::SyntaxKind,
};

#[must_use]
pub fn try_parse_data_pattern(parser: &mut Parser) -> bool {
    let checkpoint = parser.mark();

    if !try_parse_data(parser) {
        return false;
    }

    checkpoint.start_node(parser, SyntaxKind::DataPattern);

    parser.finish_node();

    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_data_pattern(node: CSTDataPattern, ctx: &mut LowerContext) -> Option<ParsedPattern> {
    let span = node.span();

    let data = lower_data(node.data()?, ctx)?;

    Some(ParsedPatternKind::Data(Box::new(data)).with_span(span))
}
