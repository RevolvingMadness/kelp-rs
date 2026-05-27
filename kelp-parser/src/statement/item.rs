use kelp_core::parsed::statement::{ParsedStatement, ParsedStatementKind};

use crate::{
    cst::CSTItemStatement,
    extension_traits::AstNodeExt,
    item::{lower_item, try_parse_item},
    lower_context::LowerContext,
    parser::Parser,
    syntax::SyntaxKind,
};

#[must_use]
pub fn try_parse_item_statement(parser: &mut Parser) -> bool {
    let checkpoint = parser.mark();

    if !try_parse_item(parser) {
        return false;
    }

    checkpoint.start_node(parser, SyntaxKind::ItemStatement);
    parser.finish_node();

    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_item_statement(
    node: CSTItemStatement,
    ctx: &mut LowerContext,
) -> Option<ParsedStatement> {
    let span = node.span();

    let item = lower_item(node.item()?, ctx)?;

    Some(ParsedStatementKind::Item(Box::new(item)).with_span(span))
}
