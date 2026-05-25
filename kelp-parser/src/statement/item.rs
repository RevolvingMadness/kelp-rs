use kelp_core::high::statement::{Statement, StatementId};

use crate::{
    cst::CSTItemStatement,
    item::{lower_item, try_parse_item},
    lower_context::LowerContext,
    parser::Parser,
    span::span_of_cst_node,
    syntax::SyntaxKind,
};

#[must_use]
pub fn try_parse_item_statement(parser: &mut Parser) -> bool {
    let checkpoint = parser.checkpoint();

    if !try_parse_item(parser) {
        return false;
    }

    parser.start_node_at(checkpoint, SyntaxKind::ItemStatement);
    parser.finish_node();

    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_item_statement(node: CSTItemStatement, ctx: &mut LowerContext) -> Option<StatementId> {
    let span = span_of_cst_node(&node);

    let item = lower_item(node.item()?, ctx)?;

    Some(
        ctx.allocator
            .allocate_statement(span, Statement::Item(item)),
    )
}
