use kelp_core::high::{
    semantic_analysis_context::SemanticAnalysisContext,
    statement::{Statement, StatementKind},
};

use crate::{
    cst::CSTItemStatement,
    item::{
        lower_item, mcfn_declaration::try_parse_mcfn_declaration_item,
        struct_declaration::try_parse_struct_declaration_item,
        type_alias_declaration::try_parse_type_alias_declaration_item,
    },
    parser::Parser,
    span::span_of_cst_node,
    syntax::SyntaxKind,
};

#[must_use]
pub fn try_parse_item(parser: &mut Parser) -> bool {
    let Some(identifier) = parser.peek_identifier() else {
        return false;
    };

    match identifier {
        "mcfn" => try_parse_mcfn_declaration_item(parser),
        "struct" => try_parse_struct_declaration_item(parser),
        "type" => try_parse_type_alias_declaration_item(parser),
        _ => false,
    }
}

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
pub fn lower_item_statement(
    node: CSTItemStatement,
    ctx: &mut SemanticAnalysisContext,
) -> Option<Statement> {
    let span = span_of_cst_node(&node);

    let item = lower_item(node.item()?, ctx)?;

    Some(StatementKind::Item(Box::new(item)).with_span(span))
}
