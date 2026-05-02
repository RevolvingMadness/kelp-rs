use kelp_core::high::{
    data_type::unresolved::UnresolvedDataType, item::ItemKind,
    semantic_analysis::SemanticAnalysisContext,
};

use crate::{
    cst::CSTFunctionDeclarationItem,
    data_type::{lower_data_type, try_parse_data_type},
    expression::with_block::block::{lower_block_expression, try_parse_block_expression},
    parser::Parser,
    span::text_range_to_span,
    syntax::SyntaxKind,
};

#[must_use]
pub fn try_parse_function_declaration_item_kind(parser: &mut Parser) -> bool {
    let state = parser.save_state();

    parser.start_node(SyntaxKind::FunctionDeclarationItem);
    parser.bump_str(SyntaxKind::FNKeyword, "fn");

    if !parser.expect_whitespace() || !parser.try_bump_identifier() {
        parser.restore_state(state);

        return false;
    }

    parser.skip_whitespace();

    parser.expect_char('(', "Expected '('");

    parser.skip_whitespace();

    parser.expect_char(')', "Expected ')'");

    parser.skip_whitespace();

    if parser.try_bump_str("->", SyntaxKind::MinusRightArrow) {
        parser.skip_whitespace();

        if !try_parse_data_type(parser) {
            parser.error("Expected function return type");
        }

        parser.skip_whitespace();
    }

    if !try_parse_block_expression(parser) {
        parser.error("Expected block expression");
    }

    parser.finish_node();

    true
}

pub fn expect_function_declaration_item_kind(parser: &mut Parser) {
    parser.start_node(SyntaxKind::FunctionDeclarationItem);
    parser.bump_str(SyntaxKind::FNKeyword, "fn");

    parser.expect_whitespace();

    if !parser.try_bump_identifier() {
        parser.error("Expected function name");
    }

    parser.skip_whitespace();

    parser.expect_char('(', "Expected '('");

    parser.skip_whitespace();

    parser.expect_char(')', "Expected ')'");

    parser.skip_whitespace();

    if parser.try_bump_str("->", SyntaxKind::MinusRightArrow) {
        parser.skip_whitespace();

        if !try_parse_data_type(parser) {
            parser.error("Expected function return type");
        }

        parser.skip_whitespace();
    }

    if !try_parse_block_expression(parser) {
        parser.error("Expected block expression");
    }

    parser.finish_node();
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_function_declaration_item_kind(
    node: CSTFunctionDeclarationItem,
    ctx: &mut SemanticAnalysisContext,
) -> Option<ItemKind> {
    let name_token = node.name()?;
    let name_span = name_token.text_range();
    let name = name_token.text();

    let return_type = node
        .data_type()
        .and_then(lower_data_type)
        .unwrap_or(UnresolvedDataType::Unit);

    let body = lower_block_expression(node.block_expression()?, ctx)?;

    Some(ItemKind::FunctionDeclaration {
        name_span: text_range_to_span(name_span),
        name: name.to_owned(),
        generic_names: Vec::new(),
        parameter_types: Vec::new(),
        return_type,
        body,
    })
}
