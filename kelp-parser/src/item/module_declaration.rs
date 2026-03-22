use kelp_core::high::{item::Item, semantic_analysis_context::SemanticAnalysisContext};

use crate::{
    cst::CSTModuleDeclarationItem,
    item::{lower_item, try_parse_item},
    parser::Parser,
    span::text_range_to_span,
    syntax::SyntaxKind,
};

#[must_use]
pub fn try_parse_module_declaration_item(parser: &mut Parser) -> bool {
    let state = parser.save_state();

    parser.start_node(SyntaxKind::ModuleDeclarationItem);
    parser.bump_str(SyntaxKind::ModKeyword, "mod");
    parser.expect_inline_whitespace();

    if !parser.try_bump_identifier_kind(SyntaxKind::ModuleName) {
        parser.restore_state(state);

        return false;
    }

    parser.skip_whitespace();

    parser.expect_char('{', "Expected '{'");

    loop {
        parser.skip_whitespace();

        if parser.is_eof() || parser.peek_char() == Some('}') {
            break;
        }

        if !try_parse_item(parser) {
            parser.error("Expected item");
        }

        if !parser.try_parse_newline_whitespace() {
            parser.recover_newline("Expected newline to mark end of item");
        }
    }

    parser.expect_char('}', "Expected '}'");

    parser.finish_node();

    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_module_declaration_item(
    node: CSTModuleDeclarationItem,
    ctx: &mut SemanticAnalysisContext,
) -> Option<Item> {
    let name_token = node.module_name_token()?;
    let name_span = text_range_to_span(name_token.text_range());
    let name = name_token.text();

    let items = node
        .items()
        .filter_map(|item| lower_item(item, ctx))
        .collect();

    Some(Item::ModuleDeclaration(name_span, name.to_owned(), items))
}
