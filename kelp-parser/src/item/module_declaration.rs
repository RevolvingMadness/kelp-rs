use kelp_core::high::{item::ItemKind, semantic_analysis::SemanticAnalysisContext};

use crate::{
    cst::CSTModuleDeclarationItem,
    item::{expect_item, lower_item},
    parser::Parser,
    span::text_range_to_span,
    syntax::SyntaxKind,
};

#[must_use]
pub fn try_parse_module_declaration_item_kind(parser: &mut Parser) -> bool {
    let state = parser.save_state();

    parser.start_node(SyntaxKind::ModuleDeclarationItem);
    parser.bump_str(SyntaxKind::ModKeyword, "mod");

    parser.expect_whitespace();

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

        expect_item(parser);
    }

    parser.expect_char('}', "Expected '}'");

    parser.finish_node();

    true
}

#[must_use]
pub fn expect_module_declaration_item_kind(parser: &mut Parser) -> bool {
    parser.start_node(SyntaxKind::ModuleDeclarationItem);
    parser.bump_str(SyntaxKind::ModKeyword, "mod");

    parser.expect_whitespace();

    parser.expect_identifier_kind(SyntaxKind::ModuleName, "Expected module name");

    parser.skip_whitespace();

    parser.expect_char('{', "Expected '{'");

    loop {
        parser.skip_whitespace();

        if parser.is_eof() || parser.peek_char() == Some('}') {
            break;
        }

        expect_item(parser);
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
) -> Option<ItemKind> {
    let name_token = node.module_name_token()?;
    let name_span = text_range_to_span(name_token.text_range());
    let name = name_token.text();

    let items = node
        .items()
        .filter_map(|item| lower_item(item, ctx))
        .collect();

    Some(ItemKind::ModuleDeclaration(
        name_span,
        name.to_owned(),
        items,
    ))
}
