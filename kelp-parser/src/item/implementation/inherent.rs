use kelp_core::parsed::item::ParsedItemKind;

use crate::{
    cst::{CSTDataType, CSTInherentImplementationItem},
    data_type::generics::{lower_generic_names, try_parse_generic_names},
    extension_traits::{AstNodeExt, LowerableAstNode, ParsableAstNode},
    item::associated::{expect_associated_item, lower_associated_item},
    lower_context::LowerContext,
    parser::Parser,
    syntax::SyntaxKind,
};

impl ParsableAstNode for CSTInherentImplementationItem {
    fn try_parse(parser: &mut Parser) -> bool {
        let state = parser.save_state();

        parser.start_node(SyntaxKind::InherentImplementationItem);

        if !parser.try_bump_str("impl", SyntaxKind::ImplKeyword) {
            state.restore(parser);

            return false;
        }

        parser.skip_whitespace();

        try_parse_generic_names(parser);

        parser.skip_whitespace();

        if !CSTDataType::try_parse(parser) {
            state.restore(parser);
            return false;
        }

        parser.skip_whitespace();

        if !parser.expect_char('{', "Expected '{'") {
            state.restore(parser);
            return false;
        }

        loop {
            parser.skip_whitespace();

            if parser.is_eof() || parser.peek_char() == Some('}') {
                break;
            }

            expect_associated_item(parser);
        }

        parser.expect_char('}', "Expected '}'");
        parser.finish_node();

        true
    }
}

impl LowerableAstNode for CSTInherentImplementationItem {
    type Lowered = ParsedItemKind;

    fn lower(self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let generic_names = self.generic_names().and_then(lower_generic_names);
        let target_type = self.data_type()?;
        let target_type_span = target_type.span();
        let target_type = target_type.lower(ctx)?;

        let associated_items = self
            .associated_items()
            .filter_map(|item| lower_associated_item(item, ctx))
            .collect();

        Some(ParsedItemKind::InherentImplementationItem {
            generic_names: generic_names.unwrap_or_default(),
            target_type_span,
            target_type,
            associated_items,
        })
    }
}
