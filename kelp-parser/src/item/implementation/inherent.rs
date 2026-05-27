use kelp_core::parsed::item::ParsedItemKind;

use crate::{
    cst::{CSTAssociatedItem, CSTDataType, CSTGenericNames, CSTInherentImplementationItem},
    extension_traits::{AstNodeExt, LowerableAstNode, ParsableAstNode},
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

        CSTGenericNames::try_parse(parser);

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

            CSTAssociatedItem::expect(parser, "Expected associated item");
        }

        parser.expect_char('}', "Expected '}'");
        parser.finish_node();

        true
    }
}

impl LowerableAstNode for CSTInherentImplementationItem {
    type Lowered = ParsedItemKind;

    fn lower(&self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let generic_names = self.generic_names().and_then(|names| names.lower(ctx));
        let target_type = self.data_type()?;
        let target_type_span = target_type.span();
        let target_type = target_type.lower(ctx)?;

        let associated_items = self
            .associated_items()
            .filter_map(|item| item.lower(ctx))
            .collect();

        Some(ParsedItemKind::InherentImplementationItem {
            generic_names: generic_names.unwrap_or_default(),
            target_type_span,
            target_type,
            associated_items,
        })
    }
}
