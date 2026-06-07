use kelp_core::{parsed::item::ParsedEnumVariant, trait_ext::CollectOptionAllIterExt};

use crate::{
    cst::{CSTEnumVariant, CSTEnumVariants},
    extension_traits::{LowerableAstNode, ParsableAstNode},
    lower_context::LowerContext,
    parser::Parser,
    syntax::SyntaxKind,
};

impl ParsableAstNode for CSTEnumVariants {
    fn try_parse(parser: &mut Parser) -> bool {
        let marker = parser.mark();

        if !CSTEnumVariant::try_parse(parser) {
            return false;
        }

        marker.start_node(parser, SyntaxKind::EnumVariants);

        loop {
            let state = parser.save_state();
            parser.skip_whitespace();

            if !parser.try_bump_char(',') {
                state.restore(parser);
                break;
            }

            parser.skip_whitespace();
            if !CSTEnumVariant::try_parse(parser) {
                break;
            }
        }

        parser.finish_node();
        true
    }
}

impl LowerableAstNode for CSTEnumVariants {
    type Lowered = Vec<ParsedEnumVariant>;

    fn lower(&self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let variants = self
            .enum_variants()
            .map(|variant| variant.lower(ctx))
            .collect_option_all()?;

        Some(variants)
    }
}
