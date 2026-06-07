use kelp_core::parsed::item::ParsedEnumVariant;

use crate::{
    cst::CSTEnumVariant,
    extension_traits::{LowerableAstNode, ParsableAstNode, SyntaxTokenExt},
    lower_context::LowerContext,
    parser::Parser,
    syntax::SyntaxKind,
};

impl ParsableAstNode for CSTEnumVariant {
    fn try_parse(parser: &mut Parser) -> bool {
        parser.try_bump_identifier_kind(SyntaxKind::EnumVariant)
    }
}

impl LowerableAstNode for CSTEnumVariant {
    type Lowered = ParsedEnumVariant;

    fn lower(&self, _ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let name_token = self.name()?;
        let name_span = name_token.span();
        let name = name_token.text();

        Some(ParsedEnumVariant {
            name_span,
            name: name.to_owned(),
        })
    }
}
