use kelp_core::parsed::pattern::{ParsedPattern, ParsedPatternKind};

use crate::{
    cst::CSTBindingPattern,
    extension_traits::{AstNodeExt, LowerableAstNode},
    lower_context::LowerContext,
};

impl LowerableAstNode for CSTBindingPattern {
    type Lowered = ParsedPattern;

    fn lower(self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let path = self.generic_path()?.lower(ctx)?;

        Some(ParsedPatternKind::Binding(path).with_span(self.span()))
    }
}
