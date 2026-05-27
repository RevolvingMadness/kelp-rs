use kelp_core::parsed::pattern::{ParsedPattern, ParsedPatternKind};

use crate::{
    cst::CSTBindingPattern,
    extension_traits::{AstNodeExt, LowerableAstNode},
    lower_context::LowerContext,
    path::generic::lower_generic_path,
};

impl LowerableAstNode for CSTBindingPattern {
    type Lowered = ParsedPattern;

    fn lower(self, _ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let path = lower_generic_path(self.generic_path()?)?;

        Some(ParsedPatternKind::Binding(path).with_span(self.span()))
    }
}
