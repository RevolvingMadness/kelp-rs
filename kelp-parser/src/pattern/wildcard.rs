use kelp_core::parsed::pattern::{ParsedPattern, ParsedPatternKind};

use crate::{
    cst::CSTWildcardPattern,
    extension_traits::{AstNodeExt as _, LowerableAstNode},
    lower_context::LowerContext,
};

impl LowerableAstNode for CSTWildcardPattern {
    type Lowered = ParsedPattern;

    fn lower(&self, _ctx: &mut LowerContext) -> Option<Self::Lowered> {
        Some(ParsedPatternKind::Wildcard.with_span(self.span()))
    }
}
