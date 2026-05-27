use kelp_core::parsed::pattern::{ParsedPattern, ParsedPatternKind};

use crate::{
    cst::CSTTuplePattern,
    extension_traits::{AstNodeExt, LowerableAstNode},
    lower_context::LowerContext,
};

impl LowerableAstNode for CSTTuplePattern {
    type Lowered = ParsedPattern;

    fn lower(&self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let patterns = self
            .patterns()
            .filter_map(|pattern| pattern.lower(ctx))
            .collect();

        Some(ParsedPatternKind::Tuple(patterns).with_span(self.span()))
    }
}
