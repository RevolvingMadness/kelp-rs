use minecraft_command_types::{has_macro::HasMacro, snbt::SNBTString};

use crate::{parsed::semantic_analysis::SemanticAnalysisContext, span::Span};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SpannedSNBTString {
    pub span: Span,
    pub snbt_string: SNBTString,
}

impl From<String> for SpannedSNBTString {
    fn from(value: String) -> Self {
        Self {
            span: Span::dummy(),
            snbt_string: SNBTString(false, value),
        }
    }
}

impl SpannedSNBTString {
    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> (Span, SNBTString) {
        if self.snbt_string.has_macro_conflict() {
            // TODO
            // ctx.add_error_unit(SemanticAnalysisError::MacroConflict);
        }

        (self.span, self.snbt_string)
    }
}
