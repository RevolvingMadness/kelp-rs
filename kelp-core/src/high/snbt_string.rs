use std::hash::Hash;

use minecraft_command_types::{has_macro::HasMacro, snbt::SNBTString as LowSNBTString};

use crate::{
    high::semantic_analysis_context::{SemanticAnalysisContext, SemanticAnalysisError},
    span::Span,
};

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct SNBTString {
    pub span: Span,
    pub snbt_string: LowSNBTString,
}

impl From<String> for SNBTString {
    fn from(value: String) -> Self {
        Self {
            span: Span::dummy(),
            snbt_string: LowSNBTString(false, value),
        }
    }
}

impl SNBTString {
    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> (Span, LowSNBTString) {
        if self.snbt_string.has_macro_conflict() {
            ctx.add_error::<()>(self.span, SemanticAnalysisError::MacroConflict);
        }

        (self.span, self.snbt_string)
    }
}
