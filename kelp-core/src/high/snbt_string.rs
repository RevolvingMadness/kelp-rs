use std::hash::Hash;

use minecraft_command_types::{has_macro::HasMacro, impl_has_macro_false, snbt::SNBTString};
use parser_rs::parser_range::ParserRange;

use crate::semantic_analysis_context::{
    SemanticAnalysisContext, SemanticAnalysisError, SemanticAnalysisInfo, SemanticAnalysisInfoKind,
};

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct HighSNBTString {
    pub span: ParserRange,
    pub snbt_string: SNBTString,
}

impl_has_macro_false!(HighSNBTString);

impl From<String> for HighSNBTString {
    fn from(value: String) -> Self {
        HighSNBTString {
            span: ParserRange::dummy(),
            snbt_string: SNBTString(false, value),
        }
    }
}

impl HighSNBTString {
    #[must_use]
    pub fn perform_semantic_analysis(
        &self,
        ctx: &mut SemanticAnalysisContext,
        _is_lhs: bool,
    ) -> Option<()> {
        if self.snbt_string.has_macro_conflict() {
            ctx.add_info(SemanticAnalysisInfo {
                span: self.span,
                kind: SemanticAnalysisInfoKind::Error(SemanticAnalysisError::MacroConflict),
            })
        } else {
            Some(())
        }
    }
}
