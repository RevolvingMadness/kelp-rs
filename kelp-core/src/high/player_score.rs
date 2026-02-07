use minecraft_command_types::command::PlayerScore;
use minecraft_command_types_derive::HasMacro;

use crate::{
    compile_context::CompileContext, datapack::HighDatapack,
    high::entity_selector::HighEntitySelector, semantic_analysis_context::SemanticAnalysisContext,
};

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub struct HighPlayerScore {
    pub is_generated: bool,
    pub selector: HighEntitySelector,
    pub objective: String,
}

impl HighPlayerScore {
    pub fn new(selector: HighEntitySelector, objective: String) -> Self {
        Self {
            is_generated: false,
            selector,
            objective,
        }
    }

    pub fn perform_semantic_analysis(&self, ctx: &mut SemanticAnalysisContext) -> Option<()> {
        self.selector.perform_semantic_analysis(ctx)
    }

    pub fn compile(self, datapack: &mut HighDatapack, ctx: &mut CompileContext) -> PlayerScore {
        PlayerScore::new(self.selector.compile(datapack, ctx), self.objective)
    }
}
