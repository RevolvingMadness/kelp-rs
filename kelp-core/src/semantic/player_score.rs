use minecraft_command_types::command::PlayerScore as LowPlayerScore;

use crate::{
    compile_context::CompileContext,
    datapack::Datapack,
    player_score::GeneratedPlayerScore,
    semantic::{
        entity_selector::SemanticEntitySelector,
        supports_expression_sigil::SemanticSupportsExpressionSigil,
    },
};

#[derive(Debug, Clone)]
pub struct SemanticPlayerScore {
    pub is_generated: bool,
    pub selector: Box<SemanticSupportsExpressionSigil<SemanticEntitySelector>>,
    pub objective: String,
}

impl SemanticPlayerScore {
    #[must_use]
    pub fn compile(
        self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> GeneratedPlayerScore {
        GeneratedPlayerScore {
            is_generated: false,
            score: LowPlayerScore::new(self.selector.compile(datapack, ctx), self.objective),
        }
    }
}
