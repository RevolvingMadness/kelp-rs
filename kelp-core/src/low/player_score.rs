use minecraft_command_types::command::PlayerScore as LowPlayerScore;

use crate::{
    compile_context::CompileContext, datapack::Datapack, low::entity_selector::EntitySelector,
    player_score::GeneratedPlayerScore,
};

#[derive(Debug, Clone)]
pub struct PlayerScore {
    pub is_generated: bool,
    pub selector: EntitySelector,
    pub objective: String,
}

impl PlayerScore {
    #[must_use]
    pub const fn new(selector: EntitySelector, objective: String) -> Self {
        Self {
            is_generated: false,
            selector,
            objective,
        }
    }

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
