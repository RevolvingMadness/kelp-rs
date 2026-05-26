use minecraft_command_types::command::PlayerScore;

use crate::{
    ast_allocator::low::LowAstAllocator,
    compile_context::CompileContext,
    datapack::Datapack,
    player_score::GeneratedPlayerScore,
    typed::{
        entity_selector::TypedEntitySelector, supports_expression_sigil::TypedSupportsExpressionSigil,
    },
};

#[derive(Debug, Clone)]
pub struct TypedPlayerScore {
    pub is_generated: bool,
    pub selector: Box<TypedSupportsExpressionSigil<TypedEntitySelector>>,
    pub objective: String,
}

impl TypedPlayerScore {
    #[must_use]
    pub fn compile(
        self,
        allocator: &LowAstAllocator,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> GeneratedPlayerScore {
        GeneratedPlayerScore {
            is_generated: false,
            score: PlayerScore::new(
                self.selector.compile(allocator, datapack, ctx),
                self.objective,
            ),
        }
    }
}
