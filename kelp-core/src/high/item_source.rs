use minecraft_command_types::{
    command::item_source::ItemSource as LowItemSource, coordinate::Coordinates,
};
use minecraft_command_types_derive::HasMacro;

use crate::{
    compile_context::CompileContext, datapack::Datapack, high::entity_selector::EntitySelector,
    semantic_analysis_context::SemanticAnalysisContext,
};

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum ItemSource {
    Block(Coordinates),
    Entity(EntitySelector),
}

impl ItemSource {
    pub fn perform_semantic_analysis(
        &self,
        ctx: &mut SemanticAnalysisContext,
        is_lhs: bool,
    ) -> Option<()> {
        match self {
            Self::Entity(selector) => selector.perform_semantic_analysis(ctx, is_lhs),
            Self::Block(_) => Some(()),
        }
    }

    pub fn compile(&self, datapack: &mut Datapack, ctx: &mut CompileContext) -> LowItemSource {
        match self {
            Self::Block(coordinates) => LowItemSource::Block(*coordinates),
            Self::Entity(selector) => {
                let selector = selector.clone().compile(datapack, ctx);

                LowItemSource::Entity(selector)
            }
        }
    }
}
