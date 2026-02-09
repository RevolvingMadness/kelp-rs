use minecraft_command_types::{command::item_source::ItemSource, coordinate::Coordinates};
use minecraft_command_types_derive::HasMacro;

use crate::{
    compile_context::CompileContext, datapack::HighDatapack,
    high::entity_selector::HighEntitySelector, semantic_analysis_context::SemanticAnalysisContext,
};

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum HighItemSource {
    Block(Coordinates),
    Entity(HighEntitySelector),
}

impl HighItemSource {
    pub fn perform_semantic_analysis(&self, ctx: &mut SemanticAnalysisContext, is_lhs: bool) -> Option<()> {
        match self {
            HighItemSource::Entity(selector) => selector.perform_semantic_analysis(ctx, is_lhs),
            _ => Some(()),
        }
    }

    pub fn compile(&self, datapack: &mut HighDatapack, ctx: &mut CompileContext) -> ItemSource {
        match self {
            HighItemSource::Block(coordinates) => ItemSource::Block(*coordinates),
            HighItemSource::Entity(selector) => {
                let selector = selector.clone().compile(datapack, ctx);

                ItemSource::Entity(selector)
            }
        }
    }
}
