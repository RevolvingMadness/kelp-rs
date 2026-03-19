use minecraft_command_types::{
    column_position::ColumnPosition, command::enums::if_blocks_mode::IfBlocksMode,
    coordinate::Coordinates, resource_location::ResourceLocation,
};

use crate::{
    high::score_comparison::ScoreComparison,
    high::{
        block::BlockState, command::execute::subcommand::ExecuteSubcommand, data::DataTarget,
        entity_selector::EntitySelector, item_source::ItemSource, mc_item::ItemPredicate,
        nbt_path::NbtPath, player_score::PlayerScore,
    },
    middle::expression::command::execute::subcommand::r#if::ExecuteIfSubcommand as MiddleExecuteIfSubcommand,
    semantic_analysis_context::SemanticAnalysisContext,
};

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub enum ExecuteIfSubcommand {
    Biome(
        Coordinates,
        ResourceLocation,
        Option<Box<ExecuteSubcommand>>,
    ),
    Block(Coordinates, BlockState, Option<Box<ExecuteSubcommand>>),
    Blocks(
        Coordinates,
        Coordinates,
        Coordinates,
        IfBlocksMode,
        Option<Box<ExecuteSubcommand>>,
    ),
    Data(DataTarget, NbtPath, Option<Box<ExecuteSubcommand>>),
    Dimension(ResourceLocation, Option<Box<ExecuteSubcommand>>),
    Entity(EntitySelector, Option<Box<ExecuteSubcommand>>),
    Function(ResourceLocation, Option<Box<ExecuteSubcommand>>),
    Items(
        ItemSource,
        String,
        ItemPredicate,
        Option<Box<ExecuteSubcommand>>,
    ),
    Loaded(ColumnPosition, Option<Box<ExecuteSubcommand>>),
    Predicate(ResourceLocation, Option<Box<ExecuteSubcommand>>),
    Score(PlayerScore, ScoreComparison, Option<Box<ExecuteSubcommand>>),
}

impl ExecuteIfSubcommand {
    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<MiddleExecuteIfSubcommand> {
        Some(match self {
            Self::Biome(coordinates, resource_location, next) => {
                let next = match next {
                    Some(next) => Some(next.perform_semantic_analysis(ctx)?),
                    None => None,
                };

                MiddleExecuteIfSubcommand::Biome(coordinates, resource_location, next.map(Box::new))
            }
            Self::Block(coordinates, block_state, next) => {
                let block_state = block_state.perform_semantic_analysis(ctx);
                let next = match next {
                    Some(next) => Some(next.perform_semantic_analysis(ctx)?),
                    None => None,
                };

                let block_state = block_state?;

                MiddleExecuteIfSubcommand::Block(coordinates, block_state, next.map(Box::new))
            }
            Self::Blocks(start, end, desination, mode, next) => {
                let next = match next {
                    Some(next) => Some(next.perform_semantic_analysis(ctx)?),
                    None => None,
                };

                MiddleExecuteIfSubcommand::Blocks(start, end, desination, mode, next.map(Box::new))
            }
            Self::Data(target, path, next) => {
                let target = target.perform_semantic_analysis(ctx);
                let path = path.perform_semantic_analysis(ctx);
                let next = match next {
                    Some(next) => Some(next.perform_semantic_analysis(ctx)?),
                    None => None,
                };

                let target = target?;
                let path = path?;

                MiddleExecuteIfSubcommand::Data(target, path, next.map(Box::new))
            }
            Self::Dimension(resource_location, next) => {
                let next = match next {
                    Some(next) => Some(next.perform_semantic_analysis(ctx)?),
                    None => None,
                };

                MiddleExecuteIfSubcommand::Dimension(resource_location, next.map(Box::new))
            }
            Self::Entity(selector, next) => {
                let selector = selector.perform_semantic_analysis(ctx);
                let next = match next {
                    Some(next) => Some(next.perform_semantic_analysis(ctx)?),
                    None => None,
                };

                let selector = selector?;

                MiddleExecuteIfSubcommand::Entity(selector, next.map(Box::new))
            }
            Self::Function(resource_location, next) => {
                let next = match next {
                    Some(next) => Some(next.perform_semantic_analysis(ctx)?),
                    None => None,
                };

                MiddleExecuteIfSubcommand::Function(resource_location, next.map(Box::new))
            }
            Self::Items(item_source, slot, item_predicate, next) => {
                let item_source = item_source.perform_semantic_analysis(ctx);
                let item_predicate = item_predicate.perform_semantic_analysis(ctx);
                let next = match next {
                    Some(next) => Some(next.perform_semantic_analysis(ctx)?),
                    None => None,
                };

                let item_source = item_source?;
                let item_predicate = item_predicate?;

                MiddleExecuteIfSubcommand::Items(
                    item_source,
                    slot,
                    item_predicate,
                    next.map(Box::new),
                )
            }
            Self::Loaded(column_position, next) => {
                let next = match next {
                    Some(next) => Some(next.perform_semantic_analysis(ctx)?),
                    None => None,
                };

                MiddleExecuteIfSubcommand::Loaded(column_position, next.map(Box::new))
            }
            Self::Predicate(resource_location, next) => {
                let next = match next {
                    Some(next) => Some(next.perform_semantic_analysis(ctx)?),
                    None => None,
                };

                MiddleExecuteIfSubcommand::Predicate(resource_location, next.map(Box::new))
            }
            Self::Score(score, score_comparison, next) => {
                let score = score.perform_semantic_analysis(ctx);
                let score_comparison = score_comparison.perform_semantic_analysis(ctx);
                let next = match next {
                    Some(next) => Some(next.perform_semantic_analysis(ctx)?),
                    None => None,
                };

                let score = score?;
                let score_comparison = score_comparison?;

                MiddleExecuteIfSubcommand::Score(score, score_comparison, next.map(Box::new))
            }
        })
    }
}
