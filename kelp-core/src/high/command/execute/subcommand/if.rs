use minecraft_command_types::{
    column_position::ColumnPosition,
    command::{
        enums::if_blocks_mode::IfBlocksMode,
        execute::{
            ExecuteIfSubcommand as LowExecuteIfSubcommand,
            ExecuteSubcommand as LowExecuteSubcommand,
        },
    },
    coordinate::Coordinates,
    resource_location::ResourceLocation,
};
use minecraft_command_types_derive::HasMacro;

use crate::{
    compile_context::CompileContext,
    datapack::Datapack,
    high::score_comparison::ScoreComparison,
    high::{
        block::BlockState, command::execute::subcommand::ExecuteSubcommand, data::DataTarget,
        entity_selector::EntitySelector, item::ItemPredicate, item_source::ItemSource,
        nbt_path::NbtPath, player_score::PlayerScore,
    },
    semantic_analysis_context::SemanticAnalysisContext,
};

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
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
    pub fn then(self, next: ExecuteSubcommand) -> Self {
        match self {
            Self::Biome(coordinates, resource_location, high_execute_subcommand) => Self::Biome(
                coordinates,
                resource_location,
                Some(Box::new(match high_execute_subcommand {
                    Some(subcommand) => subcommand.then(next),
                    None => next,
                })),
            ),
            Self::Block(coordinates, block_state, high_execute_subcommand) => Self::Block(
                coordinates,
                block_state,
                Some(Box::new(match high_execute_subcommand {
                    Some(subcommand) => subcommand.then(next),
                    None => next,
                })),
            ),
            Self::Blocks(
                coordinates,
                coordinates1,
                coordinates2,
                if_blocks_mode,
                high_execute_subcommand,
            ) => Self::Blocks(
                coordinates,
                coordinates1,
                coordinates2,
                if_blocks_mode,
                Some(Box::new(match high_execute_subcommand {
                    Some(subcommand) => subcommand.then(next),
                    None => next,
                })),
            ),
            Self::Data(data_target, nbt_path, high_execute_subcommand) => Self::Data(
                data_target,
                nbt_path,
                Some(Box::new(match high_execute_subcommand {
                    Some(subcommand) => subcommand.then(next),
                    None => next,
                })),
            ),
            Self::Dimension(resource_location, high_execute_subcommand) => Self::Dimension(
                resource_location,
                Some(Box::new(match high_execute_subcommand {
                    Some(subcommand) => subcommand.then(next),
                    None => next,
                })),
            ),
            Self::Entity(entity_selector, high_execute_subcommand) => Self::Entity(
                entity_selector,
                Some(Box::new(match high_execute_subcommand {
                    Some(subcommand) => subcommand.then(next),
                    None => next,
                })),
            ),
            Self::Function(resource_location, high_execute_subcommand) => Self::Function(
                resource_location,
                Some(Box::new(match high_execute_subcommand {
                    Some(subcommand) => subcommand.then(next),
                    None => next,
                })),
            ),
            Self::Items(item_source, slot, item_predicate, high_execute_subcommand) => Self::Items(
                item_source,
                slot,
                item_predicate,
                Some(Box::new(match high_execute_subcommand {
                    Some(subcommand) => subcommand.then(next),
                    None => next,
                })),
            ),
            Self::Loaded(column_position, high_execute_subcommand) => Self::Loaded(
                column_position,
                Some(Box::new(match high_execute_subcommand {
                    Some(subcommand) => subcommand.then(next),
                    None => next,
                })),
            ),
            Self::Predicate(resource_location, high_execute_subcommand) => Self::Predicate(
                resource_location,
                Some(Box::new(match high_execute_subcommand {
                    Some(subcommand) => subcommand.then(next),
                    None => next,
                })),
            ),
            Self::Score(player_score, score_comparison, high_execute_subcommand) => Self::Score(
                player_score,
                score_comparison,
                Some(Box::new(match high_execute_subcommand {
                    Some(subcommand) => subcommand.then(next),
                    None => next,
                })),
            ),
        }
    }

    pub fn perform_semantic_analysis(
        &self,
        ctx: &mut SemanticAnalysisContext,
        is_lhs: bool,
    ) -> Option<()> {
        match self {
            Self::Biome(_, _, next) => next
                .as_ref()
                .map_or(Some(()), |next| next.perform_semantic_analysis(ctx, is_lhs)),
            Self::Block(_, block_state, next) => {
                let block_state_result = block_state.perform_semantic_analysis(ctx, is_lhs);
                let next_result = next
                    .as_ref()
                    .map_or(Some(()), |next| next.perform_semantic_analysis(ctx, is_lhs));

                block_state_result?;
                next_result?;

                Some(())
            }
            Self::Blocks(_, _, _, _, next) => next
                .as_ref()
                .map_or(Some(()), |next| next.perform_semantic_analysis(ctx, is_lhs)),
            Self::Data(target, path, next) => {
                let target_result = target.kind.perform_semantic_analysis(ctx, is_lhs);
                let path_result = path.perform_semantic_analysis(ctx, is_lhs);
                let next_result = next
                    .as_ref()
                    .map_or(Some(()), |next| next.perform_semantic_analysis(ctx, is_lhs));

                target_result?;
                path_result?;
                next_result?;

                Some(())
            }
            Self::Dimension(_, next) => next
                .as_ref()
                .map_or(Some(()), |next| next.perform_semantic_analysis(ctx, is_lhs)),
            Self::Entity(selector, next) => {
                let selector_result = selector.perform_semantic_analysis(ctx, is_lhs);
                let next_result = next
                    .as_ref()
                    .map_or(Some(()), |next| next.perform_semantic_analysis(ctx, is_lhs));

                selector_result?;
                next_result?;

                Some(())
            }
            Self::Function(_, next) => next
                .as_ref()
                .map_or(Some(()), |next| next.perform_semantic_analysis(ctx, is_lhs)),
            Self::Items(item_source, _, item_predicate, next) => {
                let item_source_result = item_source.perform_semantic_analysis(ctx, is_lhs);
                let item_predicate_result = item_predicate.perform_semantic_analysis(ctx, is_lhs);
                let next_result = next
                    .as_ref()
                    .map_or(Some(()), |next| next.perform_semantic_analysis(ctx, is_lhs));

                item_source_result?;
                item_predicate_result?;
                next_result?;

                Some(())
            }
            Self::Loaded(_, next) => next
                .as_ref()
                .map_or(Some(()), |next| next.perform_semantic_analysis(ctx, is_lhs)),
            Self::Predicate(_, next) => next
                .as_ref()
                .map_or(Some(()), |next| next.perform_semantic_analysis(ctx, is_lhs)),
            Self::Score(score, score_comparison, next) => {
                let score_result = score.perform_semantic_analysis(ctx, is_lhs);
                let score_comparison_result =
                    score_comparison.perform_semantic_analysis(ctx, is_lhs);
                let next_result = next
                    .as_ref()
                    .map_or(Some(()), |next| next.perform_semantic_analysis(ctx, is_lhs));

                score_result?;
                score_comparison_result?;
                next_result?;

                Some(())
            }
        }
    }

    pub fn compile(
        self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> LowExecuteIfSubcommand {
        match self {
            Self::Biome(coords, biome, next) => {
                let next = next.map(|next| Box::new(next.compile(datapack, ctx)));
                LowExecuteIfSubcommand::Biome(coords, biome, next)
            }
            Self::Block(coordinates, state, next) => {
                let state = state.compile(datapack, ctx);
                let next = next.map(|next| Box::new(next.compile(datapack, ctx)));
                LowExecuteIfSubcommand::Block(coordinates, state, next)
            }
            Self::Blocks(start, end, destination, mode, next) => {
                let next = next.map(|next| Box::new(next.compile(datapack, ctx)));
                LowExecuteIfSubcommand::Blocks(start, end, destination, mode, next)
            }
            Self::Data(target, path, next) => {
                let target = target.compile(datapack, ctx);
                let path = path.compile(datapack, ctx);

                let next = next.map(|next| Box::new(next.compile(datapack, ctx)));

                LowExecuteIfSubcommand::Data(target.target, path, next)
            }
            Self::Dimension(location, next) => {
                let next = next.map(|next| Box::new(next.compile(datapack, ctx)));
                LowExecuteIfSubcommand::Dimension(location, next)
            }
            Self::Entity(selector, next) => {
                let selector = selector.compile(datapack, ctx);

                let next = next.map(|next| Box::new(next.compile(datapack, ctx)));

                LowExecuteIfSubcommand::Entity(selector, next)
            }
            Self::Function(location, next) => {
                let next = next.map(|next| Box::new(next.compile(datapack, ctx)));

                if let Some(next) = next {
                    LowExecuteIfSubcommand::Function(location, next)
                } else {
                    let mut req = datapack.requirements.get();
                    req.always_succeed_predicate = true;
                    datapack.requirements.set(req);

                    LowExecuteIfSubcommand::Function(
                        location,
                        Box::new(LowExecuteSubcommand::If(
                            false,
                            LowExecuteIfSubcommand::Predicate(
                                ResourceLocation::new_namespace_path("kelp", "always_succeed"),
                                None,
                            ),
                        )),
                    )
                }
            }
            Self::Items(source, name, predicate, next) => {
                let source = source.compile(datapack, ctx);
                let predicate = predicate.compile(datapack, ctx);

                let next = next.map(|next| Box::new(next.compile(datapack, ctx)));

                LowExecuteIfSubcommand::Items(source, name, predicate, next)
            }
            Self::Loaded(position, next) => {
                let next = next.map(|next| Box::new(next.compile(datapack, ctx)));
                LowExecuteIfSubcommand::Loaded(position, next)
            }
            Self::Predicate(location, next) => {
                let next = next.map(|next| Box::new(next.compile(datapack, ctx)));
                LowExecuteIfSubcommand::Predicate(location, next)
            }
            Self::Score(score, comparison, next) => {
                let score = score.compile(datapack, ctx);
                let next = next.map(|next| Box::new(next.compile(datapack, ctx)));

                LowExecuteIfSubcommand::Score(score.score, comparison.compile(datapack, ctx), next)
            }
        }
    }
}
