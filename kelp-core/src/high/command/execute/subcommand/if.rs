use minecraft_command_types::{
    column_position::ColumnPosition,
    command::{
        enums::if_blocks_mode::IfBlocksMode,
        execute::{ExecuteIfSubcommand, ExecuteSubcommand},
    },
    coordinate::Coordinates,
    resource_location::ResourceLocation,
};
use minecraft_command_types_derive::HasMacro;

use crate::{
    compile_context::CompileContext,
    datapack::HighDatapack,
    high::score_comparison::HighScoreComparison,
    high::{
        block::HighBlockState, command::execute::subcommand::HighExecuteSubcommand,
        data::HighDataTarget, entity_selector::HighEntitySelector, item::HighItemPredicate,
        item_source::HighItemSource, nbt_path::HighNbtPath, player_score::HighPlayerScore,
    },
    semantic_analysis_context::SemanticAnalysisContext,
};

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum HighExecuteIfSubcommand {
    Biome(
        Coordinates,
        ResourceLocation,
        Option<Box<HighExecuteSubcommand>>,
    ),
    Block(
        Coordinates,
        HighBlockState,
        Option<Box<HighExecuteSubcommand>>,
    ),
    Blocks(
        Coordinates,
        Coordinates,
        Coordinates,
        IfBlocksMode,
        Option<Box<HighExecuteSubcommand>>,
    ),
    Data(
        HighDataTarget,
        HighNbtPath,
        Option<Box<HighExecuteSubcommand>>,
    ),
    Dimension(ResourceLocation, Option<Box<HighExecuteSubcommand>>),
    Entity(HighEntitySelector, Option<Box<HighExecuteSubcommand>>),
    Function(ResourceLocation, Option<Box<HighExecuteSubcommand>>),
    Items(
        HighItemSource,
        String,
        HighItemPredicate,
        Option<Box<HighExecuteSubcommand>>,
    ),
    Loaded(ColumnPosition, Option<Box<HighExecuteSubcommand>>),
    Predicate(ResourceLocation, Option<Box<HighExecuteSubcommand>>),
    Score(
        HighPlayerScore,
        HighScoreComparison,
        Option<Box<HighExecuteSubcommand>>,
    ),
}

impl HighExecuteIfSubcommand {
    #[must_use]
    pub fn then(self, next: HighExecuteSubcommand) -> HighExecuteIfSubcommand {
        match self {
            HighExecuteIfSubcommand::Biome(
                coordinates,
                resource_location,
                high_execute_subcommand,
            ) => HighExecuteIfSubcommand::Biome(
                coordinates,
                resource_location,
                Some(Box::new(match high_execute_subcommand {
                    Some(subcommand) => subcommand.then(next),
                    None => next,
                })),
            ),
            HighExecuteIfSubcommand::Block(coordinates, block_state, high_execute_subcommand) => {
                HighExecuteIfSubcommand::Block(
                    coordinates,
                    block_state,
                    Some(Box::new(match high_execute_subcommand {
                        Some(subcommand) => subcommand.then(next),
                        None => next,
                    })),
                )
            }
            HighExecuteIfSubcommand::Blocks(
                coordinates,
                coordinates1,
                coordinates2,
                if_blocks_mode,
                high_execute_subcommand,
            ) => HighExecuteIfSubcommand::Blocks(
                coordinates,
                coordinates1,
                coordinates2,
                if_blocks_mode,
                Some(Box::new(match high_execute_subcommand {
                    Some(subcommand) => subcommand.then(next),
                    None => next,
                })),
            ),
            HighExecuteIfSubcommand::Data(data_target, nbt_path, high_execute_subcommand) => {
                HighExecuteIfSubcommand::Data(
                    data_target,
                    nbt_path,
                    Some(Box::new(match high_execute_subcommand {
                        Some(subcommand) => subcommand.then(next),
                        None => next,
                    })),
                )
            }
            HighExecuteIfSubcommand::Dimension(resource_location, high_execute_subcommand) => {
                HighExecuteIfSubcommand::Dimension(
                    resource_location,
                    Some(Box::new(match high_execute_subcommand {
                        Some(subcommand) => subcommand.then(next),
                        None => next,
                    })),
                )
            }
            HighExecuteIfSubcommand::Entity(entity_selector, high_execute_subcommand) => {
                HighExecuteIfSubcommand::Entity(
                    entity_selector,
                    Some(Box::new(match high_execute_subcommand {
                        Some(subcommand) => subcommand.then(next),
                        None => next,
                    })),
                )
            }
            HighExecuteIfSubcommand::Function(resource_location, high_execute_subcommand) => {
                HighExecuteIfSubcommand::Function(
                    resource_location,
                    Some(Box::new(match high_execute_subcommand {
                        Some(subcommand) => subcommand.then(next),
                        None => next,
                    })),
                )
            }
            HighExecuteIfSubcommand::Items(
                item_source,
                slot,
                item_predicate,
                high_execute_subcommand,
            ) => HighExecuteIfSubcommand::Items(
                item_source,
                slot,
                item_predicate,
                Some(Box::new(match high_execute_subcommand {
                    Some(subcommand) => subcommand.then(next),
                    None => next,
                })),
            ),
            HighExecuteIfSubcommand::Loaded(column_position, high_execute_subcommand) => {
                HighExecuteIfSubcommand::Loaded(
                    column_position,
                    Some(Box::new(match high_execute_subcommand {
                        Some(subcommand) => subcommand.then(next),
                        None => next,
                    })),
                )
            }
            HighExecuteIfSubcommand::Predicate(resource_location, high_execute_subcommand) => {
                HighExecuteIfSubcommand::Predicate(
                    resource_location,
                    Some(Box::new(match high_execute_subcommand {
                        Some(subcommand) => subcommand.then(next),
                        None => next,
                    })),
                )
            }
            HighExecuteIfSubcommand::Score(
                player_score,
                score_comparison,
                high_execute_subcommand,
            ) => HighExecuteIfSubcommand::Score(
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
            HighExecuteIfSubcommand::Biome(_, _, next) => next
                .as_ref()
                .map_or(Some(()), |next| next.perform_semantic_analysis(ctx, is_lhs)),
            HighExecuteIfSubcommand::Block(_, block_state, next) => {
                let block_state_result = block_state.perform_semantic_analysis(ctx, is_lhs);
                let next_result = next
                    .as_ref()
                    .map_or(Some(()), |next| next.perform_semantic_analysis(ctx, is_lhs));

                block_state_result?;
                next_result?;

                Some(())
            }
            HighExecuteIfSubcommand::Blocks(_, _, _, _, next) => next
                .as_ref()
                .map_or(Some(()), |next| next.perform_semantic_analysis(ctx, is_lhs)),
            HighExecuteIfSubcommand::Data(target, path, next) => {
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
            HighExecuteIfSubcommand::Dimension(_, next) => next
                .as_ref()
                .map_or(Some(()), |next| next.perform_semantic_analysis(ctx, is_lhs)),
            HighExecuteIfSubcommand::Entity(selector, next) => {
                let selector_result = selector.perform_semantic_analysis(ctx, is_lhs);
                let next_result = next
                    .as_ref()
                    .map_or(Some(()), |next| next.perform_semantic_analysis(ctx, is_lhs));

                selector_result?;
                next_result?;

                Some(())
            }
            HighExecuteIfSubcommand::Function(_, next) => next
                .as_ref()
                .map_or(Some(()), |next| next.perform_semantic_analysis(ctx, is_lhs)),
            HighExecuteIfSubcommand::Items(item_source, _, item_predicate, next) => {
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
            HighExecuteIfSubcommand::Loaded(_, next) => next
                .as_ref()
                .map_or(Some(()), |next| next.perform_semantic_analysis(ctx, is_lhs)),
            HighExecuteIfSubcommand::Predicate(_, next) => next
                .as_ref()
                .map_or(Some(()), |next| next.perform_semantic_analysis(ctx, is_lhs)),
            HighExecuteIfSubcommand::Score(score, score_comparison, next) => {
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
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
    ) -> Option<ExecuteIfSubcommand> {
        match self {
            HighExecuteIfSubcommand::Biome(coords, biome, next) => {
                let next = next.and_then(|next| next.compile(datapack, ctx).map(Box::new));
                Some(ExecuteIfSubcommand::Biome(coords, biome, next))
            }
            HighExecuteIfSubcommand::Block(coordinates, state, next) => {
                let state = state.compile(datapack, ctx);
                let next = next.and_then(|next| next.compile(datapack, ctx).map(Box::new));
                Some(ExecuteIfSubcommand::Block(coordinates, state, next))
            }
            HighExecuteIfSubcommand::Blocks(start, end, destination, mode, next) => {
                let next = next.and_then(|next| next.compile(datapack, ctx).map(Box::new));
                Some(ExecuteIfSubcommand::Blocks(
                    start,
                    end,
                    destination,
                    mode,
                    next,
                ))
            }
            HighExecuteIfSubcommand::Data(target, path, next) => {
                let target = target.compile(datapack, ctx);
                let path = path.compile(datapack, ctx);

                let next = next.and_then(|next| next.compile(datapack, ctx).map(Box::new));

                Some(ExecuteIfSubcommand::Data(target.target, path, next))
            }
            HighExecuteIfSubcommand::Dimension(location, next) => {
                let next = next.and_then(|next| next.compile(datapack, ctx).map(Box::new));
                Some(ExecuteIfSubcommand::Dimension(location, next))
            }
            HighExecuteIfSubcommand::Entity(selector, next) => {
                let selector = selector.compile(datapack, ctx);

                let next = next.and_then(|next| next.compile(datapack, ctx).map(Box::new));

                Some(ExecuteIfSubcommand::Entity(selector, next))
            }
            HighExecuteIfSubcommand::Function(location, next) => {
                let next = next.map(|next| next.compile(datapack, ctx).map(Box::new));

                if let Some(next) = next.flatten() {
                    Some(ExecuteIfSubcommand::Function(location, next))
                } else {
                    let mut req = datapack.requirements.get();
                    req.always_succeed_predicate = true;
                    datapack.requirements.set(req);

                    Some(ExecuteIfSubcommand::Function(
                        location,
                        Box::new(ExecuteSubcommand::If(
                            false,
                            ExecuteIfSubcommand::Predicate(
                                ResourceLocation::new_namespace_path("kelp", "always_succeed"),
                                None,
                            ),
                        )),
                    ))
                }
            }
            HighExecuteIfSubcommand::Items(source, name, predicate, next) => {
                let source = source.compile(datapack, ctx);
                let predicate = predicate.compile(datapack, ctx);

                let next = next.and_then(|next| next.compile(datapack, ctx).map(Box::new));

                Some(ExecuteIfSubcommand::Items(source, name, predicate, next))
            }
            HighExecuteIfSubcommand::Loaded(position, next) => {
                let next = next.and_then(|next| next.compile(datapack, ctx).map(Box::new));
                Some(ExecuteIfSubcommand::Loaded(position, next))
            }
            HighExecuteIfSubcommand::Predicate(location, next) => {
                let next = next.and_then(|next| next.compile(datapack, ctx).map(Box::new));
                Some(ExecuteIfSubcommand::Predicate(location, next))
            }
            HighExecuteIfSubcommand::Score(score, comparison, next) => {
                let score = score.compile(datapack, ctx);
                let next = next.and_then(|next| next.compile(datapack, ctx).map(Box::new));

                Some(ExecuteIfSubcommand::Score(
                    score.score,
                    comparison.compile(datapack, ctx),
                    next,
                ))
            }
        }
    }
}
