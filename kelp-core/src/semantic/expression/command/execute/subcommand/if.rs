use minecraft_command_types::{
    column_position::ColumnPosition,
    command::{
        enums::if_blocks_mode::IfBlocksMode,
        execute::{ExecuteIfSubcommand, ExecuteSubcommand},
    },
    coordinate::Coordinates,
    resource_location::ResourceLocation,
};

use crate::{
    compile_context::CompileContext,
    datapack::Datapack,
    semantic::{
        block::SemanticBlockState, data::SemanticDataTarget,
        entity_selector::SemanticEntitySelector,
        expression::command::execute::subcommand::SemanticExecuteSubcommand,
        item_source::SemanticItemSource, mc_item::SemanticItemPredicate, nbt_path::SemanticNbtPath,
        player_score::SemanticPlayerScore, score_comparison::ScoreComparison,
        supports_expression_sigil::SemanticSupportsExpressionSigil,
    },
};

#[derive(Debug, Clone)]
pub enum SemanticExecuteIfSubcommand {
    Biome(
        Coordinates,
        SemanticSupportsExpressionSigil<ResourceLocation>,
        Option<Box<SemanticExecuteSubcommand>>,
    ),
    Block(
        Coordinates,
        SemanticBlockState,
        Option<Box<SemanticExecuteSubcommand>>,
    ),
    Blocks(
        Coordinates,
        Coordinates,
        Coordinates,
        IfBlocksMode,
        Option<Box<SemanticExecuteSubcommand>>,
    ),
    Data(
        SemanticDataTarget,
        SemanticNbtPath,
        Option<Box<SemanticExecuteSubcommand>>,
    ),
    Dimension(
        SemanticSupportsExpressionSigil<ResourceLocation>,
        Option<Box<SemanticExecuteSubcommand>>,
    ),
    Entity(
        SemanticEntitySelector,
        Option<Box<SemanticExecuteSubcommand>>,
    ),
    Function(
        SemanticSupportsExpressionSigil<ResourceLocation>,
        Option<Box<SemanticExecuteSubcommand>>,
    ),
    Items(
        SemanticItemSource,
        String,
        SemanticItemPredicate,
        Option<Box<SemanticExecuteSubcommand>>,
    ),
    Loaded(ColumnPosition, Option<Box<SemanticExecuteSubcommand>>),
    Predicate(
        SemanticSupportsExpressionSigil<ResourceLocation>,
        Option<Box<SemanticExecuteSubcommand>>,
    ),
    Score(
        SemanticPlayerScore,
        ScoreComparison,
        Option<Box<SemanticExecuteSubcommand>>,
    ),
}

impl SemanticExecuteIfSubcommand {
    #[must_use]
    pub fn then(self, next: SemanticExecuteSubcommand) -> Self {
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

    pub fn compile(self, datapack: &mut Datapack, ctx: &mut CompileContext) -> ExecuteIfSubcommand {
        match self {
            Self::Biome(coords, biome, next) => {
                let biome = biome.compile(datapack, ctx);

                let next = next.map(|next| Box::new(next.compile(datapack, ctx)));

                ExecuteIfSubcommand::Biome(coords, biome, next)
            }
            Self::Block(coordinates, state, next) => {
                let state = state.compile(datapack, ctx);
                let next = next.map(|next| Box::new(next.compile(datapack, ctx)));
                ExecuteIfSubcommand::Block(coordinates, state, next)
            }
            Self::Blocks(start, end, destination, mode, next) => {
                let next = next.map(|next| Box::new(next.compile(datapack, ctx)));
                ExecuteIfSubcommand::Blocks(start, end, destination, mode, next)
            }
            Self::Data(target, path, next) => {
                let target = target.compile(datapack, ctx);
                let path = path.compile(datapack, ctx);

                let next = next.map(|next| Box::new(next.compile(datapack, ctx)));

                ExecuteIfSubcommand::Data(target.target, path, next)
            }
            Self::Dimension(location, next) => {
                let location = location.compile(datapack, ctx);

                let next = next.map(|next| Box::new(next.compile(datapack, ctx)));

                ExecuteIfSubcommand::Dimension(location, next)
            }
            Self::Entity(selector, next) => {
                let selector = selector.compile(datapack, ctx);

                let next = next.map(|next| Box::new(next.compile(datapack, ctx)));

                ExecuteIfSubcommand::Entity(selector, next)
            }
            Self::Function(location, next) => {
                let location = location.compile(datapack, ctx);

                let next = next.map(|next| Box::new(next.compile(datapack, ctx)));

                if let Some(next) = next {
                    ExecuteIfSubcommand::Function(location, next)
                } else {
                    let mut req = datapack.requirements.get();
                    req.always_succeed_predicate = true;
                    datapack.requirements.set(req);

                    ExecuteIfSubcommand::Function(
                        location,
                        Box::new(ExecuteSubcommand::If(
                            false,
                            ExecuteIfSubcommand::Predicate(
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

                ExecuteIfSubcommand::Items(source, name, predicate, next)
            }
            Self::Loaded(position, next) => {
                let next = next.map(|next| Box::new(next.compile(datapack, ctx)));
                ExecuteIfSubcommand::Loaded(position, next)
            }
            Self::Predicate(location, next) => {
                let location = location.compile(datapack, ctx);

                let next = next.map(|next| Box::new(next.compile(datapack, ctx)));

                ExecuteIfSubcommand::Predicate(location, next)
            }
            Self::Score(score, comparison, next) => {
                let score = score.compile(datapack, ctx);
                let next = next.map(|next| Box::new(next.compile(datapack, ctx)));

                ExecuteIfSubcommand::Score(score.score, comparison.compile(datapack, ctx), next)
            }
        }
    }
}
