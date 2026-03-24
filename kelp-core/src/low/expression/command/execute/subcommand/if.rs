use minecraft_command_types::{
    column_position::ColumnPosition, command::enums::if_blocks_mode::IfBlocksMode,
    command::execute::ExecuteIfSubcommand as LowExecuteIfSubcommand,
    command::execute::ExecuteSubcommand as LowExecuteSubcommand, coordinate::Coordinates,
    resource_location::ResourceLocation,
};

use crate::{
    compile_context::CompileContext,
    datapack::Datapack,
    low::{
        block::BlockState, data::DataTarget, entity_selector::EntitySelector,
        expression::command::execute::subcommand::ExecuteSubcommand, item_source::ItemSource,
        mc_item::ItemPredicate, nbt_path::NbtPath, player_score::PlayerScore,
        score_comparison::ScoreComparison,
    },
};

#[derive(Debug, Clone)]
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
