use minecraft_command_types::{
    column_position::ColumnPosition,
    command::{enums::if_blocks_mode::IfBlocksMode, execute::ExecuteIfSubcommand},
    coordinate::Coordinates,
    resource_location::ResourceLocation,
};

use crate::{
    compile_context::CompileContext,
    datapack::Datapack,
    typed::arena::TypedAstArena,
    typed::{
        block::TypedBlockState, data::TypedDataTarget, entity_selector::TypedEntitySelector,
        expression::command::execute::subcommand::TypedExecuteSubcommand,
        item_source::TypedItemSource, mc_item::TypedItemPredicate, nbt_path::TypedNbtPath,
        player_score::TypedPlayerScore, score_comparison::TypedScoreComparison,
        supports_expression_sigil::TypedSupportsExpressionSigil,
    },
};

#[derive(Debug, Clone)]
pub enum TypedExecuteIfSubcommand {
    Biome(
        Coordinates,
        TypedSupportsExpressionSigil<ResourceLocation>,
        Option<Box<TypedExecuteSubcommand>>,
    ),
    Block(
        Coordinates,
        TypedBlockState,
        Option<Box<TypedExecuteSubcommand>>,
    ),
    Blocks(
        Coordinates,
        Coordinates,
        Coordinates,
        IfBlocksMode,
        Option<Box<TypedExecuteSubcommand>>,
    ),
    Data(
        TypedDataTarget,
        TypedNbtPath,
        Option<Box<TypedExecuteSubcommand>>,
    ),
    Dimension(
        TypedSupportsExpressionSigil<ResourceLocation>,
        Option<Box<TypedExecuteSubcommand>>,
    ),
    Entity(TypedEntitySelector, Option<Box<TypedExecuteSubcommand>>),
    Function(
        TypedSupportsExpressionSigil<ResourceLocation>,
        Option<Box<TypedExecuteSubcommand>>,
    ),
    Items(
        TypedItemSource,
        String,
        TypedItemPredicate,
        Option<Box<TypedExecuteSubcommand>>,
    ),
    Loaded(ColumnPosition, Option<Box<TypedExecuteSubcommand>>),
    Predicate(
        TypedSupportsExpressionSigil<ResourceLocation>,
        Option<Box<TypedExecuteSubcommand>>,
    ),
    Score(
        TypedPlayerScore,
        TypedScoreComparison,
        Option<Box<TypedExecuteSubcommand>>,
    ),
}

impl TypedExecuteIfSubcommand {
    #[must_use]
    pub fn then(self, next: TypedExecuteSubcommand) -> Self {
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
        arena: &TypedAstArena,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> ExecuteIfSubcommand {
        match self {
            Self::Biome(coords, biome, next) => {
                let biome = biome.compile(arena, datapack, ctx);

                let next = next.map(|next| Box::new(next.compile(arena, datapack, ctx)));

                ExecuteIfSubcommand::Biome(coords, biome, next)
            }
            Self::Block(coordinates, state, next) => {
                let state = state.compile(arena, datapack, ctx);
                let next = next.map(|next| Box::new(next.compile(arena, datapack, ctx)));
                ExecuteIfSubcommand::Block(coordinates, state, next)
            }
            Self::Blocks(start, end, destination, mode, next) => {
                let next = next.map(|next| Box::new(next.compile(arena, datapack, ctx)));
                ExecuteIfSubcommand::Blocks(start, end, destination, mode, next)
            }
            Self::Data(target, path, next) => {
                let target = target.compile(arena, datapack, ctx);
                let path = path.compile(arena, datapack, ctx);

                let next = next.map(|next| Box::new(next.compile(arena, datapack, ctx)));

                ExecuteIfSubcommand::Data(target.target, path, next)
            }
            Self::Dimension(location, next) => {
                let location = location.compile(arena, datapack, ctx);

                let next = next.map(|next| Box::new(next.compile(arena, datapack, ctx)));

                ExecuteIfSubcommand::Dimension(location, next)
            }
            Self::Entity(selector, next) => {
                let selector = selector.compile(arena, datapack, ctx);

                let next = next.map(|next| Box::new(next.compile(arena, datapack, ctx)));

                ExecuteIfSubcommand::Entity(selector, next)
            }
            Self::Function(location, next) => {
                let location = location.compile(arena, datapack, ctx);

                let next = next.map(|next| Box::new(next.compile(arena, datapack, ctx)));

                if let Some(next) = next {
                    ExecuteIfSubcommand::Function(location, next)
                } else {
                    let mut req = datapack.requirements.get();
                    req.always_succeed_predicate = true;
                    datapack.requirements.set(req);

                    ExecuteIfSubcommand::Function(
                        location,
                        Box::new(
                            ExecuteIfSubcommand::Predicate(
                                ResourceLocation::new_namespace_path("kelp", "always_succeed"),
                                None,
                            )
                            .if_(),
                        ),
                    )
                }
            }
            Self::Items(source, name, predicate, next) => {
                let source = source.compile(arena, datapack, ctx);
                let predicate = predicate.compile(arena, datapack, ctx);

                let next = next.map(|next| Box::new(next.compile(arena, datapack, ctx)));

                ExecuteIfSubcommand::Items(source, name, predicate, next)
            }
            Self::Loaded(position, next) => {
                let next = next.map(|next| Box::new(next.compile(arena, datapack, ctx)));
                ExecuteIfSubcommand::Loaded(position, next)
            }
            Self::Predicate(location, next) => {
                let location = location.compile(arena, datapack, ctx);

                let next = next.map(|next| Box::new(next.compile(arena, datapack, ctx)));

                ExecuteIfSubcommand::Predicate(location, next)
            }
            Self::Score(score, comparison, next) => {
                let score = score.compile(arena, datapack, ctx);
                let next = next.map(|next| Box::new(next.compile(arena, datapack, ctx)));

                ExecuteIfSubcommand::Score(
                    score.score,
                    comparison.compile(arena, datapack, ctx),
                    next,
                )
            }
        }
    }
}
