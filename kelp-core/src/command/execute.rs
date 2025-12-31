use std::collections::BTreeSet;

use crate::block::HighBlockState;
use crate::command::context::CompileContext;
use crate::command::data::HighDataTarget;
use crate::command::item_source::HighItemSource;
use crate::command::{HighCommand, HighPlayerScore};
use crate::datapack::HighDatapack;
use crate::entity_selector::HighEntitySelector;
use crate::item::HighItemPredicate;
use crate::nbt_path::HighNbtPath;
use minecraft_command_types::column_position::ColumnPosition;
use minecraft_command_types::command::Command;
use minecraft_command_types::command::enums::heightmap::Heightmap;
use minecraft_command_types::command::execute::{
    ExecuteIfSubcommand, ExecuteStoreSubcommand, ExecuteSubcommand, Facing, Positioned, Rotated,
    ScoreComparisonOperator,
};
use minecraft_command_types::range::IntegerRange;
use minecraft_command_types::rotation::Rotation;
use minecraft_command_types::{
    command::{
        enums::{
            axis::Axis, bossbar_store_type::BossbarStoreType, entity_anchor::EntityAnchor,
            if_blocks_mode::IfBlocksMode, numeric_snbt_type::NumericSNBTType, relation::Relation,
            store_type::StoreType,
        },
        execute::ScoreComparison,
    },
    coordinate::Coordinates,
    resource_location::ResourceLocation,
};
use minecraft_command_types_derive::HasMacro;
use ordered_float::NotNan;

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum HighFacing {
    Position(Coordinates),
    Entity(HighEntitySelector, EntityAnchor),
}

impl HighFacing {
    pub fn compile(self, datapack: &mut HighDatapack, ctx: &mut CompileContext) -> Facing {
        match self {
            HighFacing::Position(position) => Facing::Position(position),
            HighFacing::Entity(selector, anchor) => {
                Facing::Entity(selector.compile(datapack, ctx), anchor)
            }
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum HighPositioned {
    Position(Coordinates),
    As(HighEntitySelector),
    Over(Heightmap),
}

impl HighPositioned {
    pub fn compile(self, datapack: &mut HighDatapack, ctx: &mut CompileContext) -> Positioned {
        match self {
            HighPositioned::Position(position) => Positioned::Position(position),
            HighPositioned::As(selector) => Positioned::As(selector.compile(datapack, ctx)),
            HighPositioned::Over(heightmap) => Positioned::Over(heightmap),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum HighRotated {
    Rotation(Rotation),
    As(HighEntitySelector),
}

impl HighRotated {
    pub fn compile(self, datapack: &mut HighDatapack, ctx: &mut CompileContext) -> Rotated {
        match self {
            HighRotated::Rotation(rotation) => Rotated::Rotation(rotation),
            HighRotated::As(selector) => Rotated::As(selector.compile(datapack, ctx)),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum HighScoreComparison {
    Range(IntegerRange),
    Score(ScoreComparisonOperator, HighPlayerScore),
}

impl HighScoreComparison {
    pub fn compile(self, datapack: &mut HighDatapack, ctx: &mut CompileContext) -> ScoreComparison {
        match self {
            HighScoreComparison::Range(range) => ScoreComparison::Range(range),
            HighScoreComparison::Score(operator, player_score) => {
                ScoreComparison::Score(operator, player_score.compile(datapack, ctx))
            }
        }
    }
}

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
                let target = target.kind.compile(datapack, ctx);
                let path = path.compile(datapack, ctx);

                let next = next.and_then(|next| next.compile(datapack, ctx).map(Box::new));

                Some(ExecuteIfSubcommand::Data(target, path, next))
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
            HighExecuteIfSubcommand::Score(player, comparison, next) => {
                let next = next.and_then(|next| next.compile(datapack, ctx).map(Box::new));

                Some(ExecuteIfSubcommand::Score(
                    player.compile(datapack, ctx),
                    comparison.compile(datapack, ctx),
                    next,
                ))
            }
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum HighExecuteStoreSubcommand {
    Data(
        HighDataTarget,
        HighNbtPath,
        NumericSNBTType,
        NotNan<f32>,
        Box<HighExecuteSubcommand>,
    ),
    Bossbar(
        ResourceLocation,
        BossbarStoreType,
        Box<HighExecuteSubcommand>,
    ),
    Score(HighPlayerScore, Box<HighExecuteSubcommand>),
}

impl HighExecuteStoreSubcommand {
    pub fn then(self, next: HighExecuteSubcommand) -> HighExecuteStoreSubcommand {
        match self {
            HighExecuteStoreSubcommand::Data(
                target,
                path,
                numeric_snbt_type,
                scale,
                high_execute_subcommand,
            ) => HighExecuteStoreSubcommand::Data(
                target,
                path,
                numeric_snbt_type,
                scale,
                Box::new(high_execute_subcommand.then(next)),
            ),
            HighExecuteStoreSubcommand::Bossbar(
                resource_location,
                bossbar_store_type,
                high_execute_subcommand,
            ) => HighExecuteStoreSubcommand::Bossbar(
                resource_location,
                bossbar_store_type,
                Box::new(high_execute_subcommand.then(next)),
            ),
            HighExecuteStoreSubcommand::Score(player_score, high_execute_subcommand) => {
                HighExecuteStoreSubcommand::Score(
                    player_score,
                    Box::new(high_execute_subcommand.then(next)),
                )
            }
        }
    }

    pub fn compile(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
    ) -> Option<ExecuteStoreSubcommand> {
        match self {
            HighExecuteStoreSubcommand::Data(target, path, numeric_snbt_type, scale, next) => {
                next.compile(datapack, ctx).map(|next| {
                    let target = target.kind.compile(datapack, ctx);
                    let path = path.compile(datapack, ctx);

                    ExecuteStoreSubcommand::Data(
                        target,
                        path,
                        numeric_snbt_type,
                        scale,
                        Box::new(next),
                    )
                })
            }
            HighExecuteStoreSubcommand::Bossbar(location, store_type, next) => next
                .compile(datapack, ctx)
                .map(|next| ExecuteStoreSubcommand::Bossbar(location, store_type, Box::new(next))),
            HighExecuteStoreSubcommand::Score(score, next) => {
                let score = score.compile(datapack, ctx);

                next.compile(datapack, ctx)
                    .map(|next| ExecuteStoreSubcommand::Score(score, Box::new(next)))
            }
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum HighExecuteSubcommand {
    Align(BTreeSet<Axis>, Box<HighExecuteSubcommand>),
    Anchored(EntityAnchor, Box<HighExecuteSubcommand>),
    As(HighEntitySelector, Box<HighExecuteSubcommand>),
    At(HighEntitySelector, Box<HighExecuteSubcommand>),
    Facing(HighFacing, Box<HighExecuteSubcommand>),
    In(ResourceLocation, Box<HighExecuteSubcommand>),
    On(Relation, Box<HighExecuteSubcommand>),
    Positioned(HighPositioned, Box<HighExecuteSubcommand>),
    Rotated(HighRotated, Box<HighExecuteSubcommand>),
    Summon(ResourceLocation, Box<HighExecuteSubcommand>),
    If(bool, HighExecuteIfSubcommand),
    Store(StoreType, HighExecuteStoreSubcommand),
    Run(Vec<HighCommand>),
    Multiple(Vec<HighExecuteSubcommand>),
}

impl HighExecuteSubcommand {
    pub fn then(self, next: HighExecuteSubcommand) -> HighExecuteSubcommand {
        match self {
            HighExecuteSubcommand::Align(btree_set, high_execute_subcommand) => {
                HighExecuteSubcommand::Align(
                    btree_set,
                    Box::new(high_execute_subcommand.then(next)),
                )
            }
            HighExecuteSubcommand::Anchored(entity_anchor, high_execute_subcommand) => {
                HighExecuteSubcommand::Anchored(
                    entity_anchor,
                    Box::new(high_execute_subcommand.then(next)),
                )
            }
            HighExecuteSubcommand::As(entity_selector, high_execute_subcommand) => {
                HighExecuteSubcommand::As(
                    entity_selector,
                    Box::new(high_execute_subcommand.then(next)),
                )
            }
            HighExecuteSubcommand::At(entity_selector, high_execute_subcommand) => {
                HighExecuteSubcommand::At(
                    entity_selector,
                    Box::new(high_execute_subcommand.then(next)),
                )
            }
            HighExecuteSubcommand::Facing(facing, high_execute_subcommand) => {
                HighExecuteSubcommand::Facing(facing, Box::new(high_execute_subcommand.then(next)))
            }
            HighExecuteSubcommand::In(resource_location, high_execute_subcommand) => {
                HighExecuteSubcommand::In(
                    resource_location,
                    Box::new(high_execute_subcommand.then(next)),
                )
            }
            HighExecuteSubcommand::On(relation, high_execute_subcommand) => {
                HighExecuteSubcommand::On(relation, Box::new(high_execute_subcommand.then(next)))
            }
            HighExecuteSubcommand::Positioned(positioned, high_execute_subcommand) => {
                HighExecuteSubcommand::Positioned(
                    positioned,
                    Box::new(high_execute_subcommand.then(next)),
                )
            }
            HighExecuteSubcommand::Rotated(rotated, high_execute_subcommand) => {
                HighExecuteSubcommand::Rotated(
                    rotated,
                    Box::new(high_execute_subcommand.then(next)),
                )
            }
            HighExecuteSubcommand::Summon(resource_location, high_execute_subcommand) => {
                HighExecuteSubcommand::Summon(
                    resource_location,
                    Box::new(high_execute_subcommand.then(next)),
                )
            }
            HighExecuteSubcommand::If(inverted, high_execute_if_subcommand) => {
                HighExecuteSubcommand::If(inverted, high_execute_if_subcommand.then(next))
            }
            HighExecuteSubcommand::Store(store_type, high_execute_store_subcommand) => {
                HighExecuteSubcommand::Store(store_type, high_execute_store_subcommand.then(next))
            }
            HighExecuteSubcommand::Run(_) => next.then(self),
            HighExecuteSubcommand::Multiple(high_execute_subcommands) => {
                HighExecuteSubcommand::Multiple(
                    high_execute_subcommands
                        .into_iter()
                        .map(|subcommand| subcommand.then(next.clone()))
                        .collect(),
                )
            }
        }
    }

    pub fn compile(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
    ) -> Option<ExecuteSubcommand> {
        match self {
            HighExecuteSubcommand::Align(axes, next) => next
                .compile(datapack, ctx)
                .map(|next| ExecuteSubcommand::Align(axes, Box::new(next))),
            HighExecuteSubcommand::Anchored(anchor, next) => next
                .compile(datapack, ctx)
                .map(|next| ExecuteSubcommand::Anchored(anchor, Box::new(next))),
            HighExecuteSubcommand::As(selector, next) => {
                let selector = selector.compile(datapack, ctx);

                next.compile(datapack, ctx)
                    .map(|next| ExecuteSubcommand::As(selector, Box::new(next)))
            }
            HighExecuteSubcommand::At(selector, next) => {
                let selector = selector.compile(datapack, ctx);

                next.compile(datapack, ctx)
                    .map(|next| ExecuteSubcommand::At(selector, Box::new(next)))
            }
            HighExecuteSubcommand::Facing(facing, next) => {
                let facing = facing.compile(datapack, ctx);

                next.compile(datapack, ctx)
                    .map(|next| ExecuteSubcommand::Facing(facing, Box::new(next)))
            }
            HighExecuteSubcommand::In(location, next) => next
                .compile(datapack, ctx)
                .map(|next| ExecuteSubcommand::In(location, Box::new(next))),
            HighExecuteSubcommand::On(relation, next) => next
                .compile(datapack, ctx)
                .map(|next| ExecuteSubcommand::On(relation, Box::new(next))),
            HighExecuteSubcommand::Positioned(positioned, next) => {
                let positioned = positioned.compile(datapack, ctx);

                next.compile(datapack, ctx)
                    .map(|next| ExecuteSubcommand::Positioned(positioned, Box::new(next)))
            }
            HighExecuteSubcommand::Rotated(rotated, next) => {
                let rotated = rotated.compile(datapack, ctx);

                next.compile(datapack, ctx)
                    .map(|next| ExecuteSubcommand::Rotated(rotated, Box::new(next)))
            }
            HighExecuteSubcommand::Summon(entity_id, next) => next
                .compile(datapack, ctx)
                .map(|next| ExecuteSubcommand::Summon(entity_id, Box::new(next))),
            HighExecuteSubcommand::If(is_if, next) => next
                .compile(datapack, ctx)
                .map(|next| ExecuteSubcommand::If(is_if, next)),
            HighExecuteSubcommand::Store(store_type, next) => next
                .compile(datapack, ctx)
                .map(|next| ExecuteSubcommand::Store(store_type, next)),
            HighExecuteSubcommand::Run(commands) => {
                let mut commands = commands
                    .into_iter()
                    .filter_map(|command| command.compile(datapack, ctx))
                    .collect::<Vec<_>>();

                let last = commands.pop();

                ctx.add_commands(datapack, commands);

                last.map(|next| ExecuteSubcommand::Run(Box::new(next)))
            }
            HighExecuteSubcommand::Multiple(subcommands) => {
                let mut subcommands = subcommands
                    .into_iter()
                    .filter_map(|subcommand| subcommand.compile(datapack, ctx))
                    .collect::<Vec<_>>();

                let last = subcommands.pop();

                let commands = subcommands.into_iter().map(Command::Execute).collect();

                ctx.add_commands(datapack, commands);

                last
            }
        }
    }
}
