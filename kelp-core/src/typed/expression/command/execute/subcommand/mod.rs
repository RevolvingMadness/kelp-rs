use std::collections::BTreeSet;

use minecraft_command_types::{
    command::{
        Command,
        enums::{
            axis::Axis, entity_anchor::EntityAnchor, relation::Relation, store_type::StoreType,
        },
        execute::ExecuteSubcommand,
    },
    resource_location::ResourceLocation,
};

use crate::{
    ast_allocator::low::LowAstAllocator,
    compile_context::CompileContext,
    datapack::Datapack,
    typed::{
        entity_selector::TypedEntitySelector,
        expression::command::{
            TypedCommand,
            execute::{
                facing::TypedFacing,
                positioned::TypedPositioned,
                rotated::TypedRotated,
                subcommand::{r#if::TypedExecuteIfSubcommand, store::TypedExecuteStoreSubcommand},
            },
        },
    },
};

pub mod r#if;
pub mod store;

#[derive(Debug, Clone)]
pub enum TypedExecuteSubcommand {
    Align(BTreeSet<Axis>, Box<Self>),
    Anchored(EntityAnchor, Box<Self>),
    As(TypedEntitySelector, Box<Self>),
    At(TypedEntitySelector, Box<Self>),
    Facing(TypedFacing, Box<Self>),
    In(ResourceLocation, Box<Self>),
    On(Relation, Box<Self>),
    Positioned(TypedPositioned, Box<Self>),
    Rotated(TypedRotated, Box<Self>),
    Summon(ResourceLocation, Box<Self>),
    If(bool, TypedExecuteIfSubcommand),
    Store(StoreType, TypedExecuteStoreSubcommand),
    Run(Vec<TypedCommand>),
    Multiple(Vec<Self>),
}

impl TypedExecuteSubcommand {
    #[must_use]
    pub fn then(self, next: Self) -> Self {
        match self {
            Self::Align(btree_set, high_execute_subcommand) => {
                Self::Align(btree_set, Box::new(high_execute_subcommand.then(next)))
            }
            Self::Anchored(entity_anchor, high_execute_subcommand) => {
                Self::Anchored(entity_anchor, Box::new(high_execute_subcommand.then(next)))
            }
            Self::As(entity_selector, high_execute_subcommand) => Self::As(
                entity_selector,
                Box::new(high_execute_subcommand.then(next)),
            ),
            Self::At(entity_selector, high_execute_subcommand) => Self::At(
                entity_selector,
                Box::new(high_execute_subcommand.then(next)),
            ),
            Self::Facing(facing, high_execute_subcommand) => {
                Self::Facing(facing, Box::new(high_execute_subcommand.then(next)))
            }
            Self::In(resource_location, high_execute_subcommand) => Self::In(
                resource_location,
                Box::new(high_execute_subcommand.then(next)),
            ),
            Self::On(relation, high_execute_subcommand) => {
                Self::On(relation, Box::new(high_execute_subcommand.then(next)))
            }
            Self::Positioned(positioned, high_execute_subcommand) => {
                Self::Positioned(positioned, Box::new(high_execute_subcommand.then(next)))
            }
            Self::Rotated(rotated, high_execute_subcommand) => {
                Self::Rotated(rotated, Box::new(high_execute_subcommand.then(next)))
            }
            Self::Summon(resource_location, high_execute_subcommand) => Self::Summon(
                resource_location,
                Box::new(high_execute_subcommand.then(next)),
            ),
            Self::If(inverted, high_execute_if_subcommand) => {
                Self::If(inverted, high_execute_if_subcommand.then(next))
            }
            Self::Store(store_type, high_execute_store_subcommand) => {
                Self::Store(store_type, high_execute_store_subcommand.then(next))
            }
            Self::Run(..) => next.then(self),
            Self::Multiple(high_execute_subcommands) => Self::Multiple(
                high_execute_subcommands
                    .into_iter()
                    .map(|subcommand| subcommand.then(next.clone()))
                    .collect(),
            ),
        }
    }

    pub fn compile(
        self,
        allocator: &LowAstAllocator,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> ExecuteSubcommand {
        match self {
            Self::Align(axes, next) => {
                let next = next.compile(allocator, datapack, ctx);
                ExecuteSubcommand::Align(axes, Box::new(next))
            }
            Self::Anchored(anchor, next) => {
                let next = next.compile(allocator, datapack, ctx);
                ExecuteSubcommand::Anchored(anchor, Box::new(next))
            }
            Self::As(selector, next) => {
                let selector = selector.compile(allocator, datapack, ctx);
                let next = next.compile(allocator, datapack, ctx);
                ExecuteSubcommand::As(selector, Box::new(next))
            }
            Self::At(selector, next) => {
                let selector = selector.compile(allocator, datapack, ctx);
                let next = next.compile(allocator, datapack, ctx);
                ExecuteSubcommand::At(selector, Box::new(next))
            }
            Self::Facing(facing, next) => {
                let facing = facing.compile(allocator, datapack, ctx);
                let next = next.compile(allocator, datapack, ctx);
                ExecuteSubcommand::Facing(facing, Box::new(next))
            }
            Self::In(location, next) => {
                let next = next.compile(allocator, datapack, ctx);
                ExecuteSubcommand::In(location, Box::new(next))
            }
            Self::On(relation, next) => {
                let next = next.compile(allocator, datapack, ctx);
                ExecuteSubcommand::On(relation, Box::new(next))
            }
            Self::Positioned(positioned, next) => {
                let positioned = positioned.compile(allocator, datapack, ctx);
                let next = next.compile(allocator, datapack, ctx);
                ExecuteSubcommand::Positioned(positioned, Box::new(next))
            }
            Self::Rotated(rotated, next) => {
                let rotated = rotated.compile(allocator, datapack, ctx);
                let next = next.compile(allocator, datapack, ctx);
                ExecuteSubcommand::Rotated(rotated, Box::new(next))
            }
            Self::Summon(entity_id, next) => {
                let next = next.compile(allocator, datapack, ctx);
                ExecuteSubcommand::Summon(entity_id, Box::new(next))
            }
            Self::If(is_if, next) => {
                let next = next.compile(allocator, datapack, ctx);
                ExecuteSubcommand::If(is_if, next)
            }
            Self::Store(store_type, next) => {
                let next = next.compile(allocator, datapack, ctx);
                ExecuteSubcommand::Store(store_type, next)
            }
            Self::Run(commands) => {
                let mut commands = commands
                    .into_iter()
                    .map(|command| command.compile(allocator, datapack, ctx))
                    .collect::<Vec<_>>();

                let last = commands.pop().unwrap();
                ctx.add_commands(datapack, commands);

                ExecuteSubcommand::Run(Box::new(last))
            }
            Self::Multiple(subcommands) => {
                let mut subcommands = subcommands
                    .into_iter()
                    .map(|subcommand| subcommand.compile(allocator, datapack, ctx))
                    .collect::<Vec<_>>();

                let last = subcommands.pop().unwrap();
                let commands = subcommands.into_iter().map(Command::Execute).collect();
                ctx.add_commands(datapack, commands);

                last
            }
        }
    }
}
