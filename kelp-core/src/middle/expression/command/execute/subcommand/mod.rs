use std::collections::BTreeSet;

use minecraft_command_types::{
    command::{
        Command as LowCommand,
        enums::{
            axis::Axis, entity_anchor::EntityAnchor, relation::Relation, store_type::StoreType,
        },
        execute::ExecuteSubcommand as LowExecuteSubcommand,
    },
    resource_location::ResourceLocation,
};
use minecraft_command_types_derive::HasMacro;

use crate::{
    compile_context::CompileContext,
    datapack::Datapack,
    middle::{
        entity_selector::EntitySelector,
        expression::command::{
            Command,
            execute::{
                facing::Facing,
                positioned::Positioned,
                rotated::Rotated,
                subcommand::{r#if::ExecuteIfSubcommand, store::ExecuteStoreSubcommand},
            },
        },
    },
};

pub mod r#if;
pub mod store;

#[derive(Debug, Clone, Hash, HasMacro)]
pub enum ExecuteSubcommand {
    Align(BTreeSet<Axis>, Box<Self>),
    Anchored(EntityAnchor, Box<Self>),
    As(EntitySelector, Box<Self>),
    At(EntitySelector, Box<Self>),
    Facing(Facing, Box<Self>),
    In(ResourceLocation, Box<Self>),
    On(Relation, Box<Self>),
    Positioned(Positioned, Box<Self>),
    Rotated(Rotated, Box<Self>),
    Summon(ResourceLocation, Box<Self>),
    If(bool, ExecuteIfSubcommand),
    Store(StoreType, ExecuteStoreSubcommand),
    Run(Vec<Command>),
    Multiple(Vec<Self>),
}

impl ExecuteSubcommand {
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
            Self::Run(_) => next.then(self),
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
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> LowExecuteSubcommand {
        match self {
            Self::Align(axes, next) => {
                let next = next.compile(datapack, ctx);
                LowExecuteSubcommand::Align(axes, Box::new(next))
            }
            Self::Anchored(anchor, next) => {
                let next = next.compile(datapack, ctx);
                LowExecuteSubcommand::Anchored(anchor, Box::new(next))
            }
            Self::As(selector, next) => {
                let selector = selector.compile(datapack, ctx);
                let next = next.compile(datapack, ctx);
                LowExecuteSubcommand::As(selector, Box::new(next))
            }
            Self::At(selector, next) => {
                let selector = selector.compile(datapack, ctx);
                let next = next.compile(datapack, ctx);
                LowExecuteSubcommand::At(selector, Box::new(next))
            }
            Self::Facing(facing, next) => {
                let facing = facing.compile(datapack, ctx);
                let next = next.compile(datapack, ctx);
                LowExecuteSubcommand::Facing(facing, Box::new(next))
            }
            Self::In(location, next) => {
                let next = next.compile(datapack, ctx);
                LowExecuteSubcommand::In(location, Box::new(next))
            }
            Self::On(relation, next) => {
                let next = next.compile(datapack, ctx);
                LowExecuteSubcommand::On(relation, Box::new(next))
            }
            Self::Positioned(positioned, next) => {
                let positioned = positioned.compile(datapack, ctx);
                let next = next.compile(datapack, ctx);
                LowExecuteSubcommand::Positioned(positioned, Box::new(next))
            }
            Self::Rotated(rotated, next) => {
                let rotated = rotated.compile(datapack, ctx);
                let next = next.compile(datapack, ctx);
                LowExecuteSubcommand::Rotated(rotated, Box::new(next))
            }
            Self::Summon(entity_id, next) => {
                let next = next.compile(datapack, ctx);
                LowExecuteSubcommand::Summon(entity_id, Box::new(next))
            }
            Self::If(is_if, next) => {
                let next = next.compile(datapack, ctx);
                LowExecuteSubcommand::If(is_if, next)
            }
            Self::Store(store_type, next) => {
                let next = next.compile(datapack, ctx);
                LowExecuteSubcommand::Store(store_type, next)
            }
            Self::Run(commands) => {
                let mut commands = commands
                    .into_iter()
                    .map(|command| command.compile(datapack, ctx))
                    .collect::<Vec<_>>();

                let last = commands.pop().unwrap();
                ctx.add_commands(datapack, commands);

                LowExecuteSubcommand::Run(Box::new(last))
            }
            Self::Multiple(subcommands) => {
                let mut subcommands = subcommands
                    .into_iter()
                    .map(|subcommand| subcommand.compile(datapack, ctx))
                    .collect::<Vec<_>>();

                let last = subcommands.pop().unwrap();
                let commands = subcommands.into_iter().map(LowCommand::Execute).collect();
                ctx.add_commands(datapack, commands);

                last
            }
        }
    }
}
