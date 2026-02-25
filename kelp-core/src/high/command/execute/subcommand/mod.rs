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
use minecraft_command_types_derive::HasMacro;

use crate::{
    compile_context::CompileContext,
    datapack::HighDatapack,
    high::{
        command::{
            HighCommand,
            execute::{
                facing::HighFacing,
                positioned::HighPositioned,
                rotated::HighRotated,
                subcommand::{r#if::HighExecuteIfSubcommand, store::HighExecuteStoreSubcommand},
            },
        },
        entity_selector::HighEntitySelector,
    },
    semantic_analysis_context::SemanticAnalysisContext,
    trait_ext::OptionUnitIterExt,
};

pub mod r#if;
pub mod store;

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum HighExecuteSubcommand {
    Align(BTreeSet<Axis>, Box<Self>),
    Anchored(EntityAnchor, Box<Self>),
    As(HighEntitySelector, Box<Self>),
    At(HighEntitySelector, Box<Self>),
    Facing(HighFacing, Box<Self>),
    In(ResourceLocation, Box<Self>),
    On(Relation, Box<Self>),
    Positioned(HighPositioned, Box<Self>),
    Rotated(HighRotated, Box<Self>),
    Summon(ResourceLocation, Box<Self>),
    If(bool, HighExecuteIfSubcommand),
    Store(StoreType, HighExecuteStoreSubcommand),
    Run(Vec<HighCommand>),
    Multiple(Vec<Self>),
}

impl HighExecuteSubcommand {
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

    pub fn perform_semantic_analysis(
        &self,
        ctx: &mut SemanticAnalysisContext,
        is_lhs: bool,
    ) -> Option<()> {
        match self {
            Self::As(selector, next) | Self::At(selector, next) => {
                let selector_result = selector.perform_semantic_analysis(ctx, is_lhs);
                let next_result = next.perform_semantic_analysis(ctx, is_lhs);

                selector_result?;
                next_result?;

                Some(())
            }
            Self::Positioned(_, next)
            | Self::Align(_, next)
            | Self::Anchored(_, next)
            | Self::Facing(_, next)
            | Self::In(_, next)
            | Self::On(_, next)
            | Self::Rotated(_, next)
            | Self::Summon(_, next) => next.perform_semantic_analysis(ctx, is_lhs),
            Self::If(_, if_subcommand) => if_subcommand.perform_semantic_analysis(ctx, is_lhs),
            Self::Store(_, store_subcommand) => {
                store_subcommand.perform_semantic_analysis(ctx, is_lhs)
            }
            Self::Run(commands) => commands
                .iter()
                .map(|command| command.perform_semantic_analysis(ctx, is_lhs))
                .all_some(),
            Self::Multiple(subcommands) => subcommands
                .iter()
                .map(|subcommand| subcommand.perform_semantic_analysis(ctx, is_lhs))
                .all_some(),
        }
    }

    pub fn compile(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
    ) -> Option<ExecuteSubcommand> {
        match self {
            Self::Align(axes, next) => next
                .compile(datapack, ctx)
                .map(|next| ExecuteSubcommand::Align(axes, Box::new(next))),
            Self::Anchored(anchor, next) => next
                .compile(datapack, ctx)
                .map(|next| ExecuteSubcommand::Anchored(anchor, Box::new(next))),
            Self::As(selector, next) => {
                let selector = selector.compile(datapack, ctx);

                next.compile(datapack, ctx)
                    .map(|next| ExecuteSubcommand::As(selector, Box::new(next)))
            }
            Self::At(selector, next) => {
                let selector = selector.compile(datapack, ctx);

                next.compile(datapack, ctx)
                    .map(|next| ExecuteSubcommand::At(selector, Box::new(next)))
            }
            Self::Facing(facing, next) => {
                let facing = facing.compile(datapack, ctx);

                next.compile(datapack, ctx)
                    .map(|next| ExecuteSubcommand::Facing(facing, Box::new(next)))
            }
            Self::In(location, next) => next
                .compile(datapack, ctx)
                .map(|next| ExecuteSubcommand::In(location, Box::new(next))),
            Self::On(relation, next) => next
                .compile(datapack, ctx)
                .map(|next| ExecuteSubcommand::On(relation, Box::new(next))),
            Self::Positioned(positioned, next) => {
                let positioned = positioned.compile(datapack, ctx);

                next.compile(datapack, ctx)
                    .map(|next| ExecuteSubcommand::Positioned(positioned, Box::new(next)))
            }
            Self::Rotated(rotated, next) => {
                let rotated = rotated.compile(datapack, ctx);

                next.compile(datapack, ctx)
                    .map(|next| ExecuteSubcommand::Rotated(rotated, Box::new(next)))
            }
            Self::Summon(entity_id, next) => next
                .compile(datapack, ctx)
                .map(|next| ExecuteSubcommand::Summon(entity_id, Box::new(next))),
            Self::If(is_if, next) => next
                .compile(datapack, ctx)
                .map(|next| ExecuteSubcommand::If(is_if, next)),
            Self::Store(store_type, next) => next
                .compile(datapack, ctx)
                .map(|next| ExecuteSubcommand::Store(store_type, next)),
            Self::Run(commands) => {
                let mut commands = commands
                    .into_iter()
                    .filter_map(|command| command.compile(datapack, ctx))
                    .collect::<Vec<_>>();

                let last = commands.pop();

                ctx.add_commands(datapack, commands);

                last.map(|next| ExecuteSubcommand::Run(Box::new(next)))
            }
            Self::Multiple(subcommands) => {
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
