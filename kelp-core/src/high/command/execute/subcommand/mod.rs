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
    trait_ext::OptionIterExt,
};

pub mod r#if;
pub mod store;

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

    pub fn perform_semantic_analysis(&self, ctx: &mut SemanticAnalysisContext, is_lhs: bool) -> Option<()> {
        match self {
            HighExecuteSubcommand::Align(_, next) => next.perform_semantic_analysis(ctx, is_lhs),
            HighExecuteSubcommand::Anchored(_, next) => next.perform_semantic_analysis(ctx, is_lhs),
            HighExecuteSubcommand::As(selector, next) => {
                let selector_result = selector.perform_semantic_analysis(ctx, is_lhs);
                let next_result = next.perform_semantic_analysis(ctx, is_lhs);

                selector_result?;
                next_result?;

                Some(())
            }
            HighExecuteSubcommand::At(selector, next) => {
                let selector_result = selector.perform_semantic_analysis(ctx, is_lhs);
                let next_result = next.perform_semantic_analysis(ctx, is_lhs);

                selector_result?;
                next_result?;

                Some(())
            }
            HighExecuteSubcommand::Facing(_, next) => next.perform_semantic_analysis(ctx, is_lhs),
            HighExecuteSubcommand::In(_, next) => next.perform_semantic_analysis(ctx, is_lhs),
            HighExecuteSubcommand::On(_, next) => next.perform_semantic_analysis(ctx, is_lhs),
            HighExecuteSubcommand::Positioned(_, next) => next.perform_semantic_analysis(ctx, is_lhs),
            HighExecuteSubcommand::Rotated(_, next) => next.perform_semantic_analysis(ctx, is_lhs),
            HighExecuteSubcommand::Summon(_, next) => next.perform_semantic_analysis(ctx, is_lhs),
            HighExecuteSubcommand::If(_, if_subcommand) => {
                if_subcommand.perform_semantic_analysis(ctx, is_lhs)
            }
            HighExecuteSubcommand::Store(_, store_subcommand) => {
                store_subcommand.perform_semantic_analysis(ctx, is_lhs)
            }
            HighExecuteSubcommand::Run(commands) => commands
                .iter()
                .map(|command| command.perform_semantic_analysis(ctx, is_lhs))
                .all_some(),
            HighExecuteSubcommand::Multiple(subcommands) => subcommands
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
