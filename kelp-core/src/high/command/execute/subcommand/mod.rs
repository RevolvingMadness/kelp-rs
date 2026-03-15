use std::collections::BTreeSet;

use minecraft_command_types::{
    command::enums::{
        axis::Axis, entity_anchor::EntityAnchor, relation::Relation, store_type::StoreType,
    },
    resource_location::ResourceLocation,
};
use minecraft_command_types_derive::HasMacro;

use crate::{
    high::{
        command::{
            Command,
            execute::{
                facing::Facing,
                positioned::Positioned,
                rotated::Rotated,
                subcommand::{r#if::ExecuteIfSubcommand, store::ExecuteStoreSubcommand},
            },
        },
        entity_selector::EntitySelector,
    },
    middle::expression::command::execute::subcommand::ExecuteSubcommand as MiddleExecuteSubcommand,
    semantic_analysis_context::SemanticAnalysisContext,
    trait_ext::CollectOptionAllIterExt,
};

pub mod r#if;
pub mod store;

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
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
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
        is_lhs: bool,
    ) -> Option<MiddleExecuteSubcommand> {
        Some(match self {
            Self::As(selector, next) | Self::At(selector, next) => {
                let selector = selector.perform_semantic_analysis(ctx, is_lhs);
                let next = next.perform_semantic_analysis(ctx, is_lhs);

                let selector = selector?;
                let next = next?;

                MiddleExecuteSubcommand::As(selector, Box::new(next))
            }
            Self::Positioned(positioned, next) => {
                let positioned = positioned.perform_semantic_analysis(ctx, is_lhs);
                let next = next.perform_semantic_analysis(ctx, is_lhs);

                let positioned = positioned?;
                let next = next?;

                MiddleExecuteSubcommand::Positioned(positioned, Box::new(next))
            }
            Self::Align(alignment, next) => {
                let next = next.perform_semantic_analysis(ctx, is_lhs)?;

                MiddleExecuteSubcommand::Align(alignment, Box::new(next))
            }
            Self::Anchored(anchor, next) => {
                let next = next.perform_semantic_analysis(ctx, is_lhs)?;

                MiddleExecuteSubcommand::Anchored(anchor, Box::new(next))
            }
            Self::Facing(facing, next) => {
                let facing = facing.perform_semantic_analysis(ctx, is_lhs);
                let next = next.perform_semantic_analysis(ctx, is_lhs);

                let facing = facing?;
                let next = next?;

                MiddleExecuteSubcommand::Facing(facing, Box::new(next))
            }
            Self::In(resource_location, next) => {
                let next = next.perform_semantic_analysis(ctx, is_lhs)?;

                MiddleExecuteSubcommand::In(resource_location, Box::new(next))
            }
            Self::On(relation, next) => {
                let next = next.perform_semantic_analysis(ctx, is_lhs)?;

                MiddleExecuteSubcommand::On(relation, Box::new(next))
            }
            Self::Rotated(rotation, next) => {
                let rotation = rotation.perform_semantic_analysis(ctx, is_lhs);
                let next = next.perform_semantic_analysis(ctx, is_lhs);

                let rotation = rotation?;
                let next = next?;

                MiddleExecuteSubcommand::Rotated(rotation, Box::new(next))
            }
            Self::Summon(resource_location, next) => {
                let next = next.perform_semantic_analysis(ctx, is_lhs)?;

                MiddleExecuteSubcommand::Summon(resource_location, Box::new(next))
            }
            Self::If(inverted, subcommand) => {
                let subcommand = subcommand.perform_semantic_analysis(ctx, is_lhs)?;

                MiddleExecuteSubcommand::If(inverted, subcommand)
            }
            Self::Store(store_type, subcommand) => {
                let subcommand = subcommand.perform_semantic_analysis(ctx, is_lhs)?;

                MiddleExecuteSubcommand::Store(store_type, subcommand)
            }
            Self::Run(commands) => {
                let commands = commands
                    .into_iter()
                    .map(|command| command.perform_semantic_analysis(ctx, is_lhs))
                    .collect_option_all()?;

                MiddleExecuteSubcommand::Run(commands)
            }
            Self::Multiple(subcommands) => {
                let subcommands = subcommands
                    .into_iter()
                    .map(|subcommand| subcommand.perform_semantic_analysis(ctx, is_lhs))
                    .collect_option_all()?;

                MiddleExecuteSubcommand::Multiple(subcommands)
            }
        })
    }
}
