use std::collections::BTreeSet;

use minecraft_command_types::{
    command::enums::{
        axis::Axis, entity_anchor::EntityAnchor, relation::Relation, store_type::StoreType,
    },
    resource_location::ResourceLocation,
};

use crate::{
    parsed::{
        command::{
            ParsedCommand,
            execute::{
                facing::Facing,
                positioned::Positioned,
                rotated::Rotated,
                subcommand::{r#if::ParsedExecuteIfSubcommand, store::ExecuteStoreSubcommand},
            },
        },
        entity_selector::ParsedEntitySelector,
        semantic_analysis::SemanticAnalysisContext,
    },
    semantic::expression::command::execute::subcommand::SemanticExecuteSubcommand as MiddleExecuteSubcommand,
    trait_ext::CollectOptionAllIterExt,
};

pub mod r#if;
pub mod store;

#[derive(Debug, Clone)]
pub enum ParsedExecuteSubcommand {
    Align(BTreeSet<Axis>, Box<Self>),
    Anchored(EntityAnchor, Box<Self>),
    As(ParsedEntitySelector, Box<Self>),
    At(ParsedEntitySelector, Box<Self>),
    Facing(Facing, Box<Self>),
    In(ResourceLocation, Box<Self>),
    On(Relation, Box<Self>),
    Positioned(Positioned, Box<Self>),
    Rotated(Rotated, Box<Self>),
    Summon(ResourceLocation, Box<Self>),
    If(bool, Box<ParsedExecuteIfSubcommand>),
    Store(StoreType, ExecuteStoreSubcommand),
    Run(Vec<ParsedCommand>),
    Multiple(Vec<Self>),
}

impl ParsedExecuteSubcommand {
    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<MiddleExecuteSubcommand> {
        Some(match self {
            Self::As(selector, next) | Self::At(selector, next) => {
                let selector = selector.perform_semantic_analysis(ctx);
                let next = next.perform_semantic_analysis(ctx);

                let selector = selector?;
                let next = next?;

                MiddleExecuteSubcommand::As(selector, Box::new(next))
            }
            Self::Positioned(positioned, next) => {
                let positioned = positioned.perform_semantic_analysis(ctx);
                let next = next.perform_semantic_analysis(ctx);

                let positioned = positioned?;
                let next = next?;

                MiddleExecuteSubcommand::Positioned(positioned, Box::new(next))
            }
            Self::Align(alignment, next) => {
                let next = next.perform_semantic_analysis(ctx)?;

                MiddleExecuteSubcommand::Align(alignment, Box::new(next))
            }
            Self::Anchored(anchor, next) => {
                let next = next.perform_semantic_analysis(ctx)?;

                MiddleExecuteSubcommand::Anchored(anchor, Box::new(next))
            }
            Self::Facing(facing, next) => {
                let facing = facing.perform_semantic_analysis(ctx);
                let next = next.perform_semantic_analysis(ctx);

                let facing = facing?;
                let next = next?;

                MiddleExecuteSubcommand::Facing(facing, Box::new(next))
            }
            Self::In(resource_location, next) => {
                let next = next.perform_semantic_analysis(ctx)?;

                MiddleExecuteSubcommand::In(resource_location, Box::new(next))
            }
            Self::On(relation, next) => {
                let next = next.perform_semantic_analysis(ctx)?;

                MiddleExecuteSubcommand::On(relation, Box::new(next))
            }
            Self::Rotated(rotation, next) => {
                let rotation = rotation.perform_semantic_analysis(ctx);
                let next = next.perform_semantic_analysis(ctx);

                let rotation = rotation?;
                let next = next?;

                MiddleExecuteSubcommand::Rotated(rotation, Box::new(next))
            }
            Self::Summon(resource_location, next) => {
                let next = next.perform_semantic_analysis(ctx)?;

                MiddleExecuteSubcommand::Summon(resource_location, Box::new(next))
            }
            Self::If(inverted, subcommand) => {
                let subcommand = subcommand.perform_semantic_analysis(ctx)?;

                MiddleExecuteSubcommand::If(inverted, Box::new(subcommand))
            }
            Self::Store(store_type, subcommand) => {
                let subcommand = subcommand.perform_semantic_analysis(ctx)?;

                MiddleExecuteSubcommand::Store(store_type, subcommand)
            }
            Self::Run(commands) => {
                let commands = commands
                    .into_iter()
                    .map(|command| command.perform_semantic_analysis(ctx))
                    .collect_option_all()?;

                MiddleExecuteSubcommand::Run(commands)
            }
            Self::Multiple(subcommands) => {
                let subcommands = subcommands
                    .into_iter()
                    .map(|subcommand| subcommand.perform_semantic_analysis(ctx))
                    .collect_option_all()?;

                MiddleExecuteSubcommand::Multiple(subcommands)
            }
        })
    }
}
