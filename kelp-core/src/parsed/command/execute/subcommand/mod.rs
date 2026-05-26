use std::collections::BTreeSet;

use minecraft_command_types::{
    command::enums::{
        axis::Axis, entity_anchor::EntityAnchor, relation::Relation, store_type::StoreType,
    },
    resource_location::ResourceLocation,
};

use crate::{
    parsed::arena::ParsedAstArena,
    parsed::{
        command::{
            Command,
            execute::{
                facing::ParsedFacing,
                positioned::ParsedPositioned,
                rotated::ParsedRotated,
                subcommand::{
                    r#if::ParsedExecuteIfSubcommand, store::ParsedExecuteStoreSubcommand,
                },
            },
        },
        entity_selector::EntitySelector,
        semantic_analysis::SemanticAnalysisContext,
    },
    trait_ext::CollectOptionAllIterExt,
    typed::arena::TypedAstArena,
    typed::expression::command::execute::subcommand::TypedExecuteSubcommand,
};

pub mod r#if;
pub mod store;

#[derive(Debug, Clone)]
pub enum ParsedExecuteSubcommand {
    Align(BTreeSet<Axis>, Box<Self>),
    Anchored(EntityAnchor, Box<Self>),
    As(EntitySelector, Box<Self>),
    At(EntitySelector, Box<Self>),
    Facing(ParsedFacing, Box<Self>),
    In(ResourceLocation, Box<Self>),
    On(Relation, Box<Self>),
    Positioned(ParsedPositioned, Box<Self>),
    Rotated(ParsedRotated, Box<Self>),
    Summon(ResourceLocation, Box<Self>),
    If(bool, Box<ParsedExecuteIfSubcommand>),
    Store(StoreType, ParsedExecuteStoreSubcommand),
    Run(Vec<Command>),
    Multiple(Vec<Self>),
}

impl ParsedExecuteSubcommand {
    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        parsed_arena: &ParsedAstArena,
        typed_arena: &mut TypedAstArena,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<TypedExecuteSubcommand> {
        Some(match self {
            Self::As(selector, next) | Self::At(selector, next) => {
                let selector = selector.perform_semantic_analysis(parsed_arena, typed_arena, ctx);
                let next = next.perform_semantic_analysis(parsed_arena, typed_arena, ctx);

                let selector = selector?;
                let next = next?;

                TypedExecuteSubcommand::As(selector, Box::new(next))
            }
            Self::Positioned(positioned, next) => {
                let positioned =
                    positioned.perform_semantic_analysis(parsed_arena, typed_arena, ctx);
                let next = next.perform_semantic_analysis(parsed_arena, typed_arena, ctx);

                let positioned = positioned?;
                let next = next?;

                TypedExecuteSubcommand::Positioned(positioned, Box::new(next))
            }
            Self::Align(alignment, next) => {
                let next = next.perform_semantic_analysis(parsed_arena, typed_arena, ctx)?;

                TypedExecuteSubcommand::Align(alignment, Box::new(next))
            }
            Self::Anchored(anchor, next) => {
                let next = next.perform_semantic_analysis(parsed_arena, typed_arena, ctx)?;

                TypedExecuteSubcommand::Anchored(anchor, Box::new(next))
            }
            Self::Facing(facing, next) => {
                let facing = facing.perform_semantic_analysis(parsed_arena, typed_arena, ctx);
                let next = next.perform_semantic_analysis(parsed_arena, typed_arena, ctx);

                let facing = facing?;
                let next = next?;

                TypedExecuteSubcommand::Facing(facing, Box::new(next))
            }
            Self::In(resource_location, next) => {
                let next = next.perform_semantic_analysis(parsed_arena, typed_arena, ctx)?;

                TypedExecuteSubcommand::In(resource_location, Box::new(next))
            }
            Self::On(relation, next) => {
                let next = next.perform_semantic_analysis(parsed_arena, typed_arena, ctx)?;

                TypedExecuteSubcommand::On(relation, Box::new(next))
            }
            Self::Rotated(rotation, next) => {
                let rotation = rotation.perform_semantic_analysis(parsed_arena, typed_arena, ctx);
                let next = next.perform_semantic_analysis(parsed_arena, typed_arena, ctx);

                let rotation = rotation?;
                let next = next?;

                TypedExecuteSubcommand::Rotated(rotation, Box::new(next))
            }
            Self::Summon(resource_location, next) => {
                let next = next.perform_semantic_analysis(parsed_arena, typed_arena, ctx)?;

                TypedExecuteSubcommand::Summon(resource_location, Box::new(next))
            }
            Self::If(inverted, subcommand) => {
                let subcommand =
                    subcommand.perform_semantic_analysis(parsed_arena, typed_arena, ctx)?;

                TypedExecuteSubcommand::If(inverted, subcommand)
            }
            Self::Store(store_type, subcommand) => {
                let subcommand =
                    subcommand.perform_semantic_analysis(parsed_arena, typed_arena, ctx)?;

                TypedExecuteSubcommand::Store(store_type, subcommand)
            }
            Self::Run(commands) => {
                let commands = commands
                    .into_iter()
                    .map(|command| {
                        command.perform_semantic_analysis(parsed_arena, typed_arena, ctx)
                    })
                    .collect_option_all()?;

                TypedExecuteSubcommand::Run(commands)
            }
            Self::Multiple(subcommands) => {
                let subcommands = subcommands
                    .into_iter()
                    .map(|subcommand| {
                        subcommand.perform_semantic_analysis(parsed_arena, typed_arena, ctx)
                    })
                    .collect_option_all()?;

                TypedExecuteSubcommand::Multiple(subcommands)
            }
        })
    }
}
