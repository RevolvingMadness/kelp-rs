use minecraft_command_types::resource_location::ResourceLocation;
use ordered_float::NotNan;

use crate::{
    parsed::arena::ParsedAstArena,
    parsed::{
        semantic_analysis::SemanticAnalysisContext,
        supports_expression_sigil::ParsedSupportsExpressionSigil,
    },
    typed::arena::TypedAstArena,
    typed::expression::command::stopwatch::TypedStopwatchCommand,
};

#[derive(Debug, Clone)]
pub enum ParsedStopwatchCommand {
    Create(ParsedSupportsExpressionSigil<ResourceLocation>),
    Query(
        ParsedSupportsExpressionSigil<ResourceLocation>,
        Option<NotNan<f32>>,
    ),
    Restart(ParsedSupportsExpressionSigil<ResourceLocation>),
    Remove(ParsedSupportsExpressionSigil<ResourceLocation>),
}

impl ParsedStopwatchCommand {
    pub fn perform_semantic_analysis(
        self,
        parsed_arena: &ParsedAstArena,
        typed_arena: &mut TypedAstArena,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<TypedStopwatchCommand> {
        Some(match self {
            Self::Create(resource_location) => {
                let resource_location =
                    resource_location.perform_semantic_analysis(parsed_arena, typed_arena, ctx)?;

                TypedStopwatchCommand::Create(resource_location)
            }
            Self::Query(resource_location, scale) => {
                let resource_location =
                    resource_location.perform_semantic_analysis(parsed_arena, typed_arena, ctx)?;

                TypedStopwatchCommand::Query(resource_location, scale)
            }
            Self::Restart(resource_location) => {
                let resource_location =
                    resource_location.perform_semantic_analysis(parsed_arena, typed_arena, ctx)?;

                TypedStopwatchCommand::Restart(resource_location)
            }
            Self::Remove(resource_location) => {
                let resource_location =
                    resource_location.perform_semantic_analysis(parsed_arena, typed_arena, ctx)?;

                TypedStopwatchCommand::Remove(resource_location)
            }
        })
    }
}
