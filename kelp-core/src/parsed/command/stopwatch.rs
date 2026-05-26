use minecraft_command_types::resource_location::ResourceLocation;
use ordered_float::NotNan;

use crate::{
    parsed::{
        semantic_analysis::SemanticAnalysisContext,
        supports_expression_sigil::ParsedSupportsExpressionSigil,
    },
    semantic::expression::command::stopwatch::SemanticStopwatchCommand,
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
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<SemanticStopwatchCommand> {
        Some(match self {
            Self::Create(resource_location) => {
                let resource_location = resource_location.perform_semantic_analysis(ctx)?;

                SemanticStopwatchCommand::Create(resource_location)
            }
            Self::Query(resource_location, scale) => {
                let resource_location = resource_location.perform_semantic_analysis(ctx)?;

                SemanticStopwatchCommand::Query(resource_location, scale)
            }
            Self::Restart(resource_location) => {
                let resource_location = resource_location.perform_semantic_analysis(ctx)?;

                SemanticStopwatchCommand::Restart(resource_location)
            }
            Self::Remove(resource_location) => {
                let resource_location = resource_location.perform_semantic_analysis(ctx)?;

                SemanticStopwatchCommand::Remove(resource_location)
            }
        })
    }
}
