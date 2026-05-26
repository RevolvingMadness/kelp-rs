use minecraft_command_types::resource_location::ResourceLocation;
use ordered_float::NotNan;

use crate::{
    parsed::{
        semantic_analysis::SemanticAnalysisContext,
        supports_expression_sigil::ParsedSupportsExpressionSigil,
    },
    semantic::expression::command::stopwatch::SemanticStopwatchCommand as LowStopwatchCommand,
};

#[derive(Debug, Clone)]
pub enum StopwatchCommand {
    Create(ParsedSupportsExpressionSigil<ResourceLocation>),
    Query(
        ParsedSupportsExpressionSigil<ResourceLocation>,
        Option<NotNan<f32>>,
    ),
    Restart(ParsedSupportsExpressionSigil<ResourceLocation>),
    Remove(ParsedSupportsExpressionSigil<ResourceLocation>),
}

impl StopwatchCommand {
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<LowStopwatchCommand> {
        Some(match self {
            Self::Create(resource_location) => {
                let resource_location = resource_location.perform_semantic_analysis(ctx)?;

                LowStopwatchCommand::Create(resource_location)
            }
            Self::Query(resource_location, scale) => {
                let resource_location = resource_location.perform_semantic_analysis(ctx)?;

                LowStopwatchCommand::Query(resource_location, scale)
            }
            Self::Restart(resource_location) => {
                let resource_location = resource_location.perform_semantic_analysis(ctx)?;

                LowStopwatchCommand::Restart(resource_location)
            }
            Self::Remove(resource_location) => {
                let resource_location = resource_location.perform_semantic_analysis(ctx)?;

                LowStopwatchCommand::Remove(resource_location)
            }
        })
    }
}
