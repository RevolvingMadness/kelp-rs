use minecraft_command_types::resource_location::ResourceLocation;
use ordered_float::NotNan;

use crate::{
    high::{
        semantic_analysis::SemanticAnalysisContext,
        supports_expression_sigil::SupportsExpressionSigil,
    },
    low::{
        data_type::DataType,
        expression::command::stopwatch::StopwatchCommand as LowStopwatchCommand,
    },
};

#[derive(Debug, Clone)]
pub enum StopwatchCommand {
    Create(SupportsExpressionSigil<ResourceLocation>),
    Query(
        SupportsExpressionSigil<ResourceLocation>,
        Option<NotNan<f32>>,
    ),
    Restart(SupportsExpressionSigil<ResourceLocation>),
    Remove(SupportsExpressionSigil<ResourceLocation>),
}

impl StopwatchCommand {
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<LowStopwatchCommand> {
        Some(match self {
            Self::Create(resource_location) => {
                let resource_location = resource_location
                    .perform_semantic_analysis(ctx, &DataType::ResourceLocation)?;

                LowStopwatchCommand::Create(resource_location)
            }
            Self::Query(resource_location, scale) => {
                let resource_location = resource_location
                    .perform_semantic_analysis(ctx, &DataType::ResourceLocation)?;

                LowStopwatchCommand::Query(resource_location, scale)
            }
            Self::Restart(resource_location) => {
                let resource_location = resource_location
                    .perform_semantic_analysis(ctx, &DataType::ResourceLocation)?;

                LowStopwatchCommand::Restart(resource_location)
            }
            Self::Remove(resource_location) => {
                let resource_location = resource_location
                    .perform_semantic_analysis(ctx, &DataType::ResourceLocation)?;

                LowStopwatchCommand::Remove(resource_location)
            }
        })
    }
}
