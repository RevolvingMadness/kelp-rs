use minecraft_command_types::{
    command::stopwatch::StopwatchCommand, resource_location::ResourceLocation,
};
use ordered_float::NotNan;

use crate::{
    compile_context::CompileContext, datapack::Datapack,
    semantic::supports_expression_sigil::SemanticSupportsExpressionSigil,
};

#[derive(Debug, Clone)]
pub enum SemanticStopwatchCommand {
    Create(SemanticSupportsExpressionSigil<ResourceLocation>),
    Query(
        SemanticSupportsExpressionSigil<ResourceLocation>,
        Option<NotNan<f32>>,
    ),
    Restart(SemanticSupportsExpressionSigil<ResourceLocation>),
    Remove(SemanticSupportsExpressionSigil<ResourceLocation>),
}

impl SemanticStopwatchCommand {
    #[must_use]
    pub fn compile(self, datapack: &mut Datapack, ctx: &mut CompileContext) -> StopwatchCommand {
        match self {
            Self::Create(resource_location) => {
                let resource_location = resource_location.compile(datapack, ctx);

                StopwatchCommand::Create(resource_location)
            }
            Self::Query(resource_location, scale) => {
                let resource_location = resource_location.compile(datapack, ctx);

                StopwatchCommand::Query(resource_location, scale)
            }
            Self::Restart(resource_location) => {
                let resource_location = resource_location.compile(datapack, ctx);

                StopwatchCommand::Restart(resource_location)
            }
            Self::Remove(resource_location) => {
                let resource_location = resource_location.compile(datapack, ctx);

                StopwatchCommand::Remove(resource_location)
            }
        }
    }
}
