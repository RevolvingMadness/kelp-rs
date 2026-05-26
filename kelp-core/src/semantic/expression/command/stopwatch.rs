use minecraft_command_types::{
    command::stopwatch::StopwatchCommand as LowStopwatchCommand,
    resource_location::ResourceLocation,
};
use ordered_float::NotNan;

use crate::{
    compile_context::CompileContext, datapack::Datapack,
    semantic::supports_expression_sigil::SemanticSupportsExpressionSigil,
};

#[derive(Debug, Clone)]
pub enum StopwatchCommand {
    Create(SemanticSupportsExpressionSigil<ResourceLocation>),
    Query(
        SemanticSupportsExpressionSigil<ResourceLocation>,
        Option<NotNan<f32>>,
    ),
    Restart(SemanticSupportsExpressionSigil<ResourceLocation>),
    Remove(SemanticSupportsExpressionSigil<ResourceLocation>),
}

impl StopwatchCommand {
    #[must_use]
    pub fn compile(self, datapack: &mut Datapack, ctx: &mut CompileContext) -> LowStopwatchCommand {
        match self {
            Self::Create(resource_location) => {
                let resource_location = resource_location.compile(datapack, ctx);

                LowStopwatchCommand::Create(resource_location)
            }
            Self::Query(resource_location, scale) => {
                let resource_location = resource_location.compile(datapack, ctx);

                LowStopwatchCommand::Query(resource_location, scale)
            }
            Self::Restart(resource_location) => {
                let resource_location = resource_location.compile(datapack, ctx);

                LowStopwatchCommand::Restart(resource_location)
            }
            Self::Remove(resource_location) => {
                let resource_location = resource_location.compile(datapack, ctx);

                LowStopwatchCommand::Remove(resource_location)
            }
        }
    }
}
