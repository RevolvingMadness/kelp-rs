use minecraft_command_types::{
    command::stopwatch::StopwatchCommand, resource_location::ResourceLocation,
};
use ordered_float::NotNan;

use crate::{
    ast_allocator::low::LowAstAllocator, compile_context::CompileContext, datapack::Datapack,
    typed::supports_expression_sigil::TypedSupportsExpressionSigil,
};

#[derive(Debug, Clone)]
pub enum TypedStopwatchCommand {
    Create(TypedSupportsExpressionSigil<ResourceLocation>),
    Query(
        TypedSupportsExpressionSigil<ResourceLocation>,
        Option<NotNan<f32>>,
    ),
    Restart(TypedSupportsExpressionSigil<ResourceLocation>),
    Remove(TypedSupportsExpressionSigil<ResourceLocation>),
}

impl TypedStopwatchCommand {
    #[must_use]
    pub fn compile(
        self,
        allocator: &LowAstAllocator,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> StopwatchCommand {
        match self {
            Self::Create(resource_location) => {
                let resource_location = resource_location.compile(allocator, datapack, ctx);

                StopwatchCommand::Create(resource_location)
            }
            Self::Query(resource_location, scale) => {
                let resource_location = resource_location.compile(allocator, datapack, ctx);

                StopwatchCommand::Query(resource_location, scale)
            }
            Self::Restart(resource_location) => {
                let resource_location = resource_location.compile(allocator, datapack, ctx);

                StopwatchCommand::Restart(resource_location)
            }
            Self::Remove(resource_location) => {
                let resource_location = resource_location.compile(allocator, datapack, ctx);

                StopwatchCommand::Remove(resource_location)
            }
        }
    }
}
