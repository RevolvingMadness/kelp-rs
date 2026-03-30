use std::fmt::Debug;

use minecraft_command_types::resource_location::ResourceLocation;

use crate::{
    compile_context::CompileContext,
    datapack::Datapack,
    low::expression::{resolved::ResolvedExpression, unresolved::UnresolvedExpression},
};

#[derive(Debug, Clone)]
pub enum SupportsExpressionSigil<T> {
    Regular(T),
    Sigil(UnresolvedExpression),
}

impl SupportsExpressionSigil<ResourceLocation> {
    #[must_use]
    pub fn compile(self, datapack: &mut Datapack, ctx: &mut CompileContext) -> ResourceLocation {
        match self {
            Self::Regular(value) => value,
            Self::Sigil(expression) => {
                let ResolvedExpression::ResourceLocation(resource_location) =
                    expression.kind.resolve(datapack, ctx)
                else {
                    unreachable!();
                };

                resource_location
            }
        }
    }
}
