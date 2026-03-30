use std::fmt::Debug;

use minecraft_command_types::{
    entity_selector::EntitySelector as LowEntitySelector, resource_location::ResourceLocation,
};

use crate::{
    compile_context::CompileContext,
    datapack::Datapack,
    low::{
        entity_selector::EntitySelector,
        expression::{resolved::ResolvedExpression, unresolved::UnresolvedExpression},
    },
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

impl SupportsExpressionSigil<EntitySelector> {
    #[must_use]
    pub fn compile(self, datapack: &mut Datapack, ctx: &mut CompileContext) -> LowEntitySelector {
        match self {
            Self::Regular(selector) => selector.compile(datapack, ctx),
            Self::Sigil(expression) => {
                let ResolvedExpression::EntitySelector(selector) =
                    expression.kind.resolve(datapack, ctx)
                else {
                    unreachable!();
                };

                selector
            }
        }
    }

    pub fn compile_as_statement(self, datapack: &mut Datapack, ctx: &mut CompileContext) {
        match self {
            Self::Regular(selector) => {
                selector.compile_as_statement(datapack, ctx);
            }
            Self::Sigil(expression) => {
                expression.kind.compile_as_statement(datapack, ctx);
            }
        }
    }
}
