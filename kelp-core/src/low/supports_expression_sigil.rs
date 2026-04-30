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

macro_rules! impl_supports_expression_sigil {
    (no_compile, $ty:ty, $resolved_expression_variant:ident) => {
        impl SupportsExpressionSigil<$ty> {
            #[must_use]
            pub fn compile(self, datapack: &mut Datapack, ctx: &mut CompileContext) -> $ty {
                match self {
                    Self::Regular(value) => value,
                    Self::Sigil(expression) => {
                        let expression = expression.kind.resolve(datapack, ctx);

                        let ResolvedExpression::$resolved_expression_variant(value) = expression
                        else {
                            unreachable!();
                        };

                        value
                    }
                }
            }
        }
    };

    (compile_as_statement, $ty:ty, $ret_ty:ty, $resolved_expression_variant:ident) => {
        impl_supports_expression_sigil!($ty, $ret_ty, $resolved_expression_variant);

        impl SupportsExpressionSigil<$ty> {
            pub fn compile_as_statement(self, datapack: &mut Datapack, ctx: &mut CompileContext) {
                match self {
                    Self::Regular(value) => {
                        value.compile_as_statement(datapack, ctx);
                    }
                    Self::Sigil(expression) => {
                        expression.kind.compile_as_statement(datapack, ctx);
                    }
                }
            }
        }
    };

    ($ty:ty, $ret_ty:ty, $resolved_expression_variant:ident) => {
        impl SupportsExpressionSigil<$ty> {
            #[must_use]
            pub fn compile(self, datapack: &mut Datapack, ctx: &mut CompileContext) -> $ret_ty {
                match self {
                    Self::Regular(value) => value.compile(datapack, ctx),
                    Self::Sigil(expression) => {
                        let expression = expression.kind.resolve(datapack, ctx);

                        let ResolvedExpression::$resolved_expression_variant(value) = expression
                        else {
                            unreachable!();
                        };

                        value
                    }
                }
            }
        }
    };
}

impl_supports_expression_sigil!(no_compile, ResourceLocation, ResourceLocation);

impl_supports_expression_sigil!(
    compile_as_statement,
    EntitySelector,
    LowEntitySelector,
    EntitySelector
);
