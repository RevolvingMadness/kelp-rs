use std::fmt::Debug;

use la_arena::Idx;
use minecraft_command_types::{
    coordinate::Coordinates as LowCoordinates,
    entity_selector::EntitySelector as LowEntitySelector, resource_location::ResourceLocation,
};

use crate::{
    ast_allocator::low::LowAstAllocator,
    compile_context::CompileContext,
    datapack::Datapack,
    low::{
        coordinate::Coordinates,
        entity_selector::EntitySelector,
        expression::{resolved::ResolvedExpression, unresolved::UnresolvedExpression},
    },
};

#[derive(Debug, Clone)]
pub enum SupportsExpressionSigil<T> {
    Regular(T),
    Sigil(Idx<UnresolvedExpression>),
}

impl<T> SupportsExpressionSigil<T> {
    #[must_use]
    pub fn map<R>(self, f: impl FnOnce(T) -> R) -> SupportsExpressionSigil<R> {
        match self {
            Self::Regular(value) => SupportsExpressionSigil::Regular(f(value)),
            Self::Sigil(expression) => SupportsExpressionSigil::Sigil(expression),
        }
    }
}

macro_rules! impl_supports_expression_sigil {
    (no_compile, $ty:ty, $resolved_expression_variant:ident) => {
        impl SupportsExpressionSigil<$ty> {
            #[must_use]
            pub fn compile(
                self,
                allocator: &LowAstAllocator,
                datapack: &mut Datapack,
                ctx: &mut CompileContext,
            ) -> $ty {
                match self {
                    Self::Regular(value) => value,
                    Self::Sigil(expression) => {
                        let expression =
                            UnresolvedExpression::resolve(expression, allocator, datapack, ctx);

                        let ResolvedExpression::$resolved_expression_variant(value) = expression
                        else {
                            unreachable!();
                        };

                        value
                    }
                }
            }

            pub fn compile_as_statement(
                self,
                allocator: &LowAstAllocator,
                datapack: &mut Datapack,
                ctx: &mut CompileContext,
            ) {
                match self {
                    Self::Regular(..) => {}
                    Self::Sigil(expression) => UnresolvedExpression::compile_as_statement(
                        expression, allocator, datapack, ctx,
                    ),
                }
            }
        }
    };

    ($ty:ty, $ret_ty:ty, $resolved_expression_variant:ident) => {
        impl SupportsExpressionSigil<$ty> {
            #[must_use]
            pub fn compile(
                self,
                allocator: &LowAstAllocator,
                datapack: &mut Datapack,
                ctx: &mut CompileContext,
            ) -> $ret_ty {
                match self {
                    Self::Regular(value) => value.compile(allocator, datapack, ctx),
                    Self::Sigil(expression) => {
                        let expression =
                            UnresolvedExpression::resolve(expression, allocator, datapack, ctx);

                        match expression {
                            ResolvedExpression::$resolved_expression_variant(value) => value,
                            _ => unreachable!(),
                        }
                    }
                }
            }

            pub fn compile_as_statement(
                self,
                allocator: &LowAstAllocator,
                datapack: &mut Datapack,
                ctx: &mut CompileContext,
            ) {
                match self {
                    Self::Regular(value) => {
                        value.compile_as_statement(allocator, datapack, ctx);
                    }
                    Self::Sigil(expression) => UnresolvedExpression::compile_as_statement(
                        expression, allocator, datapack, ctx,
                    ),
                }
            }
        }
    };
}

impl_supports_expression_sigil!(no_compile, ResourceLocation, ResourceLocation);

impl_supports_expression_sigil!(EntitySelector, LowEntitySelector, EntitySelector);

impl_supports_expression_sigil!(Coordinates, LowCoordinates, Coordinates);

impl_supports_expression_sigil!(Box<Coordinates>, LowCoordinates, Coordinates);
