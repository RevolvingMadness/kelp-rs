use std::fmt::Debug;

use minecraft_command_types::{
    coordinate::Coordinates as LowCoordinates,
    entity_selector::EntitySelector as LowEntitySelector, resource_location::ResourceLocation,
};

use crate::{
    ast_allocator::low::LowAstAllocator,
    compile_context::CompileContext,
    datapack::Datapack,
    low::expression::Expression,
    typed::{
        coordinate::TypedCoordinates,
        entity_selector::TypedEntitySelector,
        expression::{TypedExpression, TypedExpressionId},
    },
};

#[derive(Debug, Clone)]
pub enum TypedSupportsExpressionSigil<T> {
    Regular(T),
    Sigil(TypedExpressionId),
}

impl<T> TypedSupportsExpressionSigil<T> {
    #[must_use]
    pub fn map<R>(self, f: impl FnOnce(T) -> R) -> TypedSupportsExpressionSigil<R> {
        match self {
            Self::Regular(value) => TypedSupportsExpressionSigil::Regular(f(value)),
            Self::Sigil(expression) => TypedSupportsExpressionSigil::Sigil(expression),
        }
    }
}

macro_rules! impl_supports_expression_sigil {
    (no_compile, $ty:ty, $resolved_expression_variant:ident) => {
        impl TypedSupportsExpressionSigil<$ty> {
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
                            TypedExpression::resolve(expression, allocator, datapack, ctx);

                        let Expression::$resolved_expression_variant(value) = expression else {
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
                    Self::Sigil(expression) => {
                        TypedExpression::compile_as_statement(expression, allocator, datapack, ctx)
                    }
                }
            }
        }
    };

    ($ty:ty, $ret_ty:ty, $resolved_expression_variant:ident) => {
        impl TypedSupportsExpressionSigil<$ty> {
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
                            TypedExpression::resolve(expression, allocator, datapack, ctx);

                        match expression {
                            Expression::$resolved_expression_variant(value) => value,
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
                    Self::Sigil(expression) => {
                        TypedExpression::compile_as_statement(expression, allocator, datapack, ctx)
                    }
                }
            }
        }
    };
}

impl_supports_expression_sigil!(no_compile, ResourceLocation, ResourceLocation);

impl_supports_expression_sigil!(TypedEntitySelector, LowEntitySelector, EntitySelector);

impl_supports_expression_sigil!(TypedCoordinates, LowCoordinates, Coordinates);

impl_supports_expression_sigil!(Box<TypedCoordinates>, LowCoordinates, Coordinates);
