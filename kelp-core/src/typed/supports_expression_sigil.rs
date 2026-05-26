use std::fmt::Debug;

use minecraft_command_types::{
    coordinate::Coordinates as LowCoordinates,
    entity_selector::EntitySelector as LowEntitySelector, resource_location::ResourceLocation,
};

use crate::{
    compile_context::CompileContext,
    datapack::Datapack,
    low::expression::Expression,
    typed::arena::TypedAstArena,
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
                arena: &TypedAstArena,
                datapack: &mut Datapack,
                ctx: &mut CompileContext,
            ) -> $ty {
                match self {
                    Self::Regular(value) => value,
                    Self::Sigil(expression) => {
                        let expression = TypedExpression::resolve(expression, arena, datapack, ctx);

                        let Expression::$resolved_expression_variant(value) = expression else {
                            unreachable!();
                        };

                        value
                    }
                }
            }

            pub fn compile_as_statement(
                self,
                arena: &TypedAstArena,
                datapack: &mut Datapack,
                ctx: &mut CompileContext,
            ) {
                match self {
                    Self::Regular(..) => {}
                    Self::Sigil(expression) => {
                        TypedExpression::compile_as_statement(expression, arena, datapack, ctx)
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
                arena: &TypedAstArena,
                datapack: &mut Datapack,
                ctx: &mut CompileContext,
            ) -> $ret_ty {
                match self {
                    Self::Regular(value) => value.compile(arena, datapack, ctx),
                    Self::Sigil(expression) => {
                        let expression = TypedExpression::resolve(expression, arena, datapack, ctx);

                        match expression {
                            Expression::$resolved_expression_variant(value) => value,
                            _ => unreachable!(),
                        }
                    }
                }
            }

            pub fn compile_as_statement(
                self,
                arena: &TypedAstArena,
                datapack: &mut Datapack,
                ctx: &mut CompileContext,
            ) {
                match self {
                    Self::Regular(value) => {
                        value.compile_as_statement(arena, datapack, ctx);
                    }
                    Self::Sigil(expression) => {
                        TypedExpression::compile_as_statement(expression, arena, datapack, ctx)
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
