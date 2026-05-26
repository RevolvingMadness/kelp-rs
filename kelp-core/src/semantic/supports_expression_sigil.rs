use std::fmt::Debug;

use minecraft_command_types::{
    coordinate::Coordinates, entity_selector::EntitySelector, resource_location::ResourceLocation,
};

use crate::low::expression::Expression;
use crate::{
    compile_context::CompileContext,
    datapack::Datapack,
    semantic::{
        coordinate::SemanticCoordinates, entity_selector::SemanticEntitySelector,
        expression::SemanticExpression,
    },
};

#[derive(Debug, Clone)]
pub enum SemanticSupportsExpressionSigil<T> {
    Regular(T),
    Sigil(SemanticExpression),
}

impl<T> SemanticSupportsExpressionSigil<T> {
    #[must_use]
    pub fn map<R>(self, f: impl FnOnce(T) -> R) -> SemanticSupportsExpressionSigil<R> {
        match self {
            Self::Regular(value) => SemanticSupportsExpressionSigil::Regular(f(value)),
            Self::Sigil(expression) => SemanticSupportsExpressionSigil::Sigil(expression),
        }
    }
}

macro_rules! impl_supports_expression_sigil {
    (no_compile, $ty:ty, $resolved_expression_variant:ident) => {
        impl SemanticSupportsExpressionSigil<$ty> {
            #[must_use]
            pub fn compile(self, datapack: &mut Datapack, ctx: &mut CompileContext) -> $ty {
                match self {
                    Self::Regular(value) => value,
                    Self::Sigil(expression) => {
                        let expression = expression.kind.resolve(datapack, ctx);

                        let Expression::$resolved_expression_variant(value) = expression else {
                            unreachable!();
                        };

                        value
                    }
                }
            }

            pub fn compile_as_statement(self, datapack: &mut Datapack, ctx: &mut CompileContext) {
                match self {
                    Self::Regular(..) => {}
                    Self::Sigil(expression) => {
                        expression.kind.compile_as_statement(datapack, ctx);
                    }
                }
            }
        }
    };

    ($ty:ty, $ret_ty:ty, $resolved_expression_variant:ident) => {
        impl SemanticSupportsExpressionSigil<$ty> {
            #[must_use]
            pub fn compile(self, datapack: &mut Datapack, ctx: &mut CompileContext) -> $ret_ty {
                match self {
                    Self::Regular(value) => value.compile(datapack, ctx),
                    Self::Sigil(expression) => {
                        let expression = expression.kind.resolve(datapack, ctx);

                        match expression {
                            Expression::$resolved_expression_variant(value) => value,
                            _ => unreachable!(),
                        }
                    }
                }
            }

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
}

impl_supports_expression_sigil!(no_compile, ResourceLocation, ResourceLocation);

impl_supports_expression_sigil!(SemanticEntitySelector, EntitySelector, EntitySelector);

impl_supports_expression_sigil!(SemanticCoordinates, Coordinates, Coordinates);

impl_supports_expression_sigil!(Box<SemanticCoordinates>, Coordinates, Coordinates);
