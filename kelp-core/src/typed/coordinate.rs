use crate::{
    compile_context::CompileContext,
    datapack::Datapack,
    typed::arena::TypedAstArena,
    typed::expression::{TypedExpression, TypedExpressionId},
};

use minecraft_command_types::coordinate::{Coordinates, WorldCoordinate};

#[derive(Debug, Clone, Copy)]
pub enum TypedWorldCoordinate {
    Relative(Option<TypedExpressionId>),
    Absolute(TypedExpressionId),
}

impl TypedWorldCoordinate {
    pub fn compile(
        self,
        arena: &TypedAstArena,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> WorldCoordinate {
        match self {
            Self::Relative(expression) => {
                let expression = expression.map(|expression| {
                    TypedExpression::resolve(expression, arena, datapack, ctx)
                        .as_snbt_double(ctx)
                        .unwrap()
                });

                WorldCoordinate::Relative(expression)
            }
            Self::Absolute(expression) => {
                let expression = TypedExpression::resolve(expression, arena, datapack, ctx)
                    .as_snbt_double(ctx)
                    .unwrap();

                WorldCoordinate::Absolute(expression)
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
            Self::Relative(expression) => {
                if let Some(expression) = expression {
                    TypedExpression::compile_as_statement(expression, arena, datapack, ctx);
                }
            }
            Self::Absolute(expression) => {
                TypedExpression::compile_as_statement(expression, arena, datapack, ctx);
            }
        }
    }
}

pub type TypedLocalCoordinate = Option<TypedExpressionId>;

#[derive(Debug, Clone, Copy)]
pub enum TypedCoordinates {
    World(
        TypedWorldCoordinate,
        TypedWorldCoordinate,
        TypedWorldCoordinate,
    ),
    Local(
        TypedLocalCoordinate,
        TypedLocalCoordinate,
        TypedLocalCoordinate,
    ),
}

impl TypedCoordinates {
    pub fn compile(
        self,
        arena: &TypedAstArena,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> Coordinates {
        match self {
            Self::World(x, y, z) => {
                let x = x.compile(arena, datapack, ctx);
                let y = y.compile(arena, datapack, ctx);
                let z = z.compile(arena, datapack, ctx);

                Coordinates::World(x, y, z)
            }
            Self::Local(x, y, z) => {
                let x = x.map(|x| {
                    TypedExpression::resolve(x, arena, datapack, ctx)
                        .as_snbt_double(ctx)
                        .unwrap()
                });
                let y = y.map(|y| {
                    TypedExpression::resolve(y, arena, datapack, ctx)
                        .as_snbt_double(ctx)
                        .unwrap()
                });
                let z = z.map(|z| {
                    TypedExpression::resolve(z, arena, datapack, ctx)
                        .as_snbt_double(ctx)
                        .unwrap()
                });

                Coordinates::Local(x, y, z)
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
            Self::World(x, y, z) => {
                x.compile_as_statement(arena, datapack, ctx);
                y.compile_as_statement(arena, datapack, ctx);
                z.compile_as_statement(arena, datapack, ctx);
            }
            Self::Local(x, y, z) => {
                if let Some(x) = x {
                    TypedExpression::compile_as_statement(x, arena, datapack, ctx);
                }

                if let Some(y) = y {
                    TypedExpression::compile_as_statement(y, arena, datapack, ctx);
                }

                if let Some(z) = z {
                    TypedExpression::compile_as_statement(z, arena, datapack, ctx);
                }
            }
        }
    }
}
