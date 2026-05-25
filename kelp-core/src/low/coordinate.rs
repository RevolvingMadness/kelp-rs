use crate::{
    ast_allocator::low::LowAstAllocator,
    compile_context::CompileContext,
    datapack::Datapack,
    low::expression::unresolved::{UnresolvedExpression, UnresolvedExpressionId},
};

use minecraft_command_types::coordinate::{
    Coordinates as LowCoordinates, WorldCoordinate as LowWorldCoordinate,
};

#[derive(Debug, Clone, Copy)]
pub enum WorldCoordinate {
    Relative(Option<UnresolvedExpressionId>),
    Absolute(UnresolvedExpressionId),
}

impl WorldCoordinate {
    pub fn compile(
        self,
        allocator: &LowAstAllocator,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> LowWorldCoordinate {
        match self {
            Self::Relative(expression) => {
                let expression = expression.map(|expression| {
                    UnresolvedExpression::resolve(expression, allocator, datapack, ctx)
                        .as_snbt_double(ctx)
                        .unwrap()
                });

                LowWorldCoordinate::Relative(expression)
            }
            Self::Absolute(expression) => {
                let expression =
                    UnresolvedExpression::resolve(expression, allocator, datapack, ctx)
                        .as_snbt_double(ctx)
                        .unwrap();

                LowWorldCoordinate::Absolute(expression)
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
            Self::Relative(expression) => {
                if let Some(expression) = expression {
                    UnresolvedExpression::compile_as_statement(
                        expression, allocator, datapack, ctx,
                    );
                }
            }
            Self::Absolute(expression) => {
                UnresolvedExpression::compile_as_statement(expression, allocator, datapack, ctx);
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Coordinates {
    World(WorldCoordinate, WorldCoordinate, WorldCoordinate),
    Local(
        Option<UnresolvedExpressionId>,
        Option<UnresolvedExpressionId>,
        Option<UnresolvedExpressionId>,
    ),
}

impl Coordinates {
    pub fn compile(
        self,
        allocator: &LowAstAllocator,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> LowCoordinates {
        match self {
            Self::World(x, y, z) => {
                let x = x.compile(allocator, datapack, ctx);
                let y = y.compile(allocator, datapack, ctx);
                let z = z.compile(allocator, datapack, ctx);

                LowCoordinates::World(x, y, z)
            }
            Self::Local(x, y, z) => {
                let x = x.map(|x| {
                    UnresolvedExpression::resolve(x, allocator, datapack, ctx)
                        .as_snbt_double(ctx)
                        .unwrap()
                });
                let y = y.map(|y| {
                    UnresolvedExpression::resolve(y, allocator, datapack, ctx)
                        .as_snbt_double(ctx)
                        .unwrap()
                });
                let z = z.map(|z| {
                    UnresolvedExpression::resolve(z, allocator, datapack, ctx)
                        .as_snbt_double(ctx)
                        .unwrap()
                });

                LowCoordinates::Local(x, y, z)
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
            Self::World(x, y, z) => {
                x.compile_as_statement(allocator, datapack, ctx);
                y.compile_as_statement(allocator, datapack, ctx);
                z.compile_as_statement(allocator, datapack, ctx);
            }
            Self::Local(x, y, z) => {
                if let Some(x) = x {
                    UnresolvedExpression::compile_as_statement(x, allocator, datapack, ctx);
                }

                if let Some(y) = y {
                    UnresolvedExpression::compile_as_statement(y, allocator, datapack, ctx);
                }

                if let Some(z) = z {
                    UnresolvedExpression::compile_as_statement(z, allocator, datapack, ctx);
                }
            }
        }
    }
}
