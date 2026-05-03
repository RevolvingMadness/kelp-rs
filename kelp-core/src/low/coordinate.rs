use crate::{
    compile_context::CompileContext, datapack::Datapack,
    low::expression::unresolved::UnresolvedExpression,
};
use minecraft_command_types::coordinate::{
    Coordinates as LowCoordinates, WorldCoordinate as LowWorldCoordinate,
};

#[derive(Debug, Clone)]
pub enum WorldCoordinate {
    Relative(Option<UnresolvedExpression>),
    Absolute(UnresolvedExpression),
}

impl WorldCoordinate {
    pub fn compile(self, datapack: &mut Datapack, ctx: &mut CompileContext) -> LowWorldCoordinate {
        match self {
            Self::Relative(expression) => {
                let expression = expression.map(|expression| {
                    expression
                        .kind
                        .resolve(datapack, ctx)
                        .as_snbt_double(datapack, ctx)
                        .unwrap()
                });

                LowWorldCoordinate::Relative(expression)
            }
            Self::Absolute(expression) => {
                let expression = expression
                    .kind
                    .resolve(datapack, ctx)
                    .as_snbt_double(datapack, ctx)
                    .unwrap();

                LowWorldCoordinate::Absolute(expression)
            }
        }
    }

    pub fn compile_as_statement(self, datapack: &mut Datapack, ctx: &mut CompileContext) {
        match self {
            Self::Relative(expression) => {
                if let Some(expression) = expression {
                    expression.kind.compile_as_statement(datapack, ctx);
                }
            }
            Self::Absolute(expression) => {
                expression.kind.compile_as_statement(datapack, ctx);
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum Coordinates {
    World(WorldCoordinate, WorldCoordinate, WorldCoordinate),
    Local(
        Option<UnresolvedExpression>,
        Option<UnresolvedExpression>,
        Option<UnresolvedExpression>,
    ),
}

impl Coordinates {
    pub fn compile(self, datapack: &mut Datapack, ctx: &mut CompileContext) -> LowCoordinates {
        match self {
            Self::World(x, y, z) => {
                let x = x.compile(datapack, ctx);
                let y = y.compile(datapack, ctx);
                let z = z.compile(datapack, ctx);

                LowCoordinates::World(x, y, z)
            }
            Self::Local(x, y, z) => {
                let x = x.map(|x| {
                    x.kind
                        .resolve(datapack, ctx)
                        .as_snbt_double(datapack, ctx)
                        .unwrap()
                });
                let y = y.map(|y| {
                    y.kind
                        .resolve(datapack, ctx)
                        .as_snbt_double(datapack, ctx)
                        .unwrap()
                });
                let z = z.map(|z| {
                    z.kind
                        .resolve(datapack, ctx)
                        .as_snbt_double(datapack, ctx)
                        .unwrap()
                });

                LowCoordinates::Local(x, y, z)
            }
        }
    }

    pub fn compile_as_statement(self, datapack: &mut Datapack, ctx: &mut CompileContext) {
        match self {
            Self::World(x, y, z) => {
                x.compile_as_statement(datapack, ctx);
                y.compile_as_statement(datapack, ctx);
                z.compile_as_statement(datapack, ctx);
            }
            Self::Local(x, y, z) => {
                if let Some(x) = x {
                    x.kind.compile_as_statement(datapack, ctx);
                }

                if let Some(y) = y {
                    y.kind.compile_as_statement(datapack, ctx);
                }

                if let Some(z) = z {
                    z.kind.compile_as_statement(datapack, ctx);
                }
            }
        }
    }
}
