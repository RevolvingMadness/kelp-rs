use crate::{compile_context::CompileContext, datapack::Datapack, low::expression::unresolved::UnresolvedExpression};
use minecraft_command_types::coordinate::{
    Coordinates as LowCoordinates, WorldCoordinate as LowWorldCoordinate,
};

#[derive(Debug, Clone)]
pub struct WorldCoordinate {
    pub relative: bool,
    pub value: Option<UnresolvedExpression>,
}

impl WorldCoordinate {
    pub fn compile(self, datapack: &mut Datapack, ctx: &mut CompileContext) -> LowWorldCoordinate {
        let value = self.value.map(|value| {
            value
                .kind
                .resolve(datapack, ctx)
                .as_snbt_double(ctx)
                .unwrap()
        });

        LowWorldCoordinate {
            relative: self.relative,
            value,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Coordinates {
    World(WorldCoordinate, WorldCoordinate, WorldCoordinate),
    Local(Option<UnresolvedExpression>, Option<UnresolvedExpression>, Option<UnresolvedExpression>),
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
                let x = x.map(|x| x.kind.resolve(datapack, ctx).as_snbt_double(ctx).unwrap());
                let y = y.map(|y| y.kind.resolve(datapack, ctx).as_snbt_double(ctx).unwrap());
                let z = z.map(|z| z.kind.resolve(datapack, ctx).as_snbt_double(ctx).unwrap());

                LowCoordinates::Local(x, y, z)
            }
        }
    }
}
