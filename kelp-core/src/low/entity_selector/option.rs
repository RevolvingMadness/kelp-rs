use std::collections::HashMap;

use minecraft_command_types::{
    command::enums::{gamemode::Gamemode, sort::Sort},
    entity_selector::{AdvancementChoiceType, EntitySelectorOption as LowEntitySelectorOption},
    range::{FloatRange, IntegerRange},
    resource_location::ResourceLocation,
};
use ordered_float::NotNan;

use crate::{
    compile_context::CompileContext, datapack::Datapack,
    low::expression::unresolved::UnresolvedExpression,
};

#[derive(Debug, Clone)]
pub enum EntitySelectorOption {
    X(NotNan<f32>),
    Y(NotNan<f32>),
    Z(NotNan<f32>),
    Distance(FloatRange),
    DistanceX(NotNan<f32>),
    DistanceY(NotNan<f32>),
    DistanceZ(NotNan<f32>),
    XRotation(FloatRange),
    YRotation(FloatRange),
    Scores(HashMap<String, IntegerRange>),
    Tag(bool, String),
    Team(bool, String),
    Name(bool, String),
    Type(bool, ResourceLocation),
    Predicate(bool, ResourceLocation),
    Nbt(bool, Box<UnresolvedExpression>),
    Gamemode(bool, Gamemode),
    Level(IntegerRange),
    Advancements(HashMap<ResourceLocation, AdvancementChoiceType>),
    Limit(i32),
    Sort(Sort),
}

impl EntitySelectorOption {
    pub fn compile(
        self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> LowEntitySelectorOption {
        match self {
            Self::X(x) => LowEntitySelectorOption::X(x),
            Self::Y(y) => LowEntitySelectorOption::Y(y),
            Self::Z(z) => LowEntitySelectorOption::Z(z),
            Self::Distance(distance) => LowEntitySelectorOption::Distance(distance),
            Self::DistanceX(distance_x) => LowEntitySelectorOption::DistanceX(distance_x),
            Self::DistanceY(distance_y) => LowEntitySelectorOption::DistanceY(distance_y),
            Self::DistanceZ(distance_z) => LowEntitySelectorOption::DistanceZ(distance_z),
            Self::XRotation(x_rotation) => LowEntitySelectorOption::XRotation(x_rotation),
            Self::YRotation(y_rotation) => LowEntitySelectorOption::YRotation(y_rotation),
            Self::Scores(scores) => LowEntitySelectorOption::Scores(scores.into_iter().collect()),
            Self::Tag(inverted, tag) => LowEntitySelectorOption::Tag(inverted, tag),
            Self::Team(inverted, team) => LowEntitySelectorOption::Team(inverted, team),
            Self::Name(inverted, name) => LowEntitySelectorOption::Name(inverted, name),
            Self::Type(inverted, type_) => LowEntitySelectorOption::Type(inverted, type_),
            Self::Predicate(inverted, predicate) => {
                LowEntitySelectorOption::Predicate(inverted, predicate)
            }
            Self::Nbt(inverted, expression) => {
                let expression = expression.kind.resolve(datapack, ctx).as_snbt_macros(ctx);

                LowEntitySelectorOption::Nbt(inverted, expression)
            }
            Self::Gamemode(inverted, gamemode) => {
                LowEntitySelectorOption::Gamemode(inverted, gamemode)
            }
            Self::Level(level) => LowEntitySelectorOption::Level(level),
            Self::Advancements(advancements) => {
                LowEntitySelectorOption::Advancements(advancements.into_iter().collect())
            }
            Self::Limit(limit) => LowEntitySelectorOption::Limit(limit),
            Self::Sort(sort) => LowEntitySelectorOption::Sort(sort),
        }
    }

    pub fn compile_as_statement(self, datapack: &mut Datapack, ctx: &mut CompileContext) {
        match self {
            Self::Nbt(_, expression) => {
                expression.kind.compile_as_statement(datapack, ctx);
            }
            Self::X(_)
            | Self::Y(_)
            | Self::Z(_)
            | Self::Distance(_)
            | Self::DistanceX(_)
            | Self::DistanceY(_)
            | Self::DistanceZ(_)
            | Self::XRotation(_)
            | Self::YRotation(_)
            | Self::Scores(_)
            | Self::Tag(_, _)
            | Self::Team(_, _)
            | Self::Name(_, _)
            | Self::Type(_, _)
            | Self::Predicate(_, _)
            | Self::Gamemode(_, _)
            | Self::Level(_)
            | Self::Advancements(_)
            | Self::Limit(_)
            | Self::Sort(_) => {}
        }
    }
}
