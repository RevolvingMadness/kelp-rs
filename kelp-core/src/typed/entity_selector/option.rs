use std::collections::HashMap;

use minecraft_command_types::{
    command::enums::{gamemode::Gamemode, sort::Sort},
    entity_selector::{AdvancementChoiceType, EntitySelectorOption},
    range::{FloatRange, IntegerRange},
    resource_location::ResourceLocation,
};
use ordered_float::NotNan;

use crate::{
    ast_allocator::low::LowAstAllocator,
    compile_context::CompileContext,
    datapack::Datapack,
    typed::expression::{TypedExpression, TypedExpressionId},
};

#[derive(Debug, Clone)]
pub enum TypedEntitySelectorOption {
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
    Nbt(bool, TypedExpressionId),
    Gamemode(bool, Gamemode),
    Level(IntegerRange),
    Advancements(HashMap<ResourceLocation, AdvancementChoiceType>),
    Limit(i32),
    Sort(Sort),
}

impl TypedEntitySelectorOption {
    pub fn compile(
        self,
        allocator: &LowAstAllocator,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> EntitySelectorOption {
        match self {
            Self::X(x) => EntitySelectorOption::X(x),
            Self::Y(y) => EntitySelectorOption::Y(y),
            Self::Z(z) => EntitySelectorOption::Z(z),
            Self::Distance(distance) => EntitySelectorOption::Distance(distance),
            Self::DistanceX(distance_x) => EntitySelectorOption::DistanceX(distance_x),
            Self::DistanceY(distance_y) => EntitySelectorOption::DistanceY(distance_y),
            Self::DistanceZ(distance_z) => EntitySelectorOption::DistanceZ(distance_z),
            Self::XRotation(x_rotation) => EntitySelectorOption::XRotation(x_rotation),
            Self::YRotation(y_rotation) => EntitySelectorOption::YRotation(y_rotation),
            Self::Scores(scores) => EntitySelectorOption::Scores(scores.into_iter().collect()),
            Self::Tag(inverted, tag) => EntitySelectorOption::Tag(inverted, tag),
            Self::Team(inverted, team) => EntitySelectorOption::Team(inverted, team),
            Self::Name(inverted, name) => EntitySelectorOption::Name(inverted, name),
            Self::Type(inverted, type_) => EntitySelectorOption::Type(inverted, type_),
            Self::Predicate(inverted, predicate) => {
                EntitySelectorOption::Predicate(inverted, predicate)
            }
            Self::Nbt(inverted, expression) => {
                let expression = TypedExpression::resolve(expression, allocator, datapack, ctx);
                let expression = expression.as_snbt_macros(ctx);

                EntitySelectorOption::Nbt(inverted, expression)
            }
            Self::Gamemode(inverted, gamemode) => {
                EntitySelectorOption::Gamemode(inverted, gamemode)
            }
            Self::Level(level) => EntitySelectorOption::Level(level),
            Self::Advancements(advancements) => {
                EntitySelectorOption::Advancements(advancements.into_iter().collect())
            }
            Self::Limit(limit) => EntitySelectorOption::Limit(limit),
            Self::Sort(sort) => EntitySelectorOption::Sort(sort),
        }
    }

    pub fn compile_as_statement(
        self,
        allocator: &LowAstAllocator,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) {
        match self {
            Self::Nbt(_, expression) => {
                TypedExpression::compile_as_statement(expression, allocator, datapack, ctx);
            }
            Self::X(..)
            | Self::Y(..)
            | Self::Z(..)
            | Self::Distance(..)
            | Self::DistanceX(..)
            | Self::DistanceY(..)
            | Self::DistanceZ(..)
            | Self::XRotation(..)
            | Self::YRotation(..)
            | Self::Scores(..)
            | Self::Tag(..)
            | Self::Team(..)
            | Self::Name(..)
            | Self::Type(..)
            | Self::Predicate(..)
            | Self::Gamemode(..)
            | Self::Level(..)
            | Self::Advancements(..)
            | Self::Limit(..)
            | Self::Sort(..) => {}
        }
    }
}
