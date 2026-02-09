use std::collections::BTreeMap;

use minecraft_command_types::{
    command::enums::{gamemode::Gamemode, sort::Sort},
    entity_selector::{AdvancementChoiceType, EntitySelectorOption},
    range::{FloatRange, IntegerRange},
    resource_location::ResourceLocation,
};
use minecraft_command_types_derive::HasMacro;
use ordered_float::NotNan;

use crate::{
    compile_context::CompileContext, datapack::HighDatapack, expression::Expression,
    semantic_analysis_context::SemanticAnalysisContext,
};

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum HighEntitySelectorOption {
    X(NotNan<f32>),
    Y(NotNan<f32>),
    Z(NotNan<f32>),
    Distance(FloatRange),
    DistanceX(NotNan<f32>),
    DistanceY(NotNan<f32>),
    DistanceZ(NotNan<f32>),
    XRotation(FloatRange),
    YRotation(FloatRange),
    Scores(BTreeMap<String, IntegerRange>),
    Tag(bool, String),
    Team(bool, String),
    Name(bool, String),
    Type(bool, ResourceLocation),
    Predicate(bool, ResourceLocation),
    Nbt(bool, Box<Expression>),
    Gamemode(bool, Gamemode),
    Level(IntegerRange),
    Advancements(BTreeMap<ResourceLocation, AdvancementChoiceType>),
    Limit(i32),
    Sort(Sort),
}

impl HighEntitySelectorOption {
    pub fn perform_semantic_analysis(
        &self,
        ctx: &mut SemanticAnalysisContext,
        is_lhs: bool,
    ) -> Option<()> {
        match self {
            HighEntitySelectorOption::Nbt(_, expression) => {
                expression.perform_semantic_analysis(ctx, is_lhs)
            }
            _ => Some(()),
        }
    }

    pub fn compile(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
    ) -> EntitySelectorOption {
        match self {
            HighEntitySelectorOption::X(x) => EntitySelectorOption::X(x),
            HighEntitySelectorOption::Y(y) => EntitySelectorOption::Y(y),
            HighEntitySelectorOption::Z(z) => EntitySelectorOption::Z(z),
            HighEntitySelectorOption::Distance(distance) => {
                EntitySelectorOption::Distance(distance)
            }
            HighEntitySelectorOption::DistanceX(distance_x) => {
                EntitySelectorOption::DistanceX(distance_x)
            }
            HighEntitySelectorOption::DistanceY(distance_y) => {
                EntitySelectorOption::DistanceY(distance_y)
            }
            HighEntitySelectorOption::DistanceZ(distance_z) => {
                EntitySelectorOption::DistanceZ(distance_z)
            }
            HighEntitySelectorOption::XRotation(x_rotation) => {
                EntitySelectorOption::XRotation(x_rotation)
            }
            HighEntitySelectorOption::YRotation(y_rotation) => {
                EntitySelectorOption::YRotation(y_rotation)
            }
            HighEntitySelectorOption::Scores(scores) => {
                EntitySelectorOption::Scores(scores.into_iter().collect())
            }
            HighEntitySelectorOption::Tag(inverted, tag) => {
                EntitySelectorOption::Tag(inverted, tag)
            }
            HighEntitySelectorOption::Team(inverted, team) => {
                EntitySelectorOption::Team(inverted, team)
            }
            HighEntitySelectorOption::Name(inverted, name) => {
                EntitySelectorOption::Name(inverted, name)
            }
            HighEntitySelectorOption::Type(inverted, type_) => {
                EntitySelectorOption::Type(inverted, type_)
            }
            HighEntitySelectorOption::Predicate(inverted, predicate) => {
                EntitySelectorOption::Predicate(inverted, predicate)
            }
            HighEntitySelectorOption::Nbt(inverted, expression) => {
                let expression = expression.resolve(datapack, ctx).as_snbt_macros(ctx);

                EntitySelectorOption::Nbt(inverted, expression)
            }
            HighEntitySelectorOption::Gamemode(inverted, gamemode) => {
                EntitySelectorOption::Gamemode(inverted, gamemode)
            }
            HighEntitySelectorOption::Level(level) => EntitySelectorOption::Level(level),
            HighEntitySelectorOption::Advancements(advancements) => {
                EntitySelectorOption::Advancements(advancements.into_iter().collect())
            }
            HighEntitySelectorOption::Limit(limit) => EntitySelectorOption::Limit(limit),
            HighEntitySelectorOption::Sort(sort) => EntitySelectorOption::Sort(sort),
        }
    }
}
