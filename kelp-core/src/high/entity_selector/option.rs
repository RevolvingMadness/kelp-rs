use std::collections::HashMap;

use minecraft_command_types::{
    command::enums::{gamemode::Gamemode, sort::Sort},
    entity_selector::AdvancementChoiceType,
    range::{FloatRange, IntegerRange},
    resource_location::ResourceLocation,
};
use ordered_float::NotNan;

use crate::{
    high::{expression::Expression, semantic_analysis_context::SemanticAnalysisContext},
    middle::entity_selector::option::EntitySelectorOption as MiddleEntitySelectorOption,
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
    Nbt(bool, Box<Expression>),
    Gamemode(bool, Gamemode),
    Level(IntegerRange),
    Advancements(HashMap<ResourceLocation, AdvancementChoiceType>),
    Limit(i32),
    Sort(Sort),
}

impl EntitySelectorOption {
    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<MiddleEntitySelectorOption> {
        Some(match self {
            Self::X(x) => MiddleEntitySelectorOption::X(x),
            Self::Y(y) => MiddleEntitySelectorOption::Y(y),
            Self::Z(z) => MiddleEntitySelectorOption::Z(z),
            Self::Distance(distance) => MiddleEntitySelectorOption::Distance(distance),
            Self::DistanceX(distance_x) => MiddleEntitySelectorOption::DistanceX(distance_x),
            Self::DistanceY(distance_y) => MiddleEntitySelectorOption::DistanceY(distance_y),
            Self::DistanceZ(distance_z) => MiddleEntitySelectorOption::DistanceZ(distance_z),
            Self::XRotation(x_rotation) => MiddleEntitySelectorOption::XRotation(x_rotation),
            Self::YRotation(y_rotation) => MiddleEntitySelectorOption::YRotation(y_rotation),
            Self::Scores(scores) => MiddleEntitySelectorOption::Scores(scores),
            Self::Tag(inverted, tag_name) => MiddleEntitySelectorOption::Tag(inverted, tag_name),
            Self::Team(inverted, team_name) => {
                MiddleEntitySelectorOption::Team(inverted, team_name)
            }
            Self::Name(inverted, name) => MiddleEntitySelectorOption::Name(inverted, name),
            Self::Type(inverted, resource_location) => {
                MiddleEntitySelectorOption::Type(inverted, resource_location)
            }
            Self::Predicate(inverted, resource_location) => {
                MiddleEntitySelectorOption::Predicate(inverted, resource_location)
            }
            Self::Nbt(inverted, expression) => {
                let (_, expression) = expression.perform_semantic_analysis(ctx)?;

                MiddleEntitySelectorOption::Nbt(inverted, Box::new(expression))
            }
            Self::Gamemode(inverted, gamemode) => {
                MiddleEntitySelectorOption::Gamemode(inverted, gamemode)
            }
            Self::Level(level) => MiddleEntitySelectorOption::Level(level),
            Self::Advancements(advancements) => {
                MiddleEntitySelectorOption::Advancements(advancements)
            }
            Self::Limit(limit) => MiddleEntitySelectorOption::Limit(limit),
            Self::Sort(sort) => MiddleEntitySelectorOption::Sort(sort),
        })
    }
}
