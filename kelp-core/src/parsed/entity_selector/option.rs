use std::collections::HashMap;

use minecraft_command_types::{
    command::enums::{gamemode::Gamemode, sort::Sort},
    entity_selector::AdvancementChoiceType,
    range::{FloatRange, IntegerRange},
    resource_location::ResourceLocation,
};
use ordered_float::NotNan;

use crate::{
    parsed::{expression::ParsedExpression, semantic_analysis::SemanticAnalysisContext},
    semantic::entity_selector::option::SemanticEntitySelectorOption,
};

#[derive(Debug, Clone)]
pub enum ParsedEntitySelectorOption {
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
    Nbt(bool, Box<ParsedExpression>),
    Gamemode(bool, Gamemode),
    Level(IntegerRange),
    Advancements(HashMap<ResourceLocation, AdvancementChoiceType>),
    Limit(i32),
    Sort(Sort),
}

impl ParsedEntitySelectorOption {
    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<SemanticEntitySelectorOption> {
        Some(match self {
            Self::X(x) => SemanticEntitySelectorOption::X(x),
            Self::Y(y) => SemanticEntitySelectorOption::Y(y),
            Self::Z(z) => SemanticEntitySelectorOption::Z(z),
            Self::Distance(distance) => SemanticEntitySelectorOption::Distance(distance),
            Self::DistanceX(distance_x) => SemanticEntitySelectorOption::DistanceX(distance_x),
            Self::DistanceY(distance_y) => SemanticEntitySelectorOption::DistanceY(distance_y),
            Self::DistanceZ(distance_z) => SemanticEntitySelectorOption::DistanceZ(distance_z),
            Self::XRotation(x_rotation) => SemanticEntitySelectorOption::XRotation(x_rotation),
            Self::YRotation(y_rotation) => SemanticEntitySelectorOption::YRotation(y_rotation),
            Self::Scores(scores) => SemanticEntitySelectorOption::Scores(scores),
            Self::Tag(inverted, tag_name) => SemanticEntitySelectorOption::Tag(inverted, tag_name),
            Self::Team(inverted, team_name) => {
                SemanticEntitySelectorOption::Team(inverted, team_name)
            }
            Self::Name(inverted, name) => SemanticEntitySelectorOption::Name(inverted, name),
            Self::Type(inverted, resource_location) => {
                SemanticEntitySelectorOption::Type(inverted, resource_location)
            }
            Self::Predicate(inverted, resource_location) => {
                SemanticEntitySelectorOption::Predicate(inverted, resource_location)
            }
            Self::Nbt(inverted, expression) => {
                let (_, expression) = expression.perform_semantic_analysis(ctx)?;

                SemanticEntitySelectorOption::Nbt(inverted, Box::new(expression))
            }
            Self::Gamemode(inverted, gamemode) => {
                SemanticEntitySelectorOption::Gamemode(inverted, gamemode)
            }
            Self::Level(level) => SemanticEntitySelectorOption::Level(level),
            Self::Advancements(advancements) => {
                SemanticEntitySelectorOption::Advancements(advancements)
            }
            Self::Limit(limit) => SemanticEntitySelectorOption::Limit(limit),
            Self::Sort(sort) => SemanticEntitySelectorOption::Sort(sort),
        })
    }
}
