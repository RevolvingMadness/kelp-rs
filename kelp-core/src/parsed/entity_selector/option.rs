use std::collections::HashMap;

use minecraft_command_types::{
    command::enums::{gamemode::Gamemode, sort::Sort},
    entity_selector::AdvancementChoiceType,
    range::{FloatRange, IntegerRange},
    resource_location::ResourceLocation,
};
use ordered_float::NotNan;

use crate::{
    parsed::arena::ParsedAstArena,
    parsed::{
        expression::{ParsedExpression, ParsedExpressionId},
        semantic_analysis::SemanticAnalysisContext,
    },
    typed::arena::TypedAstArena,
    typed::entity_selector::option::TypedEntitySelectorOption,
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
    Nbt(bool, ParsedExpressionId),
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
        parsed_arena: &ParsedAstArena,
        typed_arena: &mut TypedAstArena,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<TypedEntitySelectorOption> {
        Some(match self {
            Self::X(x) => TypedEntitySelectorOption::X(x),
            Self::Y(y) => TypedEntitySelectorOption::Y(y),
            Self::Z(z) => TypedEntitySelectorOption::Z(z),
            Self::Distance(distance) => TypedEntitySelectorOption::Distance(distance),
            Self::DistanceX(distance_x) => TypedEntitySelectorOption::DistanceX(distance_x),
            Self::DistanceY(distance_y) => TypedEntitySelectorOption::DistanceY(distance_y),
            Self::DistanceZ(distance_z) => TypedEntitySelectorOption::DistanceZ(distance_z),
            Self::XRotation(x_rotation) => TypedEntitySelectorOption::XRotation(x_rotation),
            Self::YRotation(y_rotation) => TypedEntitySelectorOption::YRotation(y_rotation),
            Self::Scores(scores) => TypedEntitySelectorOption::Scores(scores),
            Self::Tag(inverted, tag_name) => TypedEntitySelectorOption::Tag(inverted, tag_name),
            Self::Team(inverted, team_name) => TypedEntitySelectorOption::Team(inverted, team_name),
            Self::Name(inverted, name) => TypedEntitySelectorOption::Name(inverted, name),
            Self::Type(inverted, resource_location) => {
                TypedEntitySelectorOption::Type(inverted, resource_location)
            }
            Self::Predicate(inverted, resource_location) => {
                TypedEntitySelectorOption::Predicate(inverted, resource_location)
            }
            Self::Nbt(inverted, expression) => {
                let expression = ParsedExpression::perform_semantic_analysis(
                    expression,
                    parsed_arena,
                    typed_arena,
                    ctx,
                )?;

                TypedEntitySelectorOption::Nbt(inverted, expression)
            }
            Self::Gamemode(inverted, gamemode) => {
                TypedEntitySelectorOption::Gamemode(inverted, gamemode)
            }
            Self::Level(level) => TypedEntitySelectorOption::Level(level),
            Self::Advancements(advancements) => {
                TypedEntitySelectorOption::Advancements(advancements)
            }
            Self::Limit(limit) => TypedEntitySelectorOption::Limit(limit),
            Self::Sort(sort) => TypedEntitySelectorOption::Sort(sort),
        })
    }
}
