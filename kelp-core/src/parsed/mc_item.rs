use minecraft_command_types::{item::ItemType, resource_location::ResourceLocation};

use crate::{
    parsed::arena::ParsedAstArena,
    parsed::{
        expression::{ParsedExpression, ParsedExpressionId},
        semantic_analysis::SemanticAnalysisContext,
    },
    trait_ext::CollectOptionAllIterExt,
    typed::arena::TypedAstArena,
    typed::mc_item::{TypedItemPredicate, TypedItemTest, TypedOrGroup},
};

#[derive(Debug, Clone)]
pub enum ItemTest {
    Component(ResourceLocation),
    ComponentMatches(ResourceLocation, ParsedExpressionId),
    Predicate(ResourceLocation, ParsedExpressionId),
}

impl ItemTest {
    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        parsed_arena: &ParsedAstArena,
        typed_arena: &mut TypedAstArena,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<TypedItemTest> {
        Some(match self {
            Self::Component(resource_location) => TypedItemTest::Component(resource_location),
            Self::ComponentMatches(resource_location, expression) => {
                let expression = ParsedExpression::perform_semantic_analysis(
                    expression,
                    parsed_arena,
                    typed_arena,
                    ctx,
                )?;

                TypedItemTest::ComponentMatches(resource_location, expression)
            }
            Self::Predicate(resource_location, expression) => {
                let expression = ParsedExpression::perform_semantic_analysis(
                    expression,
                    parsed_arena,
                    typed_arena,
                    ctx,
                )?;

                TypedItemTest::Predicate(resource_location, expression)
            }
        })
    }
}

#[derive(Debug, Clone)]
pub struct OrGroup(pub Vec<(bool, ItemTest)>);

impl OrGroup {
    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        parsed_arena: &ParsedAstArena,
        typed_arena: &mut TypedAstArena,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<TypedOrGroup> {
        let tests = self
            .0
            .into_iter()
            .map(|(inverted, test)| {
                let test = test.perform_semantic_analysis(parsed_arena, typed_arena, ctx)?;

                Some((inverted, test))
            })
            .collect_option_all()?;

        Some(TypedOrGroup(tests))
    }
}

#[derive(Debug, Clone)]
pub struct ItemPredicate {
    pub id: ItemType,
    pub or_groups: Vec<OrGroup>,
}

impl ItemPredicate {
    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        parsed_arena: &ParsedAstArena,
        typed_arena: &mut TypedAstArena,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<TypedItemPredicate> {
        let or_groups = self
            .or_groups
            .into_iter()
            .map(|or_group| or_group.perform_semantic_analysis(parsed_arena, typed_arena, ctx))
            .collect_option_all()?;

        Some(TypedItemPredicate {
            id: self.id,
            or_groups,
        })
    }
}
