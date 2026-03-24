use minecraft_command_types::{item::ItemType, resource_location::ResourceLocation};

use crate::{
    high::{expression::Expression, semantic_analysis_context::SemanticAnalysisContext},
    low::mc_item::{
        ItemPredicate as MiddleItemPredicate, ItemTest as MiddleItemTest, OrGroup as MiddleOrGroup,
    },
    trait_ext::CollectOptionAllIterExt,
};

#[derive(Debug, Clone)]
pub enum ItemTest {
    Component(ResourceLocation),
    ComponentMatches(ResourceLocation, Expression),
    Predicate(ResourceLocation, Expression),
}

impl ItemTest {
    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<MiddleItemTest> {
        Some(match self {
            Self::Component(resource_location) => MiddleItemTest::Component(resource_location),
            Self::ComponentMatches(resource_location, expression) => {
                let (_, expression) = expression.perform_semantic_analysis(ctx)?;

                MiddleItemTest::ComponentMatches(resource_location, expression)
            }
            Self::Predicate(resource_location, expression) => {
                let (_, expression) = expression.perform_semantic_analysis(ctx)?;

                MiddleItemTest::Predicate(resource_location, expression)
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
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<MiddleOrGroup> {
        let tests = self
            .0
            .into_iter()
            .map(|(inverted, test)| {
                let test = test.perform_semantic_analysis(ctx)?;

                Some((inverted, test))
            })
            .collect_option_all()?;

        Some(MiddleOrGroup(tests))
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
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<MiddleItemPredicate> {
        let or_groups = self
            .or_groups
            .into_iter()
            .map(|or_group| or_group.perform_semantic_analysis(ctx))
            .collect_option_all()?;

        Some(MiddleItemPredicate {
            id: self.id,
            or_groups,
        })
    }
}
