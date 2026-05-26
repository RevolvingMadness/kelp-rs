use minecraft_command_types::{item::ItemType, resource_location::ResourceLocation};

use crate::{
    parsed::{expression::ParsedExpression, semantic_analysis::SemanticAnalysisContext},
    semantic::mc_item::{SemanticItemPredicate, SemanticItemTest, SemanticOrGroup},
    trait_ext::CollectOptionAllIterExt,
};

#[derive(Debug, Clone)]
pub enum ItemTest {
    Component(ResourceLocation),
    ComponentMatches(ResourceLocation, ParsedExpression),
    Predicate(ResourceLocation, ParsedExpression),
}

impl ItemTest {
    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<SemanticItemTest> {
        Some(match self {
            Self::Component(resource_location) => SemanticItemTest::Component(resource_location),
            Self::ComponentMatches(resource_location, expression) => {
                let (_, expression) = expression.perform_semantic_analysis(ctx)?;

                SemanticItemTest::ComponentMatches(resource_location, expression)
            }
            Self::Predicate(resource_location, expression) => {
                let (_, expression) = expression.perform_semantic_analysis(ctx)?;

                SemanticItemTest::Predicate(resource_location, expression)
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
    ) -> Option<SemanticOrGroup> {
        let tests = self
            .0
            .into_iter()
            .map(|(inverted, test)| {
                let test = test.perform_semantic_analysis(ctx)?;

                Some((inverted, test))
            })
            .collect_option_all()?;

        Some(SemanticOrGroup(tests))
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
    ) -> Option<SemanticItemPredicate> {
        let or_groups = self
            .or_groups
            .into_iter()
            .map(|or_group| or_group.perform_semantic_analysis(ctx))
            .collect_option_all()?;

        Some(SemanticItemPredicate {
            id: self.id,
            or_groups,
        })
    }
}
