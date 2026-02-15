use minecraft_command_types::{
    item::{ItemPredicate, ItemTest, ItemType, OrGroup},
    resource_location::ResourceLocation,
};
use minecraft_command_types_derive::HasMacro;

use crate::{
    compile_context::CompileContext, datapack::HighDatapack, expression::Expression,
    semantic_analysis_context::SemanticAnalysisContext, trait_ext::OptionIterExt,
};

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum HighItemTest {
    Component(ResourceLocation),
    ComponentMatches(ResourceLocation, Expression),
    Predicate(ResourceLocation, Expression),
}

impl HighItemTest {
    pub fn perform_semantic_analysis(
        &self,
        ctx: &mut SemanticAnalysisContext,
        is_lhs: bool,
    ) -> Option<()> {
        match self {
            HighItemTest::Component(_) => Some(()),
            HighItemTest::ComponentMatches(_, expression) => {
                expression.perform_semantic_analysis(ctx, is_lhs, None)
            }
            HighItemTest::Predicate(_, expression) => {
                expression.perform_semantic_analysis(ctx, is_lhs, None)
            }
        }
    }

    pub fn compile(self, datapack: &mut HighDatapack, ctx: &mut CompileContext) -> ItemTest {
        match self {
            HighItemTest::Component(location) => ItemTest::Component(location),
            HighItemTest::ComponentMatches(location, value) => ItemTest::ComponentMatches(
                location,
                value.resolve(datapack, ctx).as_snbt_macros(ctx),
            ),
            HighItemTest::Predicate(location, value) => {
                ItemTest::Predicate(location, value.resolve(datapack, ctx).as_snbt_macros(ctx))
            }
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub struct HighOrGroup(pub Vec<(bool, HighItemTest)>);

impl HighOrGroup {
    pub fn perform_semantic_analysis(
        &self,
        ctx: &mut SemanticAnalysisContext,
        is_lhs: bool,
    ) -> Option<()> {
        self.0
            .iter()
            .map(|(_, test)| test.perform_semantic_analysis(ctx, is_lhs))
            .all_some()
    }

    pub fn compile(self, datapack: &mut HighDatapack, ctx: &mut CompileContext) -> OrGroup {
        OrGroup(
            self.0
                .into_iter()
                .map(|(negated, test)| (negated, test.compile(datapack, ctx)))
                .collect(),
        )
    }
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub struct HighItemPredicate {
    pub id: ItemType,
    pub or_groups: Vec<HighOrGroup>,
}

impl HighItemPredicate {
    pub fn perform_semantic_analysis(
        &self,
        ctx: &mut SemanticAnalysisContext,
        is_lhs: bool,
    ) -> Option<()> {
        self.or_groups
            .iter()
            .map(|or_group| or_group.perform_semantic_analysis(ctx, is_lhs))
            .all_some()
    }

    pub fn compile(self, datapack: &mut HighDatapack, ctx: &mut CompileContext) -> ItemPredicate {
        ItemPredicate {
            id: self.id,
            or_groups: self
                .or_groups
                .into_iter()
                .map(|or_group| or_group.compile(datapack, ctx))
                .collect(),
        }
    }
}
