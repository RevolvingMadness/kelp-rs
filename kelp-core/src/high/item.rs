use minecraft_command_types::{
    item::{
        ItemPredicate as LowItemPredicate, ItemTest as LowItemTest, ItemType, OrGroup as LowOrGroup,
    },
    resource_location::ResourceLocation,
};
use minecraft_command_types_derive::HasMacro;

use crate::{
    compile_context::CompileContext, datapack::Datapack, high::expression::Expression,
    semantic_analysis_context::SemanticAnalysisContext, trait_ext::OptionUnitIterExt,
};

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum ItemTest {
    Component(ResourceLocation),
    ComponentMatches(ResourceLocation, Expression),
    Predicate(ResourceLocation, Expression),
}

impl ItemTest {
    pub fn perform_semantic_analysis(
        &self,
        ctx: &mut SemanticAnalysisContext,
        is_lhs: bool,
    ) -> Option<()> {
        match self {
            Self::Component(_) => Some(()),
            Self::ComponentMatches(_, expression) | Self::Predicate(_, expression) => {
                expression.perform_semantic_analysis(ctx, is_lhs, None)
            }
        }
    }

    pub fn compile(self, datapack: &mut Datapack, ctx: &mut CompileContext) -> LowItemTest {
        match self {
            Self::Component(location) => LowItemTest::Component(location),
            Self::ComponentMatches(location, value) => LowItemTest::ComponentMatches(
                location,
                value.kind.resolve(datapack, ctx).as_snbt_macros(ctx),
            ),
            Self::Predicate(location, value) => LowItemTest::Predicate(
                location,
                value.kind.resolve(datapack, ctx).as_snbt_macros(ctx),
            ),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub struct OrGroup(pub Vec<(bool, ItemTest)>);

impl OrGroup {
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

    pub fn compile(self, datapack: &mut Datapack, ctx: &mut CompileContext) -> LowOrGroup {
        LowOrGroup(
            self.0
                .into_iter()
                .map(|(negated, test)| (negated, test.compile(datapack, ctx)))
                .collect(),
        )
    }
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub struct ItemPredicate {
    pub id: ItemType,
    pub or_groups: Vec<OrGroup>,
}

impl ItemPredicate {
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

    pub fn compile(self, datapack: &mut Datapack, ctx: &mut CompileContext) -> LowItemPredicate {
        LowItemPredicate {
            id: self.id,
            or_groups: self
                .or_groups
                .into_iter()
                .map(|or_group| or_group.compile(datapack, ctx))
                .collect(),
        }
    }
}
