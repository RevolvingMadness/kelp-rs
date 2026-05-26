use minecraft_command_types::{
    item::{ItemPredicate, ItemTest, ItemType, OrGroup},
    resource_location::ResourceLocation,
};

use crate::{
    compile_context::CompileContext, datapack::Datapack, semantic::expression::SemanticExpression,
};

#[derive(Debug, Clone)]
pub enum SemanticItemTest {
    Component(ResourceLocation),
    ComponentMatches(ResourceLocation, SemanticExpression),
    Predicate(ResourceLocation, SemanticExpression),
}

impl SemanticItemTest {
    #[must_use]
    pub fn compile(self, datapack: &mut Datapack, ctx: &mut CompileContext) -> ItemTest {
        match self {
            Self::Component(resource_location) => ItemTest::Component(resource_location),
            Self::ComponentMatches(resource_location, expression) => {
                let expression = expression.kind.resolve(datapack, ctx).as_snbt_macros(ctx);

                ItemTest::ComponentMatches(resource_location, expression)
            }
            Self::Predicate(resource_location, expression) => {
                let expression = expression.kind.resolve(datapack, ctx).as_snbt_macros(ctx);

                ItemTest::Predicate(resource_location, expression)
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct SemanticOrGroup(pub Vec<(bool, SemanticItemTest)>);

impl SemanticOrGroup {
    #[must_use]
    pub fn compile(self, datapack: &mut Datapack, ctx: &mut CompileContext) -> OrGroup {
        OrGroup(
            self.0
                .into_iter()
                .map(|(negated, test)| (negated, test.compile(datapack, ctx)))
                .collect(),
        )
    }
}

#[derive(Debug, Clone)]
pub struct SemanticItemPredicate {
    pub id: ItemType,
    pub or_groups: Vec<SemanticOrGroup>,
}

impl SemanticItemPredicate {
    #[must_use]
    pub fn compile(self, datapack: &mut Datapack, ctx: &mut CompileContext) -> ItemPredicate {
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
