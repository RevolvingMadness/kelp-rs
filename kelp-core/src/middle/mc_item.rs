use minecraft_command_types::{
    item::{
        ItemPredicate as LowItemPredicate, ItemTest as LowItemTest, ItemType, OrGroup as LowOrGroup,
    },
    resource_location::ResourceLocation,
};
use minecraft_command_types_derive::HasMacro;

use crate::{compile_context::CompileContext, datapack::Datapack, middle::expression::Expression};

#[derive(Debug, Clone, HasMacro)]
pub enum ItemTest {
    Component(ResourceLocation),
    ComponentMatches(ResourceLocation, Expression),
    Predicate(ResourceLocation, Expression),
}

impl ItemTest {
    #[must_use]
    pub fn compile(self, datapack: &mut Datapack, ctx: &mut CompileContext) -> LowItemTest {
        match self {
            Self::Component(resource_location) => LowItemTest::Component(resource_location),
            Self::ComponentMatches(resource_location, expression) => {
                let expression = expression.kind.resolve(datapack, ctx).as_snbt_macros(ctx);

                LowItemTest::ComponentMatches(resource_location, expression)
            }
            Self::Predicate(resource_location, expression) => {
                let expression = expression.kind.resolve(datapack, ctx).as_snbt_macros(ctx);

                LowItemTest::Predicate(resource_location, expression)
            }
        }
    }
}

#[derive(Debug, Clone, HasMacro)]
pub struct OrGroup(pub Vec<(bool, ItemTest)>);

impl OrGroup {
    #[must_use]
    pub fn compile(self, datapack: &mut Datapack, ctx: &mut CompileContext) -> LowOrGroup {
        LowOrGroup(
            self.0
                .into_iter()
                .map(|(negated, test)| (negated, test.compile(datapack, ctx)))
                .collect(),
        )
    }
}

#[derive(Debug, Clone, HasMacro)]
pub struct ItemPredicate {
    pub id: ItemType,
    pub or_groups: Vec<OrGroup>,
}

impl ItemPredicate {
    #[must_use]
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
