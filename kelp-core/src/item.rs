use minecraft_command_types::{
    item::{ItemPredicate, ItemTest, ItemType, OrGroup},
    resource_location::ResourceLocation,
};
use minecraft_command_types_derive::HasMacro;

use crate::{command::context::CompileContext, datapack::HighDatapack, expression::Expression};

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum HighItemTest {
    Component(ResourceLocation),
    ComponentMatches(ResourceLocation, Expression),
    Predicate(ResourceLocation, Expression),
}

impl HighItemTest {
    pub fn compile(self, datapack: &mut HighDatapack, ctx: &mut CompileContext) -> ItemTest {
        match self {
            HighItemTest::Component(location) => ItemTest::Component(location),
            HighItemTest::ComponentMatches(location, value) => ItemTest::ComponentMatches(
                location,
                value.resolve(datapack, ctx).kind.as_snbt_macros(ctx),
            ),
            HighItemTest::Predicate(location, value) => ItemTest::Predicate(
                location,
                value.resolve(datapack, ctx).kind.as_snbt_macros(ctx),
            ),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub struct HighOrGroup(pub Vec<(bool, HighItemTest)>);

impl HighOrGroup {
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
    pub tests: Vec<HighOrGroup>,
}

impl HighItemPredicate {
    pub fn compile(self, datapack: &mut HighDatapack, ctx: &mut CompileContext) -> ItemPredicate {
        ItemPredicate {
            id: self.id,
            tests: self
                .tests
                .into_iter()
                .map(|or_group| or_group.compile(datapack, ctx))
                .collect(),
        }
    }
}
