use minecraft_command_types::{
    item::{ItemPredicate, ItemTest, ItemType, OrGroup},
    resource_location::ResourceLocation,
};

use crate::{
    ast_allocator::low::LowAstAllocator,
    compile_context::CompileContext,
    datapack::Datapack,
    typed::expression::{TypedExpression, TypedExpressionId},
};

#[derive(Debug, Clone)]
pub enum TypedItemTest {
    Component(ResourceLocation),
    ComponentMatches(ResourceLocation, TypedExpressionId),
    Predicate(ResourceLocation, TypedExpressionId),
}

impl TypedItemTest {
    #[must_use]
    pub fn compile(
        self,
        allocator: &LowAstAllocator,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> ItemTest {
        match self {
            Self::Component(resource_location) => ItemTest::Component(resource_location),
            Self::ComponentMatches(resource_location, expression) => {
                let expression = TypedExpression::resolve(expression, allocator, datapack, ctx)
                    .as_snbt_macros(ctx);

                ItemTest::ComponentMatches(resource_location, expression)
            }
            Self::Predicate(resource_location, expression) => {
                let expression = TypedExpression::resolve(expression, allocator, datapack, ctx)
                    .as_snbt_macros(ctx);

                ItemTest::Predicate(resource_location, expression)
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypedOrGroup(pub Vec<(bool, TypedItemTest)>);

impl TypedOrGroup {
    #[must_use]
    pub fn compile(
        self,
        allocator: &LowAstAllocator,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> OrGroup {
        OrGroup(
            self.0
                .into_iter()
                .map(|(negated, test)| (negated, test.compile(allocator, datapack, ctx)))
                .collect(),
        )
    }
}

#[derive(Debug, Clone)]
pub struct TypedItemPredicate {
    pub id: ItemType,
    pub or_groups: Vec<TypedOrGroup>,
}

impl TypedItemPredicate {
    #[must_use]
    pub fn compile(
        self,
        allocator: &LowAstAllocator,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> ItemPredicate {
        ItemPredicate {
            id: self.id,
            or_groups: self
                .or_groups
                .into_iter()
                .map(|or_group| or_group.compile(allocator, datapack, ctx))
                .collect(),
        }
    }
}
