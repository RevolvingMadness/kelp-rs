use la_arena::Idx;

use crate::{
    ast_allocator::low::LowAstAllocator, compile_context::CompileContext, datapack::Datapack,
    typed::item::Item,
};

#[derive(Debug, Clone)]
pub struct Program {
    pub items: Vec<Idx<Item>>,
}

impl Program {
    pub fn compile(
        self,
        allocator: &LowAstAllocator,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) {
        for item in self.items {
            Item::compile(item, allocator, datapack, ctx);
        }
    }
}
