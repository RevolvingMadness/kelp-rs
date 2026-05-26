use la_arena::Idx;

use crate::{
    compile_context::CompileContext, datapack::Datapack, typed::arena::TypedAstArena,
    typed::item::TypedItem,
};

#[derive(Debug, Clone)]
pub struct Program {
    pub items: Vec<Idx<TypedItem>>,
}

impl Program {
    pub fn compile(self, arena: &TypedAstArena, datapack: &mut Datapack, ctx: &mut CompileContext) {
        for item in self.items {
            TypedItem::compile(item, arena, datapack, ctx);
        }
    }
}
