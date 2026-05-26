use la_arena::Idx;

use crate::{
    compile_context::CompileContext, datapack::Datapack, typed::arena::TypedAstArena,
    typed::item::TypedItem,
};

#[derive(Debug, Clone)]
pub struct TypedProgram {
    pub items: Vec<Idx<TypedItem>>,
}

impl TypedProgram {
    pub fn compile(self, arena: &TypedAstArena, datapack: &mut Datapack, ctx: &mut CompileContext) {
        for item in self.items {
            TypedItem::compile(item, arena, datapack, ctx);
        }
    }
}
