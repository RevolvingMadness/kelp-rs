use crate::{compile_context::CompileContext, datapack::Datapack, semantic::item::Item};

#[derive(Debug, Clone)]
pub struct Program {
    pub items: Vec<Item>,
}

impl Program {
    pub fn compile(self, datapack: &mut Datapack, ctx: &mut CompileContext) {
        for item in self.items {
            item.compile(datapack, ctx);
        }
    }
}
