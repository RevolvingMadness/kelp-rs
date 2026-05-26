use crate::{compile_context::CompileContext, datapack::Datapack, semantic::item::SemanticItem};

#[derive(Debug, Clone)]
pub struct Program {
    pub items: Vec<SemanticItem>,
}

impl Program {
    pub fn compile(self, datapack: &mut Datapack, ctx: &mut CompileContext) {
        for item in self.items {
            item.compile(datapack, ctx);
        }
    }
}
