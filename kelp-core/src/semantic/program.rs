use crate::{compile_context::CompileContext, datapack::Datapack, semantic::item::SemanticItem};

#[derive(Debug, Clone)]
pub struct SemanticProgram {
    pub items: Vec<SemanticItem>,
}

impl SemanticProgram {
    pub fn compile(self, datapack: &mut Datapack, ctx: &mut CompileContext) {
        for item in self.items {
            item.compile(datapack, ctx);
        }
    }
}
