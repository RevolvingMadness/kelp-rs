use crate::{compile_context::CompileContext, datapack::Datapack};
use minecraft_command_types::{
    command::{
        Command,
        data::{DataCommand, DataCommandModification, DataCommandModificationMode, DataTarget},
    },
    nbt_path::{NbtPath, NbtPathNode},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GeneratedData {
    pub target: GeneratedDataTarget,
    pub path: NbtPath,
}

impl GeneratedData {
    #[must_use]
    pub fn with_path_node(self, node: NbtPathNode) -> Self {
        Self {
            path: self.path.with_node(node),
            ..self
        }
    }

    #[inline]
    pub fn modify(
        self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
        mode: DataCommandModificationMode,
        modification: DataCommandModification,
    ) {
        ctx.add_command(
            datapack,
            Command::Data(DataCommand::Modify(
                self.target.target,
                self.path,
                mode,
                modification,
            )),
        );
    }

    #[inline]
    pub fn remove(self, datapack: &mut Datapack, ctx: &mut CompileContext) {
        ctx.add_command(
            datapack,
            Command::Data(DataCommand::Remove(self.target.target, self.path)),
        );
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GeneratedDataTarget {
    pub is_generated: bool,
    pub target: DataTarget,
}
