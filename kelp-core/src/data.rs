use crate::{compile_context::CompileContext, datapack::Datapack};
use minecraft_command_types::{
    command::{
        Command,
        data::{DataCommand, DataCommandModification, DataCommandModificationMode, DataTarget},
    },
    macroable::Macroable,
    nbt_path::{NbtPath, NbtPathNode},
    snbt::SNBT,
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
    #[must_use]
    pub fn index(self, index: i32) -> Self {
        self.with_path_node(NbtPathNode::Index(Some(SNBT::macroable_integer(index))))
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
    pub fn append(
        self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
        modification: DataCommandModification,
    ) {
        self.modify(
            datapack,
            ctx,
            DataCommandModificationMode::Append,
            modification,
        );
    }

    #[inline]
    pub fn append_from(
        self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
        data: GeneratedData,
    ) {
        self.modify(
            datapack,
            ctx,
            DataCommandModificationMode::Append,
            DataCommandModification::From(data.target.target, Some(data.path)),
        );
    }

    #[inline]
    pub fn append_value(
        self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
        value: Macroable<SNBT>,
    ) {
        self.modify(
            datapack,
            ctx,
            DataCommandModificationMode::Append,
            DataCommandModification::Value(value),
        );
    }

    #[inline]
    pub fn set_from(self, datapack: &mut Datapack, ctx: &mut CompileContext, data: GeneratedData) {
        self.modify(
            datapack,
            ctx,
            DataCommandModificationMode::Set,
            DataCommandModification::From(data.target.target, Some(data.path)),
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
