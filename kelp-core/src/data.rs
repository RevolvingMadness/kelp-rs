use minecraft_command_types::{
    command::data::{
        DataCommand, DataCommandModification, DataCommandModificationMode, DataTarget,
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
    #[must_use]
    pub fn modify(
        self,
        mode: DataCommandModificationMode,
        modification: DataCommandModification,
    ) -> DataCommand {
        DataCommand::Modify(self.target.target, self.path, mode, modification)
    }

    #[inline]
    #[must_use]
    pub fn append(self, modification: DataCommandModification) -> DataCommand {
        self.modify(DataCommandModificationMode::Append, modification)
    }

    #[inline]
    #[must_use]
    pub fn append_from(self, data: Self) -> DataCommand {
        self.append(DataCommandModification::From(
            data.target.target,
            Some(data.path),
        ))
    }

    #[inline]
    #[must_use]
    pub fn append_value<V: Into<Macroable<SNBT>>>(self, value: V) -> DataCommand {
        self.append(DataCommandModification::Value(value.into()))
    }

    #[inline]
    #[must_use]
    pub fn set(self, modification: DataCommandModification) -> DataCommand {
        self.modify(DataCommandModificationMode::Set, modification)
    }

    #[inline]
    #[must_use]
    pub fn set_value<V: Into<Macroable<SNBT>>>(self, value: V) -> DataCommand {
        self.set(DataCommandModification::Value(value.into()))
    }

    #[inline]
    #[must_use]
    pub fn set_from(self, data: Self) -> DataCommand {
        self.set(DataCommandModification::From(
            data.target.target,
            Some(data.path),
        ))
    }

    #[inline]
    #[must_use]
    pub fn remove(self) -> DataCommand {
        DataCommand::Remove(self.target.target, self.path)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GeneratedDataTarget {
    pub is_generated: bool,
    pub target: DataTarget,
}
