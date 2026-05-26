use minecraft_command_types::{
    command::{
        Command,
        data::{DataCommand, DataCommandModification, DataCommandModificationMode, DataTarget},
    },
    macroable::Macroable,
    nbt_path::{NbtPath, NbtPathNode, SNBTCompound},
    snbt::{SNBT, SNBTString},
};
use ordered_float::NotNan;

use crate::{
    compile_context::CompileContext, datapack::Datapack, low::text_component::TextComponent,
    player_score::GeneratedPlayerScore,
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GeneratedData {
    pub target: GeneratedDataTarget,
    pub path: NbtPath,
}

impl TextComponent for GeneratedData {
    fn into_text_component(self, _datapack: &mut Datapack, _ctx: &mut CompileContext) -> SNBT {
        let mut map = SNBTCompound::new();

        match self.target.target {
            DataTarget::Block(coordinates) => {
                map.insert(
                    SNBTString(false, "block".to_string()),
                    Macroable::Regular(SNBT::string(coordinates)),
                );
            }
            DataTarget::Entity(entity_selector) => {
                map.insert(
                    SNBTString(false, "entity".to_string()),
                    Macroable::Regular(SNBT::string(entity_selector)),
                );
            }
            DataTarget::Storage(resource_location) => {
                map.insert(
                    SNBTString(false, "storage".to_string()),
                    Macroable::Regular(SNBT::string(resource_location)),
                );
            }
        }

        map.insert(
            SNBTString(false, "nbt".to_string()),
            Macroable::Regular(self.path.to_snbt_string()),
        );

        SNBT::compound(map)
    }
}

impl GeneratedData {
    pub fn assign_to_score(
        self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
        target: GeneratedPlayerScore,
    ) {
        ctx.add_command(datapack, self.get().run().store_result_score(target.score));
    }

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
    pub fn get_regular(self, scale: Option<NotNan<f32>>) -> Command {
        Command::Data(DataCommand::Get(self.target.target, Some(self.path), scale))
    }

    #[inline]
    #[must_use]
    pub fn get(self) -> Command {
        self.get_regular(None)
    }

    #[inline]
    #[must_use]
    pub fn get_scaled(self, scale: NotNan<f32>) -> Command {
        self.get_regular(Some(scale))
    }

    #[inline]
    #[must_use]
    pub fn modify(
        self,
        mode: DataCommandModificationMode,
        modification: DataCommandModification,
    ) -> Command {
        Command::Data(DataCommand::Modify(
            self.target.target,
            self.path,
            mode,
            modification,
        ))
    }

    #[inline]
    #[must_use]
    pub fn append(self, modification: DataCommandModification) -> Command {
        self.modify(DataCommandModificationMode::Append, modification)
    }

    #[inline]
    #[must_use]
    pub fn append_from(self, data: Self) -> Command {
        self.append(DataCommandModification::From(
            data.target.target,
            Some(data.path),
        ))
    }

    #[inline]
    #[must_use]
    pub fn append_value<V: Into<Macroable<SNBT>>>(self, value: V) -> Command {
        self.append(DataCommandModification::Value(value.into()))
    }

    #[inline]
    #[must_use]
    pub fn set(self, modification: DataCommandModification) -> Command {
        self.modify(DataCommandModificationMode::Set, modification)
    }

    #[inline]
    #[must_use]
    pub fn set_value<V: Into<Macroable<SNBT>>>(self, value: V) -> Command {
        self.set(DataCommandModification::Value(value.into()))
    }

    #[inline]
    #[must_use]
    pub fn set_from(self, data: Self) -> Command {
        self.set(DataCommandModification::From(
            data.target.target,
            Some(data.path),
        ))
    }

    #[inline]
    #[must_use]
    pub fn remove(self) -> Command {
        Command::Data(DataCommand::Remove(self.target.target, self.path))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GeneratedDataTarget {
    pub is_generated: bool,
    pub target: DataTarget,
}
