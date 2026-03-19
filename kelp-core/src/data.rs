use crate::{compile_context::CompileContext, datapack::Datapack};
use minecraft_command_types::{
    command::{
        Command,
        data::{DataCommand, DataCommandModification, DataCommandModificationMode, DataTarget},
    },
    nbt_path::NbtPath,
};

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct GeneratedDataTarget {
    pub is_generated: bool,
    pub target: DataTarget,
}

impl GeneratedDataTarget {
    #[must_use]
    pub fn as_unique_data(
        self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
        path: NbtPath,
    ) -> (Self, NbtPath) {
        let (unique_data, unique_path) = datapack.get_unique_data();

        ctx.add_command(
            datapack,
            Command::Data(DataCommand::Modify(
                unique_data.target.clone(),
                unique_path.clone(),
                DataCommandModificationMode::Set,
                DataCommandModification::From(self.target, Some(path)),
            )),
        );

        (unique_data, unique_path)
    }
}
