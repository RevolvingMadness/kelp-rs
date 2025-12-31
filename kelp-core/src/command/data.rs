use minecraft_command_types::{
    command::{
        Command,
        data::{DataCommand, DataCommandModification, DataCommandModificationMode, DataTarget},
    },
    coordinate::Coordinates,
    resource_location::ResourceLocation,
};
use minecraft_command_types_derive::HasMacro;
use ordered_float::NotNan;

use crate::{
    command::context::CompileContext, datapack::HighDatapack, entity_selector::HighEntitySelector,
    expression::Expression, nbt_path::HighNbtPath,
};

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub struct HighDataTarget {
    pub is_generated: bool,
    pub kind: HighDataTargetKind,
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum HighDataTargetKind {
    Block(Coordinates),
    Entity(HighEntitySelector),
    Storage(ResourceLocation),
}

impl HighDataTargetKind {
    pub fn compile(self, datapack: &mut HighDatapack, ctx: &mut CompileContext) -> DataTarget {
        match self {
            HighDataTargetKind::Block(coordinates) => DataTarget::Block(coordinates),
            HighDataTargetKind::Entity(entity_selector) => {
                DataTarget::Entity(entity_selector.compile(datapack, ctx))
            }
            HighDataTargetKind::Storage(resource_location) => {
                DataTarget::Storage(resource_location)
            }
        }
    }

    #[inline]
    #[must_use]
    pub fn into_generated(self) -> HighDataTarget {
        HighDataTarget {
            is_generated: true,
            kind: self,
        }
    }

    #[inline]
    #[must_use]
    pub fn into_regular(self) -> HighDataTarget {
        HighDataTarget {
            is_generated: false,
            kind: self,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum HighDataCommandModification {
    From(HighDataTarget, Option<HighNbtPath>),
    String(
        HighDataTarget,
        Option<HighNbtPath>,
        Option<i32>,
        Option<i32>,
    ),
    Value(Box<Expression>),
}

impl HighDataCommandModification {
    pub fn compile(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
        target: &HighDataTarget,
        path: &HighNbtPath,
    ) -> Option<DataCommandModification> {
        match self {
            HighDataCommandModification::From(target, path) => {
                let target = target.kind.compile(datapack, ctx);
                let path = path.map(|path| path.compile(datapack, ctx));

                Some(DataCommandModification::From(target, path))
            }
            HighDataCommandModification::String(target, path, start, end) => {
                let target = target.kind.compile(datapack, ctx);
                let path = path.map(|path| path.compile(datapack, ctx));

                Some(DataCommandModification::String(target, path, start, end))
            }
            HighDataCommandModification::Value(expression) => {
                let target = target.kind.clone().compile(datapack, ctx);
                let path = path.clone().compile(datapack, ctx);

                let expression = expression.resolve(datapack, ctx);

                expression
                    .kind
                    .assign_to_data(datapack, ctx, target.clone(), path.clone());

                None
            }
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum HighDataCommand {
    Get(HighDataTarget, Option<HighNbtPath>, Option<NotNan<f32>>),
    Merge(HighDataTarget, Box<Expression>),
    Modify(
        HighDataTarget,
        HighNbtPath,
        DataCommandModificationMode,
        HighDataCommandModification,
    ),
    Remove(HighDataTarget, HighNbtPath),
}

impl HighDataCommand {
    pub fn compile(self, datapack: &mut HighDatapack, ctx: &mut CompileContext) -> Option<Command> {
        match self {
            HighDataCommand::Get(target, path, count) => {
                let compiled_target = target.kind.compile(datapack, ctx);
                let compiled_path = path.map(|path| path.compile(datapack, ctx));

                Some(Command::Data(DataCommand::Get(
                    compiled_target,
                    compiled_path,
                    count,
                )))
            }
            HighDataCommand::Merge(target, expression) => {
                let target = target.kind.compile(datapack, ctx);
                let expression = expression.resolve(datapack, ctx);

                let snbt = expression.kind.as_snbt_macros(ctx);

                Some(Command::Data(DataCommand::Merge(target, snbt)))
            }
            HighDataCommand::Modify(target, path, mode, modification) => {
                let compiled_modification = modification.compile(datapack, ctx, &target, &path)?;
                let compiled_target = target.kind.compile(datapack, ctx);
                let compiled_path = path.compile(datapack, ctx);

                Some(Command::Data(DataCommand::Modify(
                    compiled_target,
                    compiled_path,
                    mode,
                    compiled_modification,
                )))
            }
            HighDataCommand::Remove(target, path) => {
                let target = target.kind.compile(datapack, ctx);
                let path = path.compile(datapack, ctx);

                Some(Command::Data(DataCommand::Remove(target, path)))
            }
        }
    }
}
