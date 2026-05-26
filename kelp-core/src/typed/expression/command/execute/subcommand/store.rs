use minecraft_command_types::{
    command::{
        enums::{bossbar_store_type::BossbarStoreType, numeric_snbt_type::NumericSNBTType},
        execute::ExecuteStoreSubcommand,
    },
    resource_location::ResourceLocation,
};
use ordered_float::NotNan;

use crate::{
    compile_context::CompileContext,
    datapack::Datapack,
    typed::arena::TypedAstArena,
    typed::{
        data::TypedDataTarget, expression::command::execute::subcommand::TypedExecuteSubcommand,
        nbt_path::TypedNbtPath, player_score::TypedPlayerScore,
    },
};

#[derive(Debug, Clone)]
pub enum TypedExecuteStoreSubcommand {
    Data(
        TypedDataTarget,
        TypedNbtPath,
        NumericSNBTType,
        NotNan<f32>,
        Box<TypedExecuteSubcommand>,
    ),
    Bossbar(
        ResourceLocation,
        BossbarStoreType,
        Box<TypedExecuteSubcommand>,
    ),
    Score(TypedPlayerScore, Box<TypedExecuteSubcommand>),
}

impl TypedExecuteStoreSubcommand {
    #[must_use]
    pub fn then(self, next: TypedExecuteSubcommand) -> Self {
        match self {
            Self::Data(target, path, numeric_snbt_type, scale, high_execute_subcommand) => {
                Self::Data(
                    target,
                    path,
                    numeric_snbt_type,
                    scale,
                    Box::new(high_execute_subcommand.then(next)),
                )
            }
            Self::Bossbar(resource_location, bossbar_store_type, high_execute_subcommand) => {
                Self::Bossbar(
                    resource_location,
                    bossbar_store_type,
                    Box::new(high_execute_subcommand.then(next)),
                )
            }
            Self::Score(player_score, high_execute_subcommand) => {
                Self::Score(player_score, Box::new(high_execute_subcommand.then(next)))
            }
        }
    }

    pub fn compile(
        self,
        arena: &TypedAstArena,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> ExecuteStoreSubcommand {
        match self {
            Self::Data(target, path, numeric_snbt_type, scale, next) => {
                let target = target.compile(arena, datapack, ctx);
                let path = path.compile(arena, datapack, ctx);
                let next = next.compile(arena, datapack, ctx);

                ExecuteStoreSubcommand::Data(
                    target.target,
                    path,
                    numeric_snbt_type,
                    scale,
                    Box::new(next),
                )
            }
            Self::Bossbar(location, store_type, next) => ExecuteStoreSubcommand::Bossbar(
                location,
                store_type,
                Box::new(next.compile(arena, datapack, ctx)),
            ),
            Self::Score(score, next) => {
                let score = score.compile(arena, datapack, ctx);
                let next = next.compile(arena, datapack, ctx);

                ExecuteStoreSubcommand::Score(score.score, Box::new(next))
            }
        }
    }
}
