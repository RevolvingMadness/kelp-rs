use minecraft_command_types::{
    command::{
        enums::{bossbar_store_type::BossbarStoreType, numeric_snbt_type::NumericSNBTType},
        execute::ExecuteStoreSubcommand as LowExecuteStoreSubcommand,
    },
    resource_location::ResourceLocation,
};
use ordered_float::NotNan;

use crate::{
    ast_allocator::low::LowAstAllocator,
    compile_context::CompileContext,
    datapack::Datapack,
    low::{
        data::DataTarget, expression::command::execute::subcommand::ExecuteSubcommand,
        nbt_path::NbtPath, player_score::PlayerScore,
    },
};

#[derive(Debug, Clone)]
pub enum ExecuteStoreSubcommand {
    Data(
        DataTarget,
        NbtPath,
        NumericSNBTType,
        NotNan<f32>,
        Box<ExecuteSubcommand>,
    ),
    Bossbar(ResourceLocation, BossbarStoreType, Box<ExecuteSubcommand>),
    Score(PlayerScore, Box<ExecuteSubcommand>),
}

impl ExecuteStoreSubcommand {
    #[must_use]
    pub fn then(self, next: ExecuteSubcommand) -> Self {
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
        allocator: &LowAstAllocator,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> LowExecuteStoreSubcommand {
        match self {
            Self::Data(target, path, numeric_snbt_type, scale, next) => {
                let target = target.compile(allocator, datapack, ctx);
                let path = path.compile(allocator, datapack, ctx);
                let next = next.compile(allocator, datapack, ctx);

                LowExecuteStoreSubcommand::Data(
                    target.target,
                    path,
                    numeric_snbt_type,
                    scale,
                    Box::new(next),
                )
            }
            Self::Bossbar(location, store_type, next) => LowExecuteStoreSubcommand::Bossbar(
                location,
                store_type,
                Box::new(next.compile(allocator, datapack, ctx)),
            ),
            Self::Score(score, next) => {
                let score = score.compile(allocator, datapack, ctx);
                let next = next.compile(allocator, datapack, ctx);

                LowExecuteStoreSubcommand::Score(score.score, Box::new(next))
            }
        }
    }
}
