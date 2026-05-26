use minecraft_command_types::{
    command::{
        enums::{bossbar_store_type::BossbarStoreType, numeric_snbt_type::NumericSNBTType},
        execute::ExecuteStoreSubcommand,
    },
    resource_location::ResourceLocation,
};
use ordered_float::NotNan;

use crate::{
    ast_allocator::low::LowAstAllocator,
    compile_context::CompileContext,
    datapack::Datapack,
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
        allocator: &LowAstAllocator,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> ExecuteStoreSubcommand {
        match self {
            Self::Data(target, path, numeric_snbt_type, scale, next) => {
                let target = target.compile(allocator, datapack, ctx);
                let path = path.compile(allocator, datapack, ctx);
                let next = next.compile(allocator, datapack, ctx);

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
                Box::new(next.compile(allocator, datapack, ctx)),
            ),
            Self::Score(score, next) => {
                let score = score.compile(allocator, datapack, ctx);
                let next = next.compile(allocator, datapack, ctx);

                ExecuteStoreSubcommand::Score(score.score, Box::new(next))
            }
        }
    }
}
