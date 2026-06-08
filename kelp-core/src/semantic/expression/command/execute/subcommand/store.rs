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
    semantic::{
        data::SemanticDataTarget,
        expression::command::execute::subcommand::SemanticExecuteSubcommand,
        nbt_path::SemanticNbtPath, player_score::SemanticPlayerScore,
    },
};

#[derive(Debug, Clone)]
pub enum SemanticExecuteStoreSubcommand {
    Data(
        SemanticDataTarget,
        SemanticNbtPath,
        NumericSNBTType,
        NotNan<f32>,
        Box<SemanticExecuteSubcommand>,
    ),
    Bossbar(
        ResourceLocation,
        BossbarStoreType,
        Box<SemanticExecuteSubcommand>,
    ),
    Score(SemanticPlayerScore, Box<SemanticExecuteSubcommand>),
}

impl SemanticExecuteStoreSubcommand {
    #[must_use]
    pub fn then(self, next: SemanticExecuteSubcommand) -> Self {
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
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> ExecuteStoreSubcommand {
        match self {
            Self::Data(target, path, numeric_snbt_type, scale, next) => {
                let target = target.compile(datapack, ctx);
                let path = path.compile(datapack, ctx);
                let next = next.compile(datapack, ctx);

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
                Box::new(next.compile(datapack, ctx)),
            ),
            Self::Score(score, next) => {
                let score = score.compile(datapack, ctx);
                let next = next.compile(datapack, ctx);

                ExecuteStoreSubcommand::Score(score.score, Box::new(next))
            }
        }
    }
}
