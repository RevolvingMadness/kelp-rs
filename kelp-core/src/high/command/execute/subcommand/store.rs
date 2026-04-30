use minecraft_command_types::{
    command::enums::{bossbar_store_type::BossbarStoreType, numeric_snbt_type::NumericSNBTType},
    resource_location::ResourceLocation,
};
use ordered_float::NotNan;

use crate::{
    high::{
        command::execute::subcommand::ExecuteSubcommand, data::DataTarget, nbt_path::NbtPath,
        player_score::PlayerScore, semantic_analysis::SemanticAnalysisContext,
    },
    low::expression::command::execute::subcommand::store::ExecuteStoreSubcommand as MiddleExecuteStoreSubcommand,
};

#[derive(Debug, Clone)]
pub struct ExecuteStoreDataSubcommand {
    pub target: DataTarget,
    pub path: NbtPath,
    pub snbt_type: NumericSNBTType,
    pub scale: NotNan<f32>,
    pub next: Box<ExecuteSubcommand>,
}

#[derive(Debug, Clone)]
pub enum ExecuteStoreSubcommand {
    Data(Box<ExecuteStoreDataSubcommand>),
    Bossbar(ResourceLocation, BossbarStoreType, Box<ExecuteSubcommand>),
    Score(PlayerScore, Box<ExecuteSubcommand>),
}

impl ExecuteStoreSubcommand {
    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<MiddleExecuteStoreSubcommand> {
        Some(match self {
            Self::Data(subcommand) => {
                let ExecuteStoreDataSubcommand {
                    target,
                    path,
                    snbt_type,
                    scale,
                    next,
                } = *subcommand;

                let target = target.perform_semantic_analysis(ctx);
                let path = path.perform_semantic_analysis(ctx);
                let next = next.perform_semantic_analysis(ctx);

                let target = target?;
                let path = path?;
                let next = next?;

                MiddleExecuteStoreSubcommand::Data(target, path, snbt_type, scale, Box::new(next))
            }
            Self::Bossbar(resource_location, store_type, next) => {
                let next = next.perform_semantic_analysis(ctx)?;

                MiddleExecuteStoreSubcommand::Bossbar(resource_location, store_type, Box::new(next))
            }
            Self::Score(score, next) => {
                let score = score.perform_semantic_analysis(ctx);
                let next = next.perform_semantic_analysis(ctx);

                let score = score?;
                let next = next?;

                MiddleExecuteStoreSubcommand::Score(score, Box::new(next))
            }
        })
    }
}
