use minecraft_command_types::{
    command::enums::{bossbar_store_type::BossbarStoreType, numeric_snbt_type::NumericSNBTType},
    resource_location::ResourceLocation,
};
use ordered_float::NotNan;

use crate::{
    high::{
        command::execute::subcommand::ExecuteSubcommand, data::DataTarget, nbt_path::NbtPath,
        player_score::PlayerScore, semantic_analysis_context::SemanticAnalysisContext,
    },
    middle::expression::command::execute::subcommand::store::ExecuteStoreSubcommand as MiddleExecuteStoreSubcommand,
};

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash)]
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
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<MiddleExecuteStoreSubcommand> {
        Some(match self {
            Self::Data(target, path, snbt_type, scale, next) => {
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
