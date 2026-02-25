use minecraft_command_types::{
    command::{
        enums::{bossbar_store_type::BossbarStoreType, numeric_snbt_type::NumericSNBTType},
        execute::ExecuteStoreSubcommand,
    },
    resource_location::ResourceLocation,
};
use minecraft_command_types_derive::HasMacro;
use ordered_float::NotNan;

use crate::{
    compile_context::CompileContext,
    datapack::HighDatapack,
    high::{
        command::execute::subcommand::HighExecuteSubcommand, data::HighDataTarget,
        nbt_path::HighNbtPath, player_score::HighPlayerScore,
    },
    semantic_analysis_context::SemanticAnalysisContext,
};

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum HighExecuteStoreSubcommand {
    Data(
        HighDataTarget,
        HighNbtPath,
        NumericSNBTType,
        NotNan<f32>,
        Box<HighExecuteSubcommand>,
    ),
    Bossbar(
        ResourceLocation,
        BossbarStoreType,
        Box<HighExecuteSubcommand>,
    ),
    Score(HighPlayerScore, Box<HighExecuteSubcommand>),
}

impl HighExecuteStoreSubcommand {
    #[must_use]
    pub fn then(self, next: HighExecuteSubcommand) -> Self {
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

    pub fn perform_semantic_analysis(
        &self,
        ctx: &mut SemanticAnalysisContext,
        is_lhs: bool,
    ) -> Option<()> {
        match self {
            Self::Data(target, path, _, _, next) => {
                let target = target.kind.perform_semantic_analysis(ctx, is_lhs);
                let path = path.perform_semantic_analysis(ctx, is_lhs);
                let next = next.perform_semantic_analysis(ctx, is_lhs);

                target?;
                path?;
                next?;

                Some(())
            }
            Self::Bossbar(_, _, next) => next.perform_semantic_analysis(ctx, is_lhs),
            Self::Score(score, next) => {
                let score = score.perform_semantic_analysis(ctx, is_lhs);
                let next = next.perform_semantic_analysis(ctx, is_lhs);

                score?;
                next?;

                Some(())
            }
        }
    }

    pub fn compile(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
    ) -> Option<ExecuteStoreSubcommand> {
        match self {
            Self::Data(target, path, numeric_snbt_type, scale, next) => {
                next.compile(datapack, ctx).map(|next| {
                    let target = target.compile(datapack, ctx);
                    let path = path.compile(datapack, ctx);

                    ExecuteStoreSubcommand::Data(
                        target.target,
                        path,
                        numeric_snbt_type,
                        scale,
                        Box::new(next),
                    )
                })
            }
            Self::Bossbar(location, store_type, next) => next
                .compile(datapack, ctx)
                .map(|next| ExecuteStoreSubcommand::Bossbar(location, store_type, Box::new(next))),
            Self::Score(score, next) => {
                let score = score.compile(datapack, ctx);

                next.compile(datapack, ctx)
                    .map(|next| ExecuteStoreSubcommand::Score(score.score, Box::new(next)))
            }
        }
    }
}
