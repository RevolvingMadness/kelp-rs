use minecraft_command_types::{
    command::enums::{bossbar_store_type::BossbarStoreType, numeric_snbt_type::NumericSNBTType},
    resource_location::ResourceLocation,
};
use ordered_float::NotNan;

use crate::{
    ast_allocator::{high::HighAstAllocator, low::LowAstAllocator},
    parsed::{
        command::execute::subcommand::ParsedExecuteSubcommand, data::DataTarget,
        nbt_path::ParsedNbtPath, player_score::PlayerScore,
        semantic_analysis::SemanticAnalysisContext,
    },
    typed::expression::command::execute::subcommand::store::TypedExecuteStoreSubcommand as MiddleExecuteStoreSubcommand,
};

#[derive(Debug, Clone)]
pub struct ParsedExecuteStoreDataSubcommand {
    pub target: DataTarget,
    pub path: ParsedNbtPath,
    pub snbt_type: NumericSNBTType,
    pub scale: NotNan<f32>,
    pub next: Box<ParsedExecuteSubcommand>,
}

#[derive(Debug, Clone)]
pub enum ParsedExecuteStoreSubcommand {
    Data(Box<ParsedExecuteStoreDataSubcommand>),
    Bossbar(
        ResourceLocation,
        BossbarStoreType,
        Box<ParsedExecuteSubcommand>,
    ),
    Score(PlayerScore, Box<ParsedExecuteSubcommand>),
}

impl ParsedExecuteStoreSubcommand {
    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        high_allocator: &HighAstAllocator,
        low_allocator: &mut LowAstAllocator,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<MiddleExecuteStoreSubcommand> {
        Some(match self {
            Self::Data(subcommand) => {
                let ParsedExecuteStoreDataSubcommand {
                    target,
                    path,
                    snbt_type,
                    scale,
                    next,
                } = *subcommand;

                let target = target.perform_semantic_analysis(high_allocator, low_allocator, ctx);
                let path = path.perform_semantic_analysis(high_allocator, low_allocator, ctx);
                let next = next.perform_semantic_analysis(high_allocator, low_allocator, ctx);

                let target = target?;
                let path = path?;
                let next = next?;

                MiddleExecuteStoreSubcommand::Data(target, path, snbt_type, scale, Box::new(next))
            }
            Self::Bossbar(resource_location, store_type, next) => {
                let next = next.perform_semantic_analysis(high_allocator, low_allocator, ctx)?;

                MiddleExecuteStoreSubcommand::Bossbar(resource_location, store_type, Box::new(next))
            }
            Self::Score(score, next) => {
                let score = score.perform_semantic_analysis(high_allocator, low_allocator, ctx);
                let next = next.perform_semantic_analysis(high_allocator, low_allocator, ctx);

                let score = score?;
                let next = next?;

                MiddleExecuteStoreSubcommand::Score(score, Box::new(next))
            }
        })
    }
}
