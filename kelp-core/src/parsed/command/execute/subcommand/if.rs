use minecraft_command_types::{
    column_position::ColumnPosition, command::enums::if_blocks_mode::IfBlocksMode,
    coordinate::Coordinates, resource_location::ResourceLocation,
};

use crate::{
    parsed::{
        block::BlockState, command::execute::subcommand::ParsedExecuteSubcommand,
        data::ParsedDataTarget, entity_selector::ParsedEntitySelector,
        item_source::ParsedItemSource, mc_item::ItemPredicate, nbt_path::NbtPath,
        player_score::PlayerScore, score_comparison::ScoreComparison,
        semantic_analysis::SemanticAnalysisContext,
        supports_expression_sigil::ParsedSupportsExpressionSigil,
    },
    semantic::expression::command::execute::subcommand::r#if::SemanticExecuteIfSubcommand,
};

#[derive(Debug, Clone)]
pub enum ParsedExecuteIfSubcommand {
    Biome(
        Coordinates,
        ParsedSupportsExpressionSigil<ResourceLocation>,
        Option<Box<ParsedExecuteSubcommand>>,
    ),
    Block(
        Coordinates,
        BlockState,
        Option<Box<ParsedExecuteSubcommand>>,
    ),
    Blocks(
        Coordinates,
        Coordinates,
        Coordinates,
        IfBlocksMode,
        Option<Box<ParsedExecuteSubcommand>>,
    ),
    Data(
        ParsedDataTarget,
        NbtPath,
        Option<Box<ParsedExecuteSubcommand>>,
    ),
    Dimension(
        ParsedSupportsExpressionSigil<ResourceLocation>,
        Option<Box<ParsedExecuteSubcommand>>,
    ),
    Entity(ParsedEntitySelector, Option<Box<ParsedExecuteSubcommand>>),
    Function(
        ParsedSupportsExpressionSigil<ResourceLocation>,
        Option<Box<ParsedExecuteSubcommand>>,
    ),
    Items(
        ParsedItemSource,
        String,
        ItemPredicate,
        Option<Box<ParsedExecuteSubcommand>>,
    ),
    Loaded(ColumnPosition, Option<Box<ParsedExecuteSubcommand>>),
    Predicate(
        ParsedSupportsExpressionSigil<ResourceLocation>,
        Option<Box<ParsedExecuteSubcommand>>,
    ),
    Score(
        PlayerScore,
        ScoreComparison,
        Option<Box<ParsedExecuteSubcommand>>,
    ),
}

impl ParsedExecuteIfSubcommand {
    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<SemanticExecuteIfSubcommand> {
        Some(match self {
            Self::Biome(coordinates, biome, next) => {
                let biome = biome.perform_semantic_analysis(ctx);

                let next = match next {
                    Some(next) => Some(next.perform_semantic_analysis(ctx)?),
                    None => None,
                };

                let biome = biome?;

                SemanticExecuteIfSubcommand::Biome(coordinates, biome, next.map(Box::new))
            }
            Self::Block(coordinates, block_state, next) => {
                let block_state = block_state.perform_semantic_analysis(ctx);
                let next = match next {
                    Some(next) => Some(next.perform_semantic_analysis(ctx)?),
                    None => None,
                };

                let block_state = block_state?;

                SemanticExecuteIfSubcommand::Block(coordinates, block_state, next.map(Box::new))
            }
            Self::Blocks(start, end, desination, mode, next) => {
                let next = match next {
                    Some(next) => Some(next.perform_semantic_analysis(ctx)?),
                    None => None,
                };

                SemanticExecuteIfSubcommand::Blocks(
                    start,
                    end,
                    desination,
                    mode,
                    next.map(Box::new),
                )
            }
            Self::Data(target, path, next) => {
                let target = target.perform_semantic_analysis(ctx);
                let path = path.perform_semantic_analysis(ctx);
                let next = match next {
                    Some(next) => Some(next.perform_semantic_analysis(ctx)?),
                    None => None,
                };

                let target = target?;
                let path = path?;

                SemanticExecuteIfSubcommand::Data(target, path, next.map(Box::new))
            }
            Self::Dimension(dimension, next) => {
                let dimension = dimension.perform_semantic_analysis(ctx);

                let next = match next {
                    Some(next) => Some(next.perform_semantic_analysis(ctx)?),
                    None => None,
                };

                let dimension = dimension?;

                SemanticExecuteIfSubcommand::Dimension(dimension, next.map(Box::new))
            }
            Self::Entity(selector, next) => {
                let selector = selector.perform_semantic_analysis(ctx);
                let next = match next {
                    Some(next) => Some(next.perform_semantic_analysis(ctx)?),
                    None => None,
                };

                let selector = selector?;

                SemanticExecuteIfSubcommand::Entity(selector, next.map(Box::new))
            }
            Self::Function(function, next) => {
                let function = function.perform_semantic_analysis(ctx);

                let next = match next {
                    Some(next) => Some(next.perform_semantic_analysis(ctx)?),
                    None => None,
                };

                let function = function?;

                SemanticExecuteIfSubcommand::Function(function, next.map(Box::new))
            }
            Self::Items(item_source, slot, item_predicate, next) => {
                let item_source = item_source.perform_semantic_analysis(ctx);
                let item_predicate = item_predicate.perform_semantic_analysis(ctx);
                let next = match next {
                    Some(next) => Some(next.perform_semantic_analysis(ctx)?),
                    None => None,
                };

                let item_source = item_source?;
                let item_predicate = item_predicate?;

                SemanticExecuteIfSubcommand::Items(
                    item_source,
                    slot,
                    item_predicate,
                    next.map(Box::new),
                )
            }
            Self::Loaded(column_position, next) => {
                let next = match next {
                    Some(next) => Some(next.perform_semantic_analysis(ctx)?),
                    None => None,
                };

                SemanticExecuteIfSubcommand::Loaded(column_position, next.map(Box::new))
            }
            Self::Predicate(predicate, next) => {
                let predicate = predicate.perform_semantic_analysis(ctx);

                let next = match next {
                    Some(next) => Some(next.perform_semantic_analysis(ctx)?),
                    None => None,
                };

                let predicate = predicate?;

                SemanticExecuteIfSubcommand::Predicate(predicate, next.map(Box::new))
            }
            Self::Score(score, score_comparison, next) => {
                let score = score.perform_semantic_analysis(ctx);
                let score_comparison = score_comparison.perform_semantic_analysis(ctx);
                let next = match next {
                    Some(next) => Some(next.perform_semantic_analysis(ctx)?),
                    None => None,
                };

                let score = score?;
                let score_comparison = score_comparison?;

                SemanticExecuteIfSubcommand::Score(score, score_comparison, next.map(Box::new))
            }
        })
    }
}
