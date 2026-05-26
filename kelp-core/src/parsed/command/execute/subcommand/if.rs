use minecraft_command_types::{
    column_position::ColumnPosition, command::enums::if_blocks_mode::IfBlocksMode,
    coordinate::Coordinates, resource_location::ResourceLocation,
};

use crate::{
    parsed::arena::ParsedAstArena,
    parsed::{
        block::BlockState, command::execute::subcommand::ParsedExecuteSubcommand, data::DataTarget,
        entity_selector::EntitySelector, item_source::ItemSource, mc_item::ItemPredicate,
        nbt_path::ParsedNbtPath, player_score::PlayerScore,
        score_comparison::ParsedScoreComparison, semantic_analysis::SemanticAnalysisContext,
        supports_expression_sigil::ParsedSupportsExpressionSigil,
    },
    typed::arena::TypedAstArena,
    typed::expression::command::execute::subcommand::r#if::TypedExecuteIfSubcommand,
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
        DataTarget,
        ParsedNbtPath,
        Option<Box<ParsedExecuteSubcommand>>,
    ),
    Dimension(
        ParsedSupportsExpressionSigil<ResourceLocation>,
        Option<Box<ParsedExecuteSubcommand>>,
    ),
    Entity(EntitySelector, Option<Box<ParsedExecuteSubcommand>>),
    Function(
        ParsedSupportsExpressionSigil<ResourceLocation>,
        Option<Box<ParsedExecuteSubcommand>>,
    ),
    Items(
        ItemSource,
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
        ParsedScoreComparison,
        Option<Box<ParsedExecuteSubcommand>>,
    ),
}

impl ParsedExecuteIfSubcommand {
    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        parsed_arena: &ParsedAstArena,
        typed_arena: &mut TypedAstArena,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<TypedExecuteIfSubcommand> {
        Some(match self {
            Self::Biome(coordinates, biome, next) => {
                let biome = biome.perform_semantic_analysis(parsed_arena, typed_arena, ctx);

                let next = match next {
                    Some(next) => {
                        Some(next.perform_semantic_analysis(parsed_arena, typed_arena, ctx)?)
                    }
                    None => None,
                };

                let biome = biome?;

                TypedExecuteIfSubcommand::Biome(coordinates, biome, next.map(Box::new))
            }
            Self::Block(coordinates, block_state, next) => {
                let block_state =
                    block_state.perform_semantic_analysis(parsed_arena, typed_arena, ctx);
                let next = match next {
                    Some(next) => {
                        Some(next.perform_semantic_analysis(parsed_arena, typed_arena, ctx)?)
                    }
                    None => None,
                };

                let block_state = block_state?;

                TypedExecuteIfSubcommand::Block(coordinates, block_state, next.map(Box::new))
            }
            Self::Blocks(start, end, desination, mode, next) => {
                let next = match next {
                    Some(next) => {
                        Some(next.perform_semantic_analysis(parsed_arena, typed_arena, ctx)?)
                    }
                    None => None,
                };

                TypedExecuteIfSubcommand::Blocks(start, end, desination, mode, next.map(Box::new))
            }
            Self::Data(target, path, next) => {
                let target = target.perform_semantic_analysis(parsed_arena, typed_arena, ctx);
                let path = path.perform_semantic_analysis(parsed_arena, typed_arena, ctx);
                let next = match next {
                    Some(next) => {
                        Some(next.perform_semantic_analysis(parsed_arena, typed_arena, ctx)?)
                    }
                    None => None,
                };

                let target = target?;
                let path = path?;

                TypedExecuteIfSubcommand::Data(target, path, next.map(Box::new))
            }
            Self::Dimension(dimension, next) => {
                let dimension = dimension.perform_semantic_analysis(parsed_arena, typed_arena, ctx);

                let next = match next {
                    Some(next) => {
                        Some(next.perform_semantic_analysis(parsed_arena, typed_arena, ctx)?)
                    }
                    None => None,
                };

                let dimension = dimension?;

                TypedExecuteIfSubcommand::Dimension(dimension, next.map(Box::new))
            }
            Self::Entity(selector, next) => {
                let selector = selector.perform_semantic_analysis(parsed_arena, typed_arena, ctx);
                let next = match next {
                    Some(next) => {
                        Some(next.perform_semantic_analysis(parsed_arena, typed_arena, ctx)?)
                    }
                    None => None,
                };

                let selector = selector?;

                TypedExecuteIfSubcommand::Entity(selector, next.map(Box::new))
            }
            Self::Function(function, next) => {
                let function = function.perform_semantic_analysis(parsed_arena, typed_arena, ctx);

                let next = match next {
                    Some(next) => {
                        Some(next.perform_semantic_analysis(parsed_arena, typed_arena, ctx)?)
                    }
                    None => None,
                };

                let function = function?;

                TypedExecuteIfSubcommand::Function(function, next.map(Box::new))
            }
            Self::Items(item_source, slot, item_predicate, next) => {
                let item_source =
                    item_source.perform_semantic_analysis(parsed_arena, typed_arena, ctx);
                let item_predicate =
                    item_predicate.perform_semantic_analysis(parsed_arena, typed_arena, ctx);
                let next = match next {
                    Some(next) => {
                        Some(next.perform_semantic_analysis(parsed_arena, typed_arena, ctx)?)
                    }
                    None => None,
                };

                let item_source = item_source?;
                let item_predicate = item_predicate?;

                TypedExecuteIfSubcommand::Items(
                    item_source,
                    slot,
                    item_predicate,
                    next.map(Box::new),
                )
            }
            Self::Loaded(column_position, next) => {
                let next = match next {
                    Some(next) => {
                        Some(next.perform_semantic_analysis(parsed_arena, typed_arena, ctx)?)
                    }
                    None => None,
                };

                TypedExecuteIfSubcommand::Loaded(column_position, next.map(Box::new))
            }
            Self::Predicate(predicate, next) => {
                let predicate = predicate.perform_semantic_analysis(parsed_arena, typed_arena, ctx);

                let next = match next {
                    Some(next) => {
                        Some(next.perform_semantic_analysis(parsed_arena, typed_arena, ctx)?)
                    }
                    None => None,
                };

                let predicate = predicate?;

                TypedExecuteIfSubcommand::Predicate(predicate, next.map(Box::new))
            }
            Self::Score(score, score_comparison, next) => {
                let score = score.perform_semantic_analysis(parsed_arena, typed_arena, ctx);
                let score_comparison =
                    score_comparison.perform_semantic_analysis(parsed_arena, typed_arena, ctx);
                let next = match next {
                    Some(next) => {
                        Some(next.perform_semantic_analysis(parsed_arena, typed_arena, ctx)?)
                    }
                    None => None,
                };

                let score = score?;
                let score_comparison = score_comparison?;

                TypedExecuteIfSubcommand::Score(score, score_comparison, next.map(Box::new))
            }
        })
    }
}
