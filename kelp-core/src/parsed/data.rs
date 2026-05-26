use std::fmt::Display;

use minecraft_command_types::resource_location::ResourceLocation;

use crate::{
    parsed::{
        coordinate::ParsedCoordinates, entity_selector::ParsedEntitySelector, nbt_path::NbtPath,
        semantic_analysis::SemanticAnalysisContext,
        supports_expression_sigil::ParsedSupportsExpressionSigil,
    },
    semantic::data::{
        SemanticData as MiddleData, SemanticDataTarget as MiddleDataTarget,
        SemanticDataTargetKind as MiddleDataTargetKind,
    },
    span::Span,
};

#[derive(Debug, Clone)]
pub struct Data {
    pub target: ParsedDataTarget,
    pub path: NbtPath,
}

impl Display for Data {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} ...", self.target)
    }
}

impl Data {
    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<MiddleData> {
        let target = self.target.perform_semantic_analysis(ctx);
        let path = self.path.perform_semantic_analysis(ctx);

        let target = target?;
        let path = path?;

        Some(MiddleData { target, path })
    }
}

#[derive(Debug, Clone)]
pub enum ParsedDataTargetKind {
    Block(Box<ParsedSupportsExpressionSigil<ParsedCoordinates>>),
    Entity(ParsedSupportsExpressionSigil<ParsedEntitySelector>),
    Storage(ParsedSupportsExpressionSigil<ResourceLocation>),
}

impl ParsedDataTargetKind {
    #[must_use]
    pub const fn with_regular_span(self, span: Span) -> ParsedDataTarget {
        ParsedDataTarget {
            is_generated: false,
            span,
            kind: self,
        }
    }

    #[must_use]
    pub const fn with_generated_span(self) -> ParsedDataTarget {
        ParsedDataTarget {
            is_generated: true,
            span: Span::dummy(),
            kind: self,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ParsedDataTarget {
    pub is_generated: bool,
    pub span: Span,
    pub kind: ParsedDataTargetKind,
}

impl Display for ParsedDataTarget {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            ParsedDataTargetKind::Block(coordinates) => write!(f, "block {}", coordinates),
            ParsedDataTargetKind::Entity(selector) => write!(f, "entity {}", selector),
            ParsedDataTargetKind::Storage(resource_location) => {
                write!(f, "storage {}", resource_location)
            }
        }
    }
}

impl ParsedDataTarget {
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<MiddleDataTarget> {
        Some(MiddleDataTarget {
            is_generated: self.is_generated,
            kind: match self.kind {
                ParsedDataTargetKind::Block(coordinates) => {
                    let coordinates = coordinates.perform_semantic_analysis(ctx)?;

                    MiddleDataTargetKind::Block(coordinates.map(Box::new))
                }
                ParsedDataTargetKind::Entity(selector) => {
                    let selector = selector.perform_semantic_analysis(ctx)?;

                    MiddleDataTargetKind::Entity(selector)
                }
                ParsedDataTargetKind::Storage(resource_location) => {
                    let resource_location = resource_location.perform_semantic_analysis(ctx)?;

                    MiddleDataTargetKind::Storage(resource_location)
                }
            },
            span: self.span,
        })
    }
}
