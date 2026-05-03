use std::fmt::Display;

use minecraft_command_types::resource_location::ResourceLocation;

use crate::{
    high::{
        coordinate::Coordinates, entity_selector::EntitySelector,
        semantic_analysis::SemanticAnalysisContext,
        supports_expression_sigil::SupportsExpressionSigil,
    },
    low::data::{DataTarget as MiddleDataTarget, DataTargetKind as MiddleDataTargetKind},
    span::Span,
};

#[derive(Debug, Clone)]
pub enum DataTargetKind {
    Block(Box<SupportsExpressionSigil<Coordinates>>),
    Entity(SupportsExpressionSigil<EntitySelector>),
    Storage(SupportsExpressionSigil<ResourceLocation>),
}

impl DataTargetKind {
    #[must_use]
    pub const fn with_regular_span(self, span: Span) -> DataTarget {
        DataTarget {
            is_generated: false,
            span,
            kind: self,
        }
    }

    #[must_use]
    pub const fn with_generated_span(self) -> DataTarget {
        DataTarget {
            is_generated: true,
            span: Span::dummy(),
            kind: self,
        }
    }
}

#[derive(Debug, Clone)]
pub struct DataTarget {
    pub is_generated: bool,
    pub span: Span,
    pub kind: DataTargetKind,
}

impl Display for DataTarget {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            DataTargetKind::Block(coordinates) => write!(f, "block {}", coordinates),
            DataTargetKind::Entity(selector) => write!(f, "entity {}", selector),
            DataTargetKind::Storage(resource_location) => {
                write!(f, "storage {}", resource_location)
            }
        }
    }
}

impl DataTarget {
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<MiddleDataTarget> {
        Some(MiddleDataTarget {
            is_generated: self.is_generated,
            kind: match self.kind {
                DataTargetKind::Block(coordinates) => {
                    let coordinates = coordinates.perform_semantic_analysis(ctx)?;

                    MiddleDataTargetKind::Block(coordinates.map(Box::new))
                }
                DataTargetKind::Entity(selector) => {
                    let selector = selector.perform_semantic_analysis(ctx)?;

                    MiddleDataTargetKind::Entity(selector)
                }
                DataTargetKind::Storage(resource_location) => {
                    let resource_location = resource_location.perform_semantic_analysis(ctx)?;

                    MiddleDataTargetKind::Storage(resource_location)
                }
            },
            span: self.span,
        })
    }
}
