use minecraft_command_types::{coordinate::Coordinates, resource_location::ResourceLocation};

use crate::{
    high::entity_selector::EntitySelector,
    middle::data::{DataTarget as MiddleDataTarget, DataTargetKind as MiddleDataTargetKind},
    semantic_analysis_context::SemanticAnalysisContext,
    span::Span,
};

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub enum DataTargetKind {
    Block(Coordinates),
    Entity(EntitySelector),
    Storage(ResourceLocation),
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

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct DataTarget {
    pub is_generated: bool,
    pub span: Span,
    pub kind: DataTargetKind,
}

impl DataTarget {
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<MiddleDataTarget> {
        Some(MiddleDataTarget {
            is_generated: self.is_generated,
            kind: match self.kind {
                DataTargetKind::Block(coordinates) => MiddleDataTargetKind::Block(coordinates),
                DataTargetKind::Entity(selector) => {
                    let selector = selector.perform_semantic_analysis(ctx)?;

                    MiddleDataTargetKind::Entity(selector)
                }
                DataTargetKind::Storage(resource_location) => {
                    MiddleDataTargetKind::Storage(resource_location)
                }
            },
            span: self.span,
        })
    }
}
